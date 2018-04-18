(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Transformer of struct/union into pointers arithmetics. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Stateless
open Framework.Manager
open Framework.Visitor
open Framework.Exceptions
open Framework.Domains
open Framework.Flow
open Framework.Eval
open Universal.Ast
open Ast


let name = "c.memory.var_init"
let debug fmt = Debug.debug ~channel:name fmt



(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain = struct

  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let rec init_array man ctx a init is_global range flow =
    debug "init array %a" Framework.Pp.pp_expr a;
    match init with
    | None when not is_global -> flow

    | None when is_global ->
      let rec aux i flow =
        if i = get_array_constant_length a.etyp then flow
        else
          let flow = init_expr man ctx (mk_c_subscript_access a (mk_int i range) range) None is_global range flow in
          aux (i + 1) flow
      in
      aux 0 flow

    | Some (C_init_list (l, filler)) ->
      let n = get_array_constant_length a.etyp in
      let rec aux i flow =
        if i = n then flow
        else
          let init = if i < List.length l then Some (List.nth l i) else filler in
          let flow = init_expr man ctx (mk_c_subscript_access a (mk_int i range) range) init is_global range flow in
          aux (i + 1) flow
      in
      aux 0 flow

    | Some (Ast.C_init_expr {ekind = E_constant(C_c_string (s, _))}) ->
      let n = get_array_constant_length a.etyp in
      let rec aux i flow =
        if i = n then flow
        else
          let init = if i < String.length s then Some (C_init_expr (mk_c_character (String.get s i) range)) else Some (C_init_expr (mk_c_character (char_of_int 0) range)) in
          let flow = init_expr man ctx (mk_c_subscript_access a (mk_int i range) range) init is_global range flow in
          aux (i + 1) flow
      in
      aux 0 flow

    | _ ->
      panic "Array initialization not supported"

  and init_union  man ctx u init is_global range flow =
    debug "init union %a" Framework.Pp.pp_expr u;
    let largest_field =
      let fields = match remove_typedef u.etyp |> remove_qual with
        | T_c_record{c_record_fields} -> c_record_fields
        | _ -> assert false
      in
      match fields with
      | [] -> assert false
      | [f] -> f
      | hd :: tl ->
        let rec doit acc = function
          | [] -> acc
          | f :: tl ->
            let acc = if Z.gt (sizeof_type f.c_field_type) (sizeof_type acc.c_field_type) then f else acc in
            doit acc tl
        in
        doit hd tl
    in

    match init with
    | None when not is_global -> flow

    | None when is_global ->
      debug "initialization of uninitialized global";
      debug "largest_field = %a" Framework.Pp.pp_expr (mk_c_member_access u largest_field range);
      init_expr man ctx (mk_c_member_access u largest_field range) None is_global range flow

    | _ -> panic "Initialization of union not supported"


  and init_scalar man ctx v init is_global range flow =
    debug "init scalar %a" Framework.Pp.pp_expr v;
    match init with
    | None when not is_global -> flow

    | None when is_global ->
      man.exec ctx (mk_assign v (mk_zero range) range) flow

    | Some (C_init_expr e) ->
      man.exec ctx (mk_assign v e range) flow

    | _ -> assert false

  and init_struct man ctx s init is_global range flow =
    debug "init struct %a" Framework.Pp.pp_expr s;
    let get_nth_field n =
        match remove_typedef s.etyp |> remove_qual with
        | T_c_record{c_record_kind = C_struct; c_record_fields} -> List.nth c_record_fields n
        | _ -> assert false
    in
    let nb_fields =
      match remove_typedef s.etyp |> remove_qual with
      | T_c_record{c_record_kind = C_struct; c_record_fields} -> List.length c_record_fields
      | _ -> assert false
    in
    match init with
    | None when not is_global -> flow

    | None when is_global ->
      let rec aux i flow =
        if i = nb_fields then flow
        else
          let flow = init_expr man ctx (mk_c_member_access s (get_nth_field i) range) None is_global range flow in
          aux (i + 1) flow
      in
      aux 0 flow

    | Some (C_init_list(l, None)) ->
      let rec aux i flow =
        if i = nb_fields then flow
        else
          let init = if i < List.length l then Some (List.nth l i) else None in
          let flow = init_expr man ctx (mk_c_member_access s (get_nth_field i) range) init is_global range flow in
          aux (i + 1) flow
      in
      aux 0 flow

    | Some (C_init_expr e) ->
      man.exec ctx (mk_assign s e range) flow

    | _ -> assert false

  and init_expr man ctx e (init: c_init option) is_global range flow =
    if is_c_scalar_type e.etyp then init_scalar man ctx e init is_global range flow else
    if is_c_array_type e.etyp then  init_array man ctx e init is_global range flow else
    if is_c_struct_type e.etyp then init_struct man ctx e init is_global range flow else
    if is_c_union_type e.etyp then init_union man ctx e init is_global range flow else
      panic "Unsupported initialization of %a" pp_expr e


  and init man ctx prog flow =
    let flow =
      match prog.prog_kind with
      | C_program(globals, _) ->
        (* Initialize string symbols as global variables *)
        let range = mk_fresh_range () in
        let table = Program.find_string_table ctx in
        let globals = Program.StringTable.fold (fun s v acc ->
            let init = C_init_expr (mk_c_string s range) in
            (v, Some init) :: acc
          ) table globals
        in
        (* Initialize global variables *)
        List.fold_left (fun flow (v, init) ->
            let range = mk_fresh_range () in
            let v = mk_var v range in
            init_expr man ctx v init true range flow
          ) flow globals

      | _ -> flow
    in
    ctx, flow

  let exec (man : ('a, unit) manager) ctx (stmt : stmt) (flow : 'a flow) : 'a flow option =
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration(v, None) when is_c_pointer_type v.vtyp -> None (* Let pointer domain initialize invalid addresses *)

    | S_c_local_declaration(v, init) ->
      let v = mk_var v range in
      init_expr man ctx v init false range flow |>
      return

    | _ -> None

  let eval man ctx exp flow = None

  let ask man ctx request flow = None


end

let setup () =
  register_domain name (module Domain)
