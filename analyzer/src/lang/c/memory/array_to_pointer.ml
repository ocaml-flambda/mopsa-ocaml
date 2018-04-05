(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Arrays transformer into pointers arithmetics. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Stateless
open Framework.Manager
open Framework.Visitor
open Framework.Flow
open Framework.Utils
open Universal.Ast
open Ast


let name = "c.memory.array_to_pointer"
let debug fmt = Debug.debug ~channel:name fmt



(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain = struct


  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)
  let under_array_type t =
    match remove_typedef t with
    | T_c_array(t, _) -> t
    | _ -> assert false

  let get_array_length t =
    match remove_typedef t with
    | T_c_array(_, C_array_length_cst n) -> Z.to_int n
    | _ -> assert false

  let mk_c_array_subscript a i range =
    mk_expr (E_c_array_subscript (a, i)) ~etyp:(under_array_type a.etyp) range

  let init_array a init range man ctx flow =
    match init with
    | None -> flow

    | Some (C_init_list (l, None)) ->
      let rec aux flow a i = function
        | [] -> flow
        | C_init_expr e :: tl ->
          let flow = man.exec
              (mk_assign
                 (mk_c_array_subscript a (mk_int i range) range)
                 e
                 range
              ) ctx flow
          in
          aux flow a (i + 1) tl
        | C_init_list (l, None) :: tl ->
          let flow = aux flow (mk_c_array_subscript a (mk_int i range) range) 0 l in
          aux flow a (i + 1) tl

        | _ -> assert false
      in
      aux flow (mk_var a range) 0 l

    | Some (C_init_list ([], Some C_init_expr e)) ->
      panic "Array filler initialization not supported"

    | Some (Ast.C_init_expr {ekind = E_constant(C_string s)}) ->
      let v = mk_var a range in
      let l = get_array_length a.vtyp in
      let rec aux i flow =
        if i = l then flow
        else
          let c = if i < String.length s then String.get s i else char_of_int 0 in
          let flow = man.exec
              (mk_assign
                 (mk_c_array_subscript v (mk_int i range) range)
                 (mk_constant (C_c_character c) range ~etyp:(T_c_integer(C_char C_signed)))
                 range
              ) ctx flow
          in
          aux (i + 1) flow
      in
      aux 0 flow

    | _ ->
      panic "Array initialization not supported"

  let init prog man flow =
    match prog.prog_kind with
    | C_program(globals, _) ->
      List.fold_left (fun flow (v, init) ->
          if not (is_c_array v.vtyp) then flow
          else
            init_array v init (mk_fresh_range ()) man Framework.Context.empty flow
        ) flow globals

    | _ -> flow

  let exec (stmt : stmt) (man : ('a, unit) manager) ctx (flow : 'a flow) : 'a flow option =
    match skind stmt with
    | S_c_local_declaration(v, init) when is_c_array v.vtyp ->
      init_array v init stmt.srange man ctx flow |>
      return

    | _ -> None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_array_subscript(arr, idx) ->
      debug "array subscript to deref";
      let exp' = {exp with ekind = E_c_deref(mk_binop arr O_plus idx exp.erange ~etyp: arr.etyp)} in
      re_eval_singleton (Some exp', flow, []) man ctx

    | _ -> None



  let ask request man ctx flow = None


end

let setup () =
  register_domain name (module Domain)
