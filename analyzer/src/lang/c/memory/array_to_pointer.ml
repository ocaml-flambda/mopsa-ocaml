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
open Framework.Domains
open Framework.Flow
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
    remove_typedef t |>
    (function
      | T_c_array(t, _) -> t
      | _ -> assert false
    )
  let mk_c_array_subscript a i range =
    mk_expr (E_c_array_subscript (mk_var a range, i)) ~etyp:(under_array_type a.vtyp) range

  let init_array a init range man ctx flow =
    match init with
    | None -> flow

    | Some (C_init_list (l, None)) ->
      let rec aux flow i = function
        | [] -> flow
        | C_init_expr e :: tl ->
          let flow = man.exec
              (mk_assign
                 (mk_c_array_subscript a (mk_int i range) range)
                 e
                 range
              ) ctx flow
          in
          aux flow (i + 1) tl
        | _ -> assert false
      in
      aux flow 0 l
        
    | Some (C_init_list ([], Some C_init_expr e)) ->
      panic "Array filler initialization not supported"

    | _ ->
      panic "Array initialization not supported"

  let init prog man flow =
    match prog.prog_kind with
    | C_program(globals, _) ->
      List.fold_left (fun flow (v, init) ->
          if not (is_c_array v.vtyp) then
            flow
          else
            init_array v init (mk_fresh_range ()) man Framework.Context.empty flow
        ) flow globals

    | _ -> flow

  let exec (stmt : stmt) (man : ('a, unit) manager) ctx (flow : 'a flow) : 'a flow option =
    match skind stmt with
    | S_c_local_declaration(v, init) when is_c_array v.vtyp ->
      init_array v init stmt.srange man ctx flow |>
      Exec.return

    | _ -> None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_array_subscript(arr, idx) ->
      debug "array subscript to deref";
      let exp' = {exp with ekind = E_c_deref(mk_binop arr O_plus idx exp.erange ~etyp: arr.etyp)} in
      Eval.re_eval_singleton man ctx (Some exp', flow, [])

    | _ -> None



  let ask request man ctx flow = None


end

let setup () =
  register_domain name (module Domain)
