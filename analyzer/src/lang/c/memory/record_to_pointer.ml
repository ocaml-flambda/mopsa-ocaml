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
open Framework.Domains
open Framework.Flow
open Framework.Utils
open Universal.Ast
open Ast


let name = "c.memory.record_to_pointer"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain = struct

  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let init prog man ctx flow = ctx, flow

  let exec (stmt : stmt) (man : ('a, unit) manager) ctx (flow : 'a flow) : 'a flow option =
    match skind stmt with
    | S_assign(lval, rval, smode) when is_c_record_type lval.etyp && is_c_record_type rval.etyp ->
      let range = srange stmt in
      let t1 = remove_typedef lval.etyp |> remove_qual and t2 = remove_typedef rval.etyp |> remove_qual in
      assert (compare t1 t2 = 0);
      let fields = match t1 with
        | T_c_record{c_record_fields} -> c_record_fields
        | _ -> assert false
      in
      fields |> List.fold_left (fun flow field ->
        let lval = mk_c_member_access lval field range in
        let rval = mk_c_member_access rval field range in
        let stmt = {stmt with skind = S_assign(lval, rval, smode)} in
        man.exec stmt ctx flow
        ) flow |>
      return

    | _ -> None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_member_access(r, idx, f) ->
      let exp' = {exp with ekind = E_c_arrow_access(mk_c_address_of r r.erange, idx, f)} in
      re_eval_singleton (Some exp', flow, []) man ctx


    | _ -> None

  let ask request man ctx flow = None

end

let setup () =
  register_domain name (module Domain)
