(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interpreter of assignment expressions. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.desugar.assign"
let debug fmt = Debug.debug ~channel:name fmt


(** Abstract domain. *)
module Domain =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let eval man ctx exp flow =
    match ekind exp with
    | E_c_assign(lval, rval) ->
      man.eval ctx rval flow |>
      eval_compose
        (fun rval flow ->
           let flow = man.exec ctx (Universal.Ast.mk_assign lval rval exp.erange) flow in
           oeval_singleton (Some rval, flow, [])
        )

    | E_c_statement s ->
      assert false

    | _ -> None

  let exec man ctx stmt flow =
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
          man.exec ctx stmt flow
        ) flow |>
      return

    | _ -> None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
