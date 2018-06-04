(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for augmented assignments. *)


open Framework.Domains.Stateless
open Framework.Domains
open Framework.Flow
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Universal.Ast
open Universal.Ast
open Ast
open Addr
open Operators

let name = "python.data_model.aug_assign"
let debug fmt = Debug.debug ~channel:name fmt


module Domain = struct

  let exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_aug_assign(x, op, e) ->
      let x0 = x in
      eval_list [e; x] (man.eval ctx) flow |>
      eval_to_oexec (fun el flow ->
          let e, x = match el with [e; x] -> e, x | _ -> assert false in

          let op_fun = Operators.binop_to_incr_fun op in
          let cls = Addr.classof @@ addr_of_expr x in
          Universal.Utils.assume_to_exec
            (Utils.mk_addr_hasattr cls op_fun range)
            (fun true_flow ->
                let stmt = mk_assign x0 (mk_py_call (mk_py_addr_attr cls op_fun range) [x; e] range) range in
                man.exec ctx stmt true_flow
            )
            (fun false_flow ->
               let default_assign = mk_assign x0 (mk_binop x op e range) range in
               man.exec ctx default_assign flow
            )
            man ctx flow () |>
          return
        ) (man.exec ctx) man.flow

    | _ -> None

  let init man ctx prog flow = ctx, flow
  let eval _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
