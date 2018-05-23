(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for subscript access. *)

open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Flow
open Universal.Ast
open Framework.Domains.Stateless
open Framework.Exec
open Universal.Ast
open Ast
open Addr

let name = "python.objects.data_model.subscript"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_index_subscript(obj, index) ->
      eval_list [obj; index] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let obj, index = match el with [obj; index] -> obj, index | _ -> assert false in
          let cls = classof obj in

          let ok_cond = Utils.mk_addr_hasattr cls "__getitem__" range in
          let ok_flow = man.exec ctx (mk_assume ok_cond range) flow in
          let error_flow = man.exec ctx (mk_assume (mk_not ok_cond range) range) flow in

          let ok_case =
            if man.flow.is_cur_bottom ok_flow then None
            else
              let exp' = mk_py_call (mk_py_addr_attr cls "__getitem__" range) [obj; index] range in
              re_eval_singleton (man.eval ctx) (Some exp', ok_flow, [])
          in

          let error_case =
            if man.flow.is_cur_bottom error_flow then None
            else
              let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) error_flow in
              oeval_singleton (None, flow, [])
          in

          oeval_join ok_case error_case

        )

    | E_py_slice_subscript _ ->
      Framework.Exceptions.panic "slices not supported"

    | _ -> None

  let exec man ctx stmt flow =
    let range = stmt.srange in
    match skind stmt with
    | S_assign({ekind = E_py_index_subscript(obj, index)}, exp, mode) ->
      eval_list [exp; obj; index] (man.eval ctx) flow |>
      eval_to_exec
        (fun el flow ->
          let exp, obj, index = match el with [exp; obj; index] -> exp, obj, index | _ -> assert false in
          let cls = classof obj in
          let ok_cond = Utils.mk_addr_hasattr cls "__setitem__" range in
          let ok_flow = man.exec ctx (mk_assume ok_cond range) flow in
          let error_flow = man.exec ctx (mk_assume (mk_not ok_cond range) range) flow in

          let ok_case =
            if man.flow.is_cur_bottom ok_flow then
                man.flow.bottom
            else
              let exp' = mk_py_call (mk_py_addr_attr cls "__setitem__" range) [obj; index; exp] range in
              man.exec ctx {stmt with skind = S_expression(exp')} ok_flow
          in

          let error_case =
            if man.flow.is_cur_bottom error_flow then
              man.flow.bottom
            else
              man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow
          in

          man.flow.join ok_case error_case

        ) (man.exec ctx) man.flow |>
      return

    | S_assign({ekind = E_py_slice_subscript _}, exp, mode) ->
      Framework.Exceptions.panic "slices not supported"

    | _ -> None

  let init man ctx prog flow = ctx, flow
  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
