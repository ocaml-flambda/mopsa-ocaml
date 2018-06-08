(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for arithmetic operators. *)


open Framework.Domains
open Framework.Flow
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Universal.Ast
open Framework.Domains.Stateless
open Universal.Ast
open Ast
open Addr
open Operators

let name = "python.data_model.arith_ops"
let debug fmt = Debug.debug ~channel:name fmt


module Domain = struct

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e1, e2) when is_arith_op op && is_py_expr e1 && is_py_expr e2 ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

          let op_fun = binop_to_fun op in
          let rop_fun = binop_to_rev_fun op in

          let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
          let cls1 = Addr.class_of_object o1 and cls2 = Addr.class_of_object o2 in

          let is_same_type = compare_py_object cls1 cls2 = 0 in

          Universal.Utils.assume_to_eval
            (Utils.mk_object_hasattr cls1 op_fun range)
            (fun true_flow ->
               man.eval ctx (mk_py_call (mk_py_object_attr cls1 op_fun range) [e1; e2] range) true_flow |>
               eval_compose (fun r flow ->
                   if is_not_implemented r then
                     if is_same_type then
                       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                       oeval_singleton (None, flow, [])
                     else
                       Universal.Utils.assume_to_eval
                         (Utils.mk_object_hasattr cls2 rop_fun range)
                         (fun true_flow ->
                            man.eval ctx (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) true_flow |>
                            eval_compose (fun r flow ->
                                if is_not_implemented r then
                                  let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                                  oeval_singleton (None, flow, [])
                                else
                                  oeval_singleton (Some r, flow, [])
                              )
                         )
                         (fun false_flow ->
                            let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow in
                            oeval_singleton (None, flow, [])
                         )
                         man ctx flow ()
                   else
                     oeval_singleton (Some r, flow, [])
                 )
            )
            (fun false_flow ->
               if is_same_type then
                 let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                 oeval_singleton (None, flow, [])
               else
                 Universal.Utils.assume_to_eval
                   (Utils.mk_object_hasattr cls2 rop_fun range)
                   (fun true_flow ->
                      man.eval ctx (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) true_flow |>
                      eval_compose (fun r flow ->
                          if is_not_implemented r then
                            let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                              oeval_singleton (None, flow, [])
                          else
                            oeval_singleton (Some r, flow, [])
                        )
                   )
                   (fun false_flow ->
                      let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                      oeval_singleton (None, flow, [])
                   )
                   man ctx flow ()
            )
            man ctx flow ()
        )

    | E_unop(op, e) when is_arith_op op && is_py_expr e ->
      debug "Resolving unary operator %a" Framework.Pp.pp_operator op;
      man.eval ctx e flow |>
      eval_compose (fun e flow ->
          debug "Subexpression evaluated to %a(%a)" Framework.Pp.pp_expr e Framework.Pp.pp_typ e.etyp;
          let op_fun = unop_to_fun op in
          let obj = object_of_expr e in
          let cls = Addr.class_of_object obj in
          Universal.Utils.assume_to_eval
              (Utils.mk_object_hasattr cls op_fun range)
              (fun true_flow ->
                 man.eval ctx (mk_py_call (mk_py_object_attr cls op_fun range) [e] range) true_flow |>
                 return
              )
              (fun false_flow ->
                 let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow in
                 oeval_singleton (None, flow, [])
              )
              man ctx flow ()
        )

    | _ -> None

  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
