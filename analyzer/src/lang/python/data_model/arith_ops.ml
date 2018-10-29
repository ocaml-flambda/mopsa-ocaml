(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for arithmetic operators. *)


open Framework.Essentials
open Universal.Ast
open Ast
open Addr
open Operators


module Domain =
  struct

    type _ domain += D_python_data_model_arith_ops : unit domain

    let id = D_python_data_model_arith_ops
    let name = "python.data_model.arith_ops"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_data_model_arith_ops -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [any_zone, any_zone]; import = []}

    let init _ _ flow = Some flow

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_binop(op, e1, e2) when is_arith_op op && is_py_expr e1 && is_py_expr e2 ->
         debug "arith op@\n";
         Eval.eval_list [e1; e2] man.eval flow |>
           Eval.bind
             (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

               let op_fun = binop_to_fun op in
               let rop_fun = binop_to_rev_fun op in

               (* let o1 = object_of_expr e1 and o2 = object_of_expr e2 in *)
               man.eval (mk_py_call (mk_py_object (Addr.find_builtin "type") range) [e1] range) flow |>
                 Eval.bind (fun cls1 flow ->
                     let cls1 = object_of_expr cls1 in
                     man.eval (mk_py_call (mk_py_object (Addr.find_builtin "type") range) [e2] range) flow |>
                       Eval.bind (fun cls2 flow ->
                           let cls2 = object_of_expr cls2 in
                           (* let cls1 = Addr.class_of_object o1 and cls2 = Addr.class_of_object o2 in *)

                           let is_same_type = compare_py_object cls1 cls2 = 0 in
                           let not_implemented_type = mk_py_call (mk_py_object (Addr.find_builtin "type") range) [mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range] range in

                           Eval.assume
                             (Utils.mk_object_hasattr cls1 op_fun range)
                             ~fthen:(fun true_flow ->
                               man.eval (mk_py_call (mk_py_object_attr cls1 op_fun range) [e1; e2] range) true_flow |>
                                 Eval.bind (fun r flow ->
                                     let expr = (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [r; not_implemented_type] range) in
                                     Eval.assume expr
                                       ~fthen:(fun true_flow ->
                                         let flow = true_flow in
                                         (* if is_not_implemented r then *)
                                         if is_same_type then
                                           let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                                           Eval.empty_singleton flow
                                         else
                                           Eval.assume
                                             (Utils.mk_object_hasattr cls2 rop_fun range)
                                             ~fthen:(fun true_flow ->
                                               man.eval (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) true_flow |>
                                                 Eval.bind (fun r flow ->
                                                     Eval.assume
                                                       (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [r; not_implemented_type] range)
                                                       ~fthen:(fun true_flow ->
                                                         (* if is_not_implemented r then *)
                                                         let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) true_flow in
                                                         Eval.empty_singleton flow)
                                                       ~felse:(fun false_flow ->
                                                         (* else *)
                                                         Eval.singleton r flow)
                                                       man flow
                                                   )
                                             )
                                             ~felse:(fun false_flow ->
                                               let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                                               Eval.empty_singleton flow
                                             )
                                             man flow)
                                       ~felse:(fun false_flow ->
                                         Eval.singleton r false_flow)
                                       man flow
                                   )
                             )
                             ~felse:(fun false_flow ->
                               if is_same_type then
                                 let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                                 Eval.empty_singleton flow
                               else
                                 Eval.assume
                                   (Utils.mk_object_hasattr cls2 rop_fun range)
                                   ~fthen:(fun true_flow ->
                                     man.eval (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) true_flow |>
                                       Eval.bind (fun r flow ->
                                           Eval.assume
                                             (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [r; not_implemented_type] range)
                                             ~fthen:(fun true_flow ->
                                               (* if is_not_implemented r then *)
                                               let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) true_flow in
                                               Eval.empty_singleton flow)
                                             ~felse:(fun false_flow ->
                                               Eval.singleton r flow)
                                             man flow
                                         )
                                   )
                                   ~felse:(fun false_flow ->
                                     let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                                     Eval.empty_singleton flow
                                   )
                                   man flow
                             )
                           man flow
             )))
         |> OptionExt.return
      | E_unop(op, e) when is_arith_op op && is_py_expr e ->
         debug "Resolving unary operator %a" Framework.Ast.pp_operator op;
         man.eval e flow |>
           Eval.bind (fun e flow ->
               debug "Subexpression evaluated to %a(%a)" Framework.Ast.pp_expr e Framework.Ast.pp_typ e.etyp;
               let op_fun = unop_to_fun op in
               let obj = object_of_expr e in
               let cls = Addr.class_of_object obj in
               Eval.assume
                 (Utils.mk_object_hasattr cls op_fun range)
                 ~fthen:(fun true_flow ->
                   man.eval (mk_py_call (mk_py_object_attr cls op_fun range) [e] range) true_flow
                 )
                 ~felse:(fun false_flow ->
                   let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                   Eval.empty_singleton flow
                 )
                 man flow
             )
         |> OptionExt.return
      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
