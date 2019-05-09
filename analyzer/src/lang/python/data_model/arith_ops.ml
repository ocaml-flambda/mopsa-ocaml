(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for arithmetic operators. *)


open Mopsa
open Ast
open Addr
open Operators
open Universal.Ast


module Domain =
  struct

    let name = "python.data_model.arith_ops"
    let debug fmt = Debug.debug ~channel:name fmt

    let interface = {
      iexec = {provides = []; uses = []};
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let init _ _ flow = flow

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_binop(op, e1, e2) when is_arith_op op (*&& is_py_expr e1 && is_py_expr e2*) ->
         debug "arith op@\n";
         Eval.eval_list (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) [e1; e2] flow |>
           Eval.bind
             (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

               let op_fun = binop_to_fun op in
               let rop_fun = binop_to_rev_fun op in

               (* let o1 = object_of_expr e1 and o2 = object_of_expr e2 in *)
               man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type e1 range) flow |>
                 Eval.bind (fun cls1 flow ->
                     let cls1 = object_of_expr cls1 in
                     man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type e2 range) flow |>
                       Eval.bind (fun cls2 flow ->
                           let cls2 = object_of_expr cls2 in
                           (* let cls1 = Addr.class_of_object o1 and cls2 = Addr.class_of_object o2 in *)

                           let is_same_type = compare_py_object cls1 cls2 = 0 in
                           let not_implemented_type = mk_py_type (mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range) range in

                           assume_eval
                             (Utils.mk_object_hasattr cls1 op_fun range)
                             ~fthen:(fun true_flow ->
                               man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object_attr cls1 op_fun range) [e1; e2] range) true_flow |>
                                 Eval.bind (fun r flow ->
                                     let expr = mk_py_isinstance r not_implemented_type range in
                                     assume_eval expr
                                       ~fthen:(fun true_flow ->
                                         let flow = true_flow in
                                         (* if is_not_implemented r then *)
                                         if is_same_type then
                                           let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                                           Eval.empty_singleton flow
                                         else
                                           assume_eval
                                             (Utils.mk_object_hasattr cls2 rop_fun range)
                                             ~fthen:(fun true_flow ->
                                               man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) true_flow |>
                                                 Eval.bind (fun r flow ->
                                                     assume_eval
                                                       (mk_py_isinstance r not_implemented_type range)
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
                                 assume_eval
                                   (Utils.mk_object_hasattr cls2 rop_fun range)
                                   ~fthen:(fun true_flow ->
                                     man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) true_flow |>
                                       Eval.bind (fun r flow ->
                                           assume_eval
                                             (mk_py_isinstance r not_implemented_type range)
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
         |> Option.return
      | E_unop(op, e) when is_arith_op op(* && is_py_expr e*) ->
         debug "Resolving unary operator %a" pp_operator op;
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
           Eval.bind (fun e flow ->
               debug "Subexpression evaluated to %a(%a)" pp_expr e pp_typ e.etyp;
               let op_fun = unop_to_fun op in
               man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type e range) flow |>
                 Eval.bind (fun cls flow ->
                     let cls = object_of_expr cls in
                     assume_eval
                       (Utils.mk_object_hasattr cls op_fun range)
                       ~fthen:(fun true_flow ->
                         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object_attr cls op_fun range) [e] range) true_flow
                       )
                       ~felse:(fun false_flow ->
                         let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                         Eval.empty_singleton flow
                       )
                       man flow
                   )
             )
         |> Option.return
      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None

  end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
