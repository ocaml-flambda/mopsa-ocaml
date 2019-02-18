(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

open Mopsa
open Ast
open MapExt
open Addr
open Universal.Ast
(* au moins gérer les strings *)

module Domain =
  struct
    let name = "python.types.t_set"
    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj]}

    let init _ _ _ = None

    let eval zs exp (man: ('a, unit) man) (flow:'a flow) : ('a, expr) evl option =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      (* 𝔼⟦ set.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
           when is_compare_op_fun "set" f ->
         Eval.eval_list [e1; e2] (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               Eval.assume
                 (mk_py_isinstance_builtin e1 "set" range)
                 ~fthen:(fun true_flow ->
                   Eval.assume
                     (mk_py_isinstance_builtin e2 "set" range)
                     ~fthen:(fun true_flow ->
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_bool range) true_flow)
                     ~felse:(fun false_flow ->
                       let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                     man true_flow
                 )
                 ~felse:(fun false_flow ->
                   let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                   Eval.empty_singleton flow)
                 man flow
             )
         |>  OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__len__")}, _)}, [e], []) ->
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
           Eval.bind (fun el flow ->
               Eval.assume
                 (mk_py_isinstance_builtin e "set" range)
                 ~fthen:(fun true_flow ->
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) true_flow)
                 ~felse:(fun false_flow ->
                   man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                     Eval.empty_singleton)
                 man flow)
         |> OptionExt.return



      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
