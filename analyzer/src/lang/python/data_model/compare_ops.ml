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

(** Python data model for comparison operators. *)


open Mopsa
open Ast
open Addr
open Operators
open Universal.Ast


module Domain = struct

  let name = "python.data_model.compare_ops"
  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = {provides = []; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
  }

  let init _ _ flow = flow


  (* check equality by inspecting types and instance addresses. Return
     None when comparison can not be determined. *)
  (* let is_equal (e1: expr) (e2: expr) : bool option =
   *   let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
   *   let cls1 = Addr.class_of_object o1 and cls2 = Addr.class_of_object o2 in
   *   (\* different type => not equal *\)
   *   if compare_py_object cls1 cls2 <> 0 then Some false
   *   else (\* different addresses => not equal *\)
   *     if compare_py_object o1 o2 <> 0 then
   *       Some false
   *     else (\* same strong addresses => equal *\)
   *       if not (Addr.is_weak o1) || not (Addr.is_weak o2) then
   *         Some true
   *       else (\* cannot say anything with weak addresses *\)
   *         None *)

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e1, e2) when is_comp_op op (*&& is_py_expr e1 && is_py_expr e2*) ->
      debug "compare op@\n";
      Eval.eval_list (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj))  [e1; e2] flow |>
      Eval.bind (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

          let op_fun, rop_fun =
            match op with
            | O_eq -> "__eq__", "__eq__"
            | O_ne -> "__ne__", "__ne__"
            | O_lt -> "__lt__", "__gt__"
            | O_le -> "__le__", "__ge__"
            | O_gt -> "__gt__", "__lt__"
            | O_ge -> "__ge__", "__le__"
            | _ -> assert false
          in

          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type e1 range) flow |>
          Eval.bind (fun cls1 flow ->
              let cls1 = object_of_expr cls1 in
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type e2 range) flow |>
              Eval.bind (fun cls2 flow ->
                  let cls2 = object_of_expr cls2 in
                  man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object_attr cls1 op_fun range) [e1; e2] range) flow |>
                  Eval.bind (fun cmp flow ->
                      let not_implemented_type = mk_py_type (mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range) range in
                      let expr = mk_py_isinstance cmp not_implemented_type range in
                      debug "Expr is %a@\n" pp_expr expr;
                      assume_eval
                        expr
                        ~fthen:(fun true_flow ->
                            (* FIXME: subclass priority check is not implemented *)
                            begin
                              (* FIXME: really necessary? *)
                              match op with
                              | O_eq | O_ne ->
                                assume_eval
                                  (mk_expr (E_binop(O_py_is, e1, e2)) range)
                                  ~fthen:(fun flow ->
                                      match op with
                                      | O_eq -> man.eval (mk_py_true range) flow
                                      | O_ne -> man.eval (mk_py_false range) flow
                                      | _ -> assert false)
                                  ~felse:(fun flow ->
                                      match op with
                                      | O_eq -> man.eval (mk_py_false range) flow
                                      | O_ne -> man.eval (mk_py_true range) flow
                                      | _ -> assert false)
                                  (* TODO *)
                                  (* ~fboth:(fun flow1 flow2 -> Eval.singleton (mk_py_top T_bool range) flow) *)
                                  man flow
                              | _ ->
                                man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) flow |>
                                Eval.bind (fun rcmp flow ->
                                    assume_eval (mk_py_isinstance rcmp not_implemented_type range) man flow
                                      ~fthen:(fun flow ->
                                          let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                                          Eval.empty_singleton flow
                                        )
                                      ~felse:(fun flow ->
                                          Eval.singleton rcmp flow
                                        )
                                  )
                            end)
                        ~felse:(fun false_flow ->
                            Eval.singleton cmp flow)
                        man flow)))) |> Option.return
        | _ -> None

  let exec _ _ _ _ = None
  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
