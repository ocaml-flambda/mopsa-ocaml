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

(** Handling of and/or operators. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.desugar.bool"
      end)

    let checks = []

    let init _ _ flow = None

    let eval exp man flow =
      let range = erange exp in
      if is_py_exp exp then
      match ekind exp with
      | E_unop(O_py_not, e) ->
        man.eval   (Utils.mk_builtin_call "bool" [e] range) flow >>$
          (fun ee flow ->
            man.eval (mk_not {ee with etyp=(T_py (Some Bool))} range) flow)
        |> OptionExt.return

        (* E⟦ e1 and e2 ⟧ *)
      | E_binop(O_py_and, {ekind = E_constant (C_bool true)}, e2) ->
         man.eval e2 flow |> OptionExt.return

      | E_binop(O_py_and, {ekind = E_constant (C_bool false)}, e2) ->
         man.eval (mk_py_false range) flow |> OptionExt.return

      | E_binop(O_py_and, e1, e2) ->
         man.eval e1 flow >>$
           (fun ee1 flow ->
           man.eval (Utils.mk_builtin_call "bool" [ee1] range) flow >>$
           (fun be1 flow1 ->
             assume be1 man
               ~fthen:(fun true_flow -> man.eval e2 true_flow)
               ~felse:(fun false_flow -> Eval.singleton ee1 false_flow)
               flow1
             )
           )
         |> OptionExt.return

      (* E⟦ e1 or e2 ⟧ *)
      | E_binop(O_py_or, e1, e2) ->
         man.eval e1 flow >>$
         (fun ee1 flow ->
           man.eval (Utils.mk_builtin_call "bool" [ee1] range) flow >>$
           (fun be1 flow1 ->
             assume be1
               ~fthen:(fun true_flow ->
                 Eval.singleton ee1 true_flow)
               ~felse:(fun false_flow ->
                 man.eval e2 false_flow)
               man flow1)
         )
         |> OptionExt.return

      | E_binop(O_log_or, e1, e2) ->
         assume e1 man flow
           ~fthen:(fun true_flow ->
             man.eval e1 true_flow)
           ~felse:(fun false_flow ->
             man.eval e2 false_flow)
         |> OptionExt.return


      (* E⟦ e1 is not e2 ⟧ *)
      | E_binop(O_py_is_not, e1, e2) ->
         man.eval (mk_not (mk_binop ~etyp:(T_py None) e1 O_py_is e2 range) range) flow |> OptionExt.return

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_in, e1, e2) ->
         bind_list [e1; e2] man.eval flow |>
         bind_result_opt (fun el flow ->
             let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

             man.eval (mk_py_type e2 range) flow >>$
               (fun cls2 flow ->
                 assume
                   (Utils.mk_hasattr cls2 "__contains__" range) man flow
                   ~fthen:(fun true_flow ->
                     let exp' = mk_py_call (mk_py_attr cls2 "__contains__" range) [e2; e1] range in
                     Flow.add_safe_check Alarms.CHK_PY_TYPEERROR e2.erange true_flow |>
                       man.eval exp'
                     )
                   ~felse:(fun false_flow ->
                     let v = mk_range_attr_var range "iter_v" (T_py None) in
                     let stmt = mk_stmt (S_py_for (
                                 mk_var v range,
                                 e2,
                                 mk_if
                                   (mk_binop ~etyp:(T_py None) (mk_var v range) O_eq e1 range)
                                   (mk_stmt S_break range)
                                   (mk_block [] range)
                                   range,
                                 (mk_block [] range)
                                  )) range
                     in
                     Flow.add_safe_check Alarms.CHK_PY_TYPEERROR e2.erange false_flow |>
                       man.exec stmt >>%
                       man.eval (mk_var v range) |>
                       Cases.add_cleaners [mk_remove_var v range]
                   )
               )
             |> OptionExt.return
           )

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_not_in, e1, e2) ->
         man.eval (mk_not (mk_binop ~etyp:(T_py None) e1 O_py_in e2 range) range) flow |> OptionExt.return

      (* E⟦ e1 op e2 op e3 ... ⟧ *)
      | E_py_multi_compare(left, ops, rights) ->
         debug "multi compare";
         let range = erange exp in
         man.eval left flow >>$
           (fun left flow ->
             debug "left evaluated";
             let rec aux left flow = function
               | [] ->
                 debug "leaf case -> true";
                 man.eval (mk_py_true range) flow

               | (op, right) :: tl ->
                 man.eval right flow >>$
                   (fun right flow ->
                     assume
                       (mk_binop ~etyp:(T_py None) left op right range)
                       ~fthen:(fun true_flow -> aux right true_flow tl)
                       ~felse:(fun false_flow -> man.eval (mk_py_false range) flow)
                       man flow
                   )
             in
             aux left flow (List.combine ops rights)
           ) |> OptionExt.return

      | _ -> None
      else None

    let exec _ _ _ = None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()

  end

let () = register_stateless_domain (module Domain)
