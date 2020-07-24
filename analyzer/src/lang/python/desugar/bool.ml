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

    let alarms = []

    let init _ _ flow = flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_unop(O_py_not, e) ->
        man.eval ~route:(Semantic "Python") (Utils.mk_builtin_call "bool" [e] range) flow >>$
          (fun ee flow ->
            man.eval ~route:(Semantic "Python") (mk_not ee range) flow)
        |> OptionExt.return

        (* E⟦ e1 and e2 ⟧ *)
      | E_binop(O_py_and, {ekind = E_constant (C_bool true)}, e2) ->
         man.eval e2 flow |> OptionExt.return

      | E_binop(O_py_and, {ekind = E_constant (C_bool false)}, e2) ->
         Eval.singleton (mk_py_false range) flow |> OptionExt.return

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
         man.eval (mk_not (mk_binop e1 O_py_is e2 range) range) flow |> OptionExt.return

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_in, e1, e2) ->
         bind_list [e1; e2] man.eval flow |>
         bind_some_opt (fun el flow ->
             let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

             man.eval (mk_py_type e2 range) flow >>$
               (fun cls2 flow ->
                 assume
                   (Utils.mk_hasattr cls2 "__contains__" range)
                   ~fthen:(fun true_flow ->
                       let exp' = mk_py_call (mk_py_attr cls2 "__contains__" range) [e2; e1] range in
                       man.eval exp' true_flow
                     )
                   ~felse:(fun false_flow ->
                       assume
                         (Utils.mk_hasattr cls2 "__iter__" range)
                         ~fthen:(fun true_flow ->
                             let v = mktmp () in
                             let stmt = mk_stmt (S_py_for (
                                 mk_var v range,
                                 e2,
                                 mk_if
                                   (mk_binop (mk_var v range) O_eq e1 range)
                                   (mk_stmt S_break range)
                                   (mk_block [] range)
                                   range,
                                 (mk_block [] range)
                               )) range
                             in
                             let flow = man.exec stmt true_flow in
                             man.eval (mk_var v range) flow |> Cases.add_cleaners [mk_remove_var v range]
                           )
                         ~felse:(fun false_flow ->
                             assume
                               (Utils.mk_hasattr cls2 "__getitem__" range)
                               ~fthen:(fun true_flow ->
                                   panic_at range "evaluating 'in' operator using __getitem__ not supported"
                                 )
                               ~felse:(fun false_flow ->
                                 let msg = Format.asprintf "argument of type '%a' is not iterable" pp_addr_kind (akind @@ fst @@ object_of_expr cls2) in
                                 let flow = man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow in
                                 Eval.empty_singleton flow
                               )
                               man false_flow
                           )
                         man false_flow
                     ) man flow
               )
             |> OptionExt.return
           )

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_not_in, e1, e2) ->
         man.eval (mk_not (mk_binop e1 O_py_in e2 range) range) flow |> OptionExt.return

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
                 Eval.singleton (mk_py_true range) flow

               | (op, right) :: tl ->
                 man.eval right flow >>$
                   (fun right flow ->
                     assume
                       (mk_binop left op right range)
                       ~fthen:(fun true_flow -> aux right true_flow tl)
                       ~felse:(fun false_flow -> Eval.singleton (mk_py_false range) flow)
                       man flow
                   )
             in
             aux left flow (List.combine ops rights)
           ) |> OptionExt.return

      | _ -> None


    let exec _ _ _ = None

    let ask _ _ _ = None

  end

let () = register_stateless_domain (module Domain)
