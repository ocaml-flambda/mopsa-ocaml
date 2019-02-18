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

(** Inliner of list comprehensions. This translates
   comprehensions into for loops. While this is not the best in terms
   of precision, due to the widenings we may have to do afterwards,
   it's a generic, rewriting-based approach that may be helpful *)


open Mopsa
open Addr
open Ast
open Universal.Ast

module Domain =
  struct

    type _ domain += D_python_desugar_comprehensions : unit domain

    let id = D_python_desugar_comprehensions
    let name = "python.desugar.comprehensions"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_desugar_comprehensions -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = [Zone.Z_py]}
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj]}

    let unfold_comprehension expr comprehensions base append range =
         let tmp_acc = mktmp () in
         let acc_var = mk_var tmp_acc range in
         let rec unfold_lc aux_compr = match aux_compr with
           | [] ->
              mk_stmt (S_expression (mk_py_call append (acc_var::expr) range)) range
           | (target, iter, conds)::tl ->
              (* todo: mk_remove target in the end *)
              let i_conds = List.rev conds in
              let empty_stmt = mk_stmt (Universal.Ast.S_block []) range in
              let if_stmt = List.fold_left (fun acc cond ->
                                mk_stmt (Universal.Ast.S_if (cond, acc, empty_stmt)) range
                              ) (unfold_lc tl) i_conds in
              mk_stmt (S_py_for(target, iter,
                                if_stmt,
                                empty_stmt)) range in
         let clean_targets = List.fold_left (fun acc (target, _, _) -> match ekind target with
                                                                       | E_var (v, _) -> (mk_remove_var v range)::acc
                                                                       | _ -> Exceptions.panic "Comprehension: target %a is not a variable...@\n" pp_expr target) [] comprehensions in
         let stmt = mk_block ((mk_assign acc_var base range) :: (unfold_lc comprehensions) :: clean_targets) range in
         stmt, tmp_acc


    let init _ _ flow = Some flow
    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_list_comprehension (expr, comprehensions) ->
         let list = find_builtin "list" in
         let listappend = mk_py_object (find_builtin_attribute list "append") range in
         let stmt, tmp_acc = unfold_comprehension [expr] comprehensions (mk_expr (E_py_list []) range) listappend range in
         let acc_var = mk_var tmp_acc range in
         debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
         man.exec stmt flow |>
           man.eval acc_var |>
           Eval.add_cleaners [mk_remove_var tmp_acc range] |>
           OptionExt.return

      | E_py_set_comprehension (expr, comprehensions) ->
         let set = find_builtin "set" in
         let setadd = mk_py_object (find_builtin_attribute set "add") range in
         let emptyset = mk_expr (E_py_set []) range in
         let stmt, tmp_acc = unfold_comprehension [expr] comprehensions emptyset setadd range in
         let acc_var = mk_var tmp_acc range in
         debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
         man.exec stmt flow |>
           man.eval acc_var |>
           Eval.add_cleaners [mk_remove_var tmp_acc range] |>
           OptionExt.return

      | E_py_dict_comprehension (key, value, comprehensions) ->
         let dict = find_builtin "dict" in
         let dictset = mk_py_object (find_builtin_attribute dict "__setitem__") range in
         let emptydict = mk_expr (E_py_dict ([], [])) range in
         let stmt, tmp_acc = unfold_comprehension (key::value::[]) comprehensions emptydict dictset range in
         let acc_var = mk_var tmp_acc range in
         debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
         man.exec stmt flow |>
           man.eval acc_var |>
           Eval.add_cleaners [mk_remove_var tmp_acc range] |>
           OptionExt.return

      | E_py_generator_comprehension (expr, comprehensions) ->
         Debug.warn "No desugaring for generator comprehensions@\n"; None

      | _ -> None

    let exec _ _ _ _ = None

    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
