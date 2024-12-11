(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2018-2019 The MOPSA Project.                               *)
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
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast
open Callstack
open Data_container_utils
open Py_list

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.iterable"
    end)

  let checks = []

  let init (prog:program) man flow =
    None

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.join" as f, _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_result
        (fun eargs flow ->
           if List.length eargs <> 2 then
             let msg = Format.asprintf "%s: too %s arguments: %d given, %d expected" f (if List.length eargs < 2 then "few" else "many") (List.length args) 2 in
             man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
             Eval.empty
           else
             let self, iterable = match eargs with a :: b :: [] -> a, b | _ -> assert false in
             assume (mk_py_isinstance_builtin self "str" range) man flow
               ~fthen:(fun flow ->
                   assume (mk_py_isinstance_builtin iterable "list" range) man flow
                     ~fthen:(fun flow ->
                         let var_iterable = Py_list.Domain.var_of_eobj iterable in
                         assume (mk_py_isinstance_builtin (mk_var ~mode:(Some WEAK) var_iterable range) "str" range) man flow
                           ~fthen:(fun flow ->
                             Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow |>
                             man.eval (mk_py_top T_string range))
                           ~felse:(fun flow ->
                               man.exec (Utils.mk_builtin_raise_msg "TypeError" "sequence item: expected str instance" range) flow >>%
                               Eval.empty
                             )
                       )
                     ~felse:(fun flow ->
                         assume (mk_py_isinstance_builtin iterable "tuple" range) man flow
                           ~fthen:(fun flow ->
                               let vars_tuple = Tuple.Domain.var_of_eobj iterable in
                               let assume_all =
                                 List.fold_left (fun acc var ->
                                     mk_binop ~etyp:(T_py None) acc O_py_and (mk_py_isinstance_builtin (mk_var ~mode:(Some WEAK) var range) "str" range) range) (mk_py_true range) vars_tuple in
                               assume assume_all man flow
                                 ~fthen:(fun flow ->
                                   Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow |>
                                   man.eval (mk_py_top T_string range))
                                 ~felse:(fun flow ->
                                     man.exec (Utils.mk_builtin_raise_msg "TypeError" "sequence item: expected str instance" range) flow >>%
                                     Eval.empty
                                   )
                             )
                           ~felse:(fun flow ->
                               man.exec (Utils.mk_builtin_raise_msg "TypeError" "can only join an iterable" range) flow >>%
                               Eval.empty
                             )
                       )
                 )
               ~felse:(fun flow ->
                 let msg = Format.asprintf "descriptor 'join' requires a 'str' object but received a '%a'" pp_expr self in
                   man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                   Eval.empty
                 )
        )
      |> OptionExt.return

    | _ -> None


  let exec stmt man flow = None

  let ask _ _ _ = None

  let print_expr _ _ _ _ = ()
end

let () =
  register_stateless_domain (module Domain)
