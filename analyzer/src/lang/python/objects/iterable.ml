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
open Sig.Domain.Stateless
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

  let interface = {
    iexec = {provides = []; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
  }

  let init (prog:program) man flow =
    flow

  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.join" as f))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some
        (fun eargs flow ->
           if List.length eargs <> 2 then
             let () = Format.fprintf Format.str_formatter "%s: too %s arguments: %d given, %d expected" f (if List.length eargs < 2 then "few" else "many") (List.length args) 2 in
             man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
             Eval.empty_singleton
           else
             let self, iterable = match eargs with a :: b :: [] -> a, b | _ -> assert false in
             assume (mk_py_isinstance_builtin self "str" range) man flow
               ~fthen:(fun flow ->
                   assume (mk_py_isinstance_builtin iterable "list" range) man flow
                     ~fthen:(fun flow ->
                         let var_iterable = Py_list.Domain.var_of_eobj iterable in
                         assume (mk_py_isinstance_builtin (mk_var ~mode:WEAK var_iterable range) "str" range) man flow
                           ~fthen:(man.eval (mk_py_top T_string range))
                           ~felse:(fun flow ->
                               man.exec (Utils.mk_builtin_raise_msg "TypeError" "sequence item: expected str instance" range) flow |>
                               Eval.empty_singleton
                             )
                       )
                     ~felse:(fun flow ->
                         assume (mk_py_isinstance_builtin iterable "tuple" range) man flow
                           ~fthen:(fun flow ->
                               let vars_tuple = Tuple.Domain.var_of_eobj iterable in
                               let assume_all =
                                 List.fold_left (fun acc var ->
                                     mk_binop acc O_py_and (mk_py_isinstance_builtin (mk_var ~mode:WEAK var range) "str" range) range) (mk_py_true range) vars_tuple in
                               assume assume_all man flow
                                 ~fthen:(man.eval (mk_py_top T_string range))
                                 ~felse:(fun flow ->
                                     man.exec (Utils.mk_builtin_raise_msg "TypeError" "sequence item: expected str instance" range) flow |>
                                     Eval.empty_singleton
                                   )
                             )
                           ~felse:(fun flow ->
                               man.exec (Utils.mk_builtin_raise_msg "TypeError" "can only join an iterable" range) flow |>
                               Eval.empty_singleton
                             )
                       )
                 )
               ~felse:(fun flow ->
                   Format.fprintf Format.str_formatter "descriptor 'join' requires a 'str' object but received a '%a'" pp_expr self;
                   man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
                   Eval.empty_singleton
                 )
        )
      |> Option.return

    | _ -> None


  let exec zone stmt man flow = None

  let ask _ _ _ = None
end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
