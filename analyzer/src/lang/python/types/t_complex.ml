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
open Sig.Abstraction.Stateless
open Ast
open MapExt
open Addr
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.types.t_complex"
      end)

    let checks = []

    let init _ _ flow = flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_constant (C_top (T_py (Some Complex))) ->
        T_string.Domain.allocate_builtin man range flow "complex" (Some exp) |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("complex.__new__", _))}, _)}, [cls], []) ->
        Utils.new_wrapper man range flow "complex" cls
          ~fthennew:(man.eval (mk_py_top (T_py (Some Complex)) range))

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("complex.__new__" as f, _))}, _)}, [cls; arg], []) ->
        Utils.check_instances_disj f man flow range [arg] [["float"; "int"; "str"]] (fun _ -> man.eval (mk_py_top (T_py (Some Complex)) range))
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("complex.__new__" as f, _))}, _)}, [cls; arg1; arg2], []) ->
        Utils.check_instances_disj f man flow range [arg1; arg2] [["float"; "int"; "str"]; ["float"; "int"; "str"]] (fun _ -> man.eval (mk_py_top (T_py (Some Complex)) range))
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("complex.__new__", _))}, _)}, args, []) ->
        man.exec (Utils.mk_builtin_raise "TypeError" range) flow >>% Eval.empty |> OptionExt.return

      | _ -> None

    let exec _ _ _ = None
    let ask _ _ _ = None
  end

let () = register_stateless_domain (module Domain)
