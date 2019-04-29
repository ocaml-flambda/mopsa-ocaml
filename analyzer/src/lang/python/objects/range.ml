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

(** Range objects. *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast

module Domain =
  struct

    let name = "python.objects.range"

    let debug fmt = Debug.debug ~channel:name fmt

    let interface = {
      iexec = {provides = []; uses = []};
      ieval = {provides = [Zone.Z_py, Zone.Z_py; Zone.Z_py, Zone.Z_py_obj]; uses = []}
    }

    let rec eval zs exp man flow =
      let range = exp.erange in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__new__")}, _)} as call, cls :: [up], []) ->
        let args' = (mk_constant T_int (C_int (Z.of_int 0)) range)::up::(mk_constant T_int (C_int (Z.of_int 1)) range)::[] in
        man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
        |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range.__new__")}, _)} as call, cls :: [down; up], []) ->
        let args' = down::up::(mk_constant T_int (C_int (Z.of_int 1)) range)::[] in
        man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
        |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "range_iterator.__iter__")}, _)}, [self], []) ->
        man.eval self flow |> Option.return

      | _ -> None

    let init _ _ flow = flow
    let exec _ _ _ _ = None
    let ask _ _ _ = None

  end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
