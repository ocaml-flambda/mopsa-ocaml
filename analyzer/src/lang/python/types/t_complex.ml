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

module Domain =
  struct
    type _ domain += D_python_types_t_complex : unit domain

    let id = D_python_types_t_complex
    let name = "python.types.t_complex"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_types_t_complex -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj]}

    let init _ _ _ = None

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "complex.__new__")}, _)}, [cls], []) ->
         (* FIXME?*)
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_py_complex range) flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "complex.__new__")}, _)}, [cls; arg], []) ->
        Utils.check_instances_disj man flow range [arg] [["float"; "int"; "str"]] (fun _ -> man.eval (mk_py_top T_py_complex range))
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "complex.__new__")}, _)}, [cls; arg1; arg2], []) ->
        Utils.check_instances_disj man flow range [arg1; arg2] [["float"; "int"; "str"]; ["float"; "int"; "str"]] (fun _ -> man.eval (mk_py_top T_py_complex range))
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "complex.__new__")}, _)}, args, []) ->
        man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton |> OptionExt.return

      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
