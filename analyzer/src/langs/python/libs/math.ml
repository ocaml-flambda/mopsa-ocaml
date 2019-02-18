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

(** Math Python library. *)


open Mopsa
open Universal.Ast
open Ast
open Addr


module Domain =
  struct

    type _ domain += D_python_libs_math : unit domain

    let id = D_python_libs_math
    let name = "python.libs.math"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_libs_math -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = { export = []; import = [] }
    let eval_interface = { export = [Zone.Z_py, Zone.Z_py_obj]; import = [] }

    let init _ _ flow = Some flow

    let exec _ _ _ _ = None

    let eval zones exp man flow = None
      (* let range = erange exp in
       * match ekind exp with
       * | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "math.sqrt")}, _)}, [e], []) ->
       *    Exceptions.panic "blaaaaa@\n";
       *    let exp' = mk_unop O_sqrt e ~etyp:(T_float F_DOUBLE) range in
       *    Eval.singleton exp' flow
       *    |> OptionExt.return
       *
       * | _ ->
       *    None *)


    let ask _ _ _ = None

  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
