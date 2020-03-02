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

(** None constant. *)

(* open Mopsa
 * open Ast
 * open Addr
 * open Universal.Ast
 *
 * module Domain =
 *   struct
 *
 *     let name = "python.objects.nones"
 *     let debug fmt = Debug.debug ~channel:name fmt
 *
 *     let exec_interface = {export = []; import = []}
 *     let eval_interface = {export = [Zone.Z_py, Zone.Z_py]; import = []}
 *
 *     let rec eval zs exp man flow =
 *       let range = exp.erange in
 *       match ekind exp with
 *       (\* 𝔼⟦ None ⟧ *\)
 *       | E_constant (C_py_none) ->
 *          Eval.singleton (mk_py_none range) flow |> OptionExt.xt.return
 *       | _ -> None
 *
 *     let init _ _ flow = Some flow
 *     let exec _ _ _ _ = None
 *     let ask _ _ _ = None
 *
 *   end
 *
 * let () =
 *   Framework.Domains.Stateless.register_domain (module Domain) *)
