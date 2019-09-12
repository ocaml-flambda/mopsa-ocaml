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
open Sig.Domain.Intermediate
open Ast
open Addr
open Universal.Ast
open Data_model.Attribute
open Alarms

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.types.dummy"
    end)

  let interface = {
    iexec = { provides = [Zone.Z_py]; uses = []; };
    ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]; }
  }

  let init _ _ flow = flow

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | _ -> None

  let exec _ stmt _ _ =
    let range = srange stmt in
    match skind stmt with
    | _ -> None

  let ask _ _ _ = None
  let refine c m f = Channel.return f

end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain);
