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

(** Empty domain. *)

open Domain

include Stateless.Make(
  struct
    let name = "framework.domains.empty"
    type _ domain += D_empty : unit domain
    let id = D_empty
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_empty -> Some Eq
      | _ -> None
    let exec_interface = {export = []; import = []}
    let eval_interface = {export = []; import = []}
    let init prog man flow = None
    let exec zone stmt man flow = None
    let eval zone exp man flow = None
    let ask query man flow = None
  end
  )
