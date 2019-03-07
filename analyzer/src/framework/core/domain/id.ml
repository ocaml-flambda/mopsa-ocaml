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


(** Generators of fresh module identifiers for domains and values *)

open Eq

type _ domain = ..

module GenDomainId(M:sig type typ val name : string end) =
struct

  type _ domain += DId : M.typ domain

  let id = DId

  let name = M.name

  let identify : type a. a domain -> (M.typ, a) eq option =
    function
    | DId -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:M.name fmt
end


type _ value = ..


module GenValueId(M:sig type typ val name : string * string end) =
struct

  type _ value += VId : M.typ value

  let id = VId

  let name = M.name

  let identify : type a. a value -> (M.typ, a) eq option =
    function
    | VId -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:(fst M.name) fmt
end
