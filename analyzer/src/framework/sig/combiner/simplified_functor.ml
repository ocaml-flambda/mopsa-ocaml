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

(** Simplified interface of functor domains. *)

open Core.All
open Abstraction.Simplified

module type SIMPLIFIED_FUNCTOR =
sig
  val name : string
  module Functor : functor(D:SIMPLIFIED) -> SIMPLIFIED
end




(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let functors : (module SIMPLIFIED_FUNCTOR) list ref = ref []

let register_simplified_functor f =
  functors := f :: !functors

let find_simplified_functor name =
  List.find (fun dom ->
      let module D = (val dom : SIMPLIFIED_FUNCTOR) in
      compare D.name name = 0
    ) !functors

let mem_simplified_functor name =
  List.exists (fun dom ->
      let module D = (val dom : SIMPLIFIED_FUNCTOR) in
      compare D.name name = 0
    ) !functors

let simplified_functor_names () =
  List.map (fun dom ->
      let module D = (val dom : SIMPLIFIED_FUNCTOR) in
      D.name
    ) !functors
