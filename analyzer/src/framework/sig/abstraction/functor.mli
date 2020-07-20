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


(** Signature of functors of standard domains *)

open Domain
open Core.Semantic

(*==========================================================================*)
(**                           {1 Signature}                                 *)
(*==========================================================================*)

module type DOMAIN_FUNCTOR =
sig
  val name : string
  val dependencies : semantic list
  module Functor : functor(D:DOMAIN) -> DOMAIN
end



(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

val register_domain_functor : (module DOMAIN_FUNCTOR) -> unit
(** Register a new functor of standard domains *)


val find_domain_functor : string -> (module DOMAIN_FUNCTOR)
(** Find a domain functor by its name. Raise [Not_found] if no functor is found *)

val mem_domain_functor : string -> bool
(** [mem_domain_functor name] checks whether a domain functor with name
    [name] is registered *)
 
val domain_functor_names : unit -> string list
(** Return the names of registered domain functor *) 
