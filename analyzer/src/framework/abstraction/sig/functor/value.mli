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


(** Signature of functors of value domains *)

open Domain.Value

(*==========================================================================*)
(**                           {1 Signature}                                 *)
(*==========================================================================*)

module type VALUE_FUNCTOR =
sig
  val name : string
  module Functor : functor(D:VALUE) -> VALUE
end



(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

val register_value_functor : (module VALUE_FUNCTOR) -> unit
(** Register a new functor of value domains *)


val find_value_functor : string -> (module VALUE_FUNCTOR)
(** Find a value functor by its name. Raise [Not_found] if no functor is found *)

val mem_value_functor : string -> bool
(** [mem_value_functor name] checks whether a value functor with name
    [name] is registered *)
 
val value_functor_names : unit -> string list
(** Return the names of registered value functor *) 
