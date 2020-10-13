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


(** Signature of functors of stacked domains *)

open Stacked
open Core.Semantic

(*==========================================================================*)
(**                           {1 Signature}                                 *)
(*==========================================================================*)

module type STACKED_FUNCTOR =
sig
  val name : string
  module Functor : functor(D:STACKED)-> STACKED
end



(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

val register_stacked_functor : (module STACKED_FUNCTOR) -> unit
(** Register a new functor of stacked domains *)


val find_stacked_functor : string -> (module STACKED_FUNCTOR)
(** Find a stacked functor by its name. Raise [Not_found] if no functor is found *)

val mem_stacked_functor : string -> bool
(** [mem_stacked_functor name] checks whether a stacked functor with name
    [name] is registered *)
 
val stacked_functor_names : unit -> string list
(** Return the names of registered stacked functor *) 
