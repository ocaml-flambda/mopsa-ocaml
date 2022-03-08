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

(** Reduction rules for products of values abstractions *)

open Core.Id


(*==========================================================================*)
(**                       {1 Reduction manager}                             *)
(*==========================================================================*)

(** Manager for value reduction rules *)
type 'a value_reduction_man = {
  get : 'r. 'r id -> 'a -> 'r;
  (** [get id pv] returns the value of domain with identifier [id] in the
      product value [pv] *)

  set : 'r. 'r id -> 'r -> 'a -> 'a;
  (** [set id v pv] sets the value of domain with identifier [id] in the
      product value [pv] *)
}


(*==========================================================================*)
(**                             {1 Signature}                               *)
(*==========================================================================*)

module type VALUE_REDUCTION =
sig
  val name   : string
  (** Name of the reduction rule *)

  val reduce : 'v value_reduction_man -> 'v -> 'v
  (** [reduce man pv] applies the reduction rule on the product value [pv] *)
end


(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

val register_value_reduction : (module VALUE_REDUCTION) -> unit
(** Register a new value reduction *)

val find_value_reduction : string -> (module VALUE_REDUCTION)
(** Find a value reduction by its name *)

(** List all simplified value reductions *)
val simplified_value_reductions : unit -> string list
