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

(** Lattice of partial inversible maps.

    Sets of partial maps M ∈ ℘(𝕂 ⇀ 𝕍) from concrete keys set 𝕂 to
    concrete values set 𝕍 are abstracted as a set of partial maps ℳ ∈
    𝕂 ⇀ (℘(𝕍) ∪ {⊤}).
*)


open Mopsa_utils
open Top
open Core.All


module type S =
sig

  type key
  (** Keys of the map *)

  type value
  (** Values of the map *)

  module KeySet : SetExtSig.S with type elt = key
  (** Set of keys *)

  module ValueSet : SetExtSig.S with type elt = value
  (** Set of values *)

  type t
  (** Type of inversible maps *)

  val bottom : t
  (** ⊥ element *)

  val top : t
  (** ⊤ element *)

  val is_bottom : t -> bool
  (** [is_bottom a] checks whether [a] is ⊥ *)

  val print : Print.printer -> t -> unit
  (** Pretty printer. *)

  val empty : t
  (** Singleton of empty map *)

  val subset : t -> t -> bool
  (** Inclusion test. *)

  val join : t -> t -> t
  (** Join two sets of partial maps. *)

  val meet :t -> t -> t
  (** Intersect to sets of partial maps. *)

  val widen : 'a ctx -> t -> t -> t
  (** Widening operator *)

  val find : key -> t -> ValueSet.t with_top
  (** [find k a] find the set of values attached to key [k] in
      [a]. Raise [Not_found] of the key is not found.
  *)

  val find_inverse : value -> t -> KeySet.t with_top
  (** [find_inverse] finds the set of keys attached to value [v] in [a]. *)

  val remove : key -> t -> t
  (** [remove k a] removes all bindings [(k,-)] in [a] *)

  val remove_inverse : value -> t -> t
  (** [remove_inverse v a] remove all bindings [(-,v)] in [a] *)

  val filter : (key -> ValueSet.t with_top -> bool) -> t -> t
  (** [filter f a] keeps all bindings [(k,vs)] in [a] such that [f k vs] is true *)

  val filter_inverse : (value -> KeySet.t -> bool) -> t -> t
  (** [filter_inverse f a] keeps all inverse bindings [(v,ks)] in [a] such that [f v ks] is true *)

  val set : key -> ValueSet.t with_top -> t -> t
  (** [set k vs a] adds the binding [(k,vs)] to [a]. Previous bindings are overwritten. *)

  val add_inverse : value -> KeySet.t -> t -> t
  (** [add_inverse v ks a] adds the binding [(k,{v} ∪ find k a)] to [a], where [k] ∈ [ks]. *)

  val rename : key -> key -> t -> t
  (** [rename k k' a] renames key [k] to [k'] in [a] *)

  val singleton :key -> value -> t
  (** [singleton k v] createw a map with the singleton binding [(k,{v})] *)

  val mem :key -> t -> bool
  (** [mem k a] check whether a binding [(k,-)] exists in [a] *)

  val mem_inverse :value -> t -> bool
  (** [mem v a] check whether a binding [(_,v)] exists in [a] *)

  val fold :(key -> ValueSet.t with_top -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f a init] folds function [f] over elements [(k,vs)] in [a] *)

  val map : (ValueSet.t with_top -> ValueSet.t with_top) -> t -> t
  (** [map f a] replace bindings [(k,vs)] in [a] with [(k,f vs)] *)

end
