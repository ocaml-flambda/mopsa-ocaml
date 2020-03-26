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

(** Powerset lattice with finite cardinality elements or ⊺. *)

open Top

module type S =
sig
  type elt
  module Set : SetExtSig.S with type elt = elt
  type t = Set.t with_top
  val bottom : t
  val top : t
  val is_top : t -> bool
  val subset : t -> t -> bool
  val equal : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val widen : 'a -> t -> t -> t
  val print : Format.formatter -> t -> unit
  val add : Set.elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val remove : elt -> t -> t
  val mem : elt -> t -> bool
  val filter : (Set.elt -> bool) -> t -> t
  val partition : (Set.elt -> bool) -> t -> t * t
  val exists : (Set.elt -> bool) -> t -> bool
  val for_all : (Set.elt -> bool) -> t -> bool
  val cardinal : t -> int
  val find : Set.elt -> t -> elt
  val choose : t -> elt
  val singleton : elt -> t
  val of_list : elt list -> Set.t Top.with_top
  val is_empty : t -> bool
  val empty : t
  val is_bottom : t -> bool
  val is_singleton : t -> bool
  val elements : t -> Set.elt list
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val apply : (Set.t -> 'a) -> 'a -> t -> 'a
end
