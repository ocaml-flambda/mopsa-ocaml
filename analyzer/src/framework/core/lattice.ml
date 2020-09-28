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

(** Interface of lattice structures *)

open Log
open Context
open Print

(** Signature of a lattice module. *)
module type LATTICE =
sig

  (** {2 Structure} *)
  (** ************* *)

  type t
  (** Type of an abstract elements. *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Predicates} *)
  (** ************** *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)


  (** {2 Operators} *)
  (** ************* *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: 'a ctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Printing} *)
  (** ************ *)

  val print: printer -> t -> unit
  (** Printer of an abstract element. *)

end



(** Lattice operators *)
type 'a lattice = {
  bottom:    'a;
  top:       'a;
  is_bottom: 'a -> bool;
  subset:    'a ctx -> 'a -> 'a -> bool;
  join:      'a ctx -> 'a -> 'a -> 'a;
  meet:      'a ctx -> 'a -> 'a -> 'a;
  widen:     'a ctx -> 'a -> 'a -> 'a;
  merge:     'a -> 'a * log -> 'a * log -> 'a;
  print:     printer -> 'a -> unit;
}
