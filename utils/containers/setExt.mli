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

open SetExtSig

module Make(Ord: OrderedType) : S with type elt = Ord.t
(** Generic functor to build a set data-type from ordered elements.
 *)     

val printer_default : set_printer
(** Print as set: {elem1,...,elemn}. *)

module StringSet   : S with type elt = string
module IntSet      : S with type elt = int
module Int32Set    : S with type elt = int32
module Int64Set    : S with type elt = int64
module ZSet        : S with type elt = Z.t
(** A few useful set instances. *)
           
