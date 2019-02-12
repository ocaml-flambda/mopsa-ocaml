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

open MapExtSig

module Make(Ord: OrderedType) : S with type key = Ord.t
(** Generic functor to build a map data-type from ordered keys
    to an arbitrary type.
 *)

val printer_default : map_printer
(** Print as {key1:val1;key2:val2;...} *)


module StringMap   : S with type key = string
module IntMap      : S with type key = int
module Int32Map    : S with type key = int32
module Int64Map    : S with type key = int64
module ZMap        : S with type key = Z.t
(** A few useful map instances. *)
