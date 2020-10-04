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



open Bot_top
open Top
open Core.All
open Partial_inversible_map_sig


(** Signature of ordered types with printers *)
module type ORDER =
sig
  type t
  val compare: t -> t -> int
  val print : Core.Print.printer -> t -> unit
end


module Make
    (Key   : ORDER)
    (Value : ORDER)
:
  S with

  type key := Key.t and
  type value := Value.t and
  module KeySet = SetExt.Make(Key) and
  module ValueSet = SetExt.Make(Value)
