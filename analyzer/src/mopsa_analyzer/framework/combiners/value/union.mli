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

(** Union of value abstractions.

    This combiner implements a disjoint union between 𝑚 value abstractions
    𝒱₁, ..., 𝒱ₘ. Each abstraction 𝒱ᵢ represent values of types 𝐓ᵢ, such that:
    ∀ j ≠ i: 𝐓ᵢ ∩ 𝐓ⱼ = ∅. The types represented by the combiner is 𝐓₁ ∪ ... ∪ 𝐓ₘ.

    The union is represented with a cartesian product. The concretization of
    a union of values is the union of the concretizations:
    γ(v₁,...,vₘ) = γ₁(v₁) ∪ ... ∪ γₘ(vₘ).
*)

open Core.All
open Sig.Abstraction.Value
open Common
open Mopsa_utils


(** Create a disjoint union of two value abstractions *)
module Make(V1:VALUE)(V2:VALUE) : VALUE with type t = V1.t * V2.t

(** Create a disjoint union of a list of value abstractions *)
val make :(module VALUE) list -> (module VALUE)
