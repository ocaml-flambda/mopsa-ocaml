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

(** Reduced product of value abstractions.

    This combiner implements a reduced product between 𝑚 value abstractions
    𝒱₁, ..., 𝒱ₘ. Each abstraction 𝒱ᵢ represent values of types 𝐓ᵢ, such that:
    ∀ j ≠ i: 𝐓ᵢ ∩ 𝐓ⱼ ≠ ∅. The types represented by the combiner is 𝐓₁ ∩ ... ∩ 𝐓ₘ.

    The reduced is represented with a cartesian product. The concretization of
    a product of values is the intersection of the concretizations:
    γ(v₁,...,vₘ) = γ₁(v₁) ∩ ... ∩ γₘ(vₘ).

    The reduced product is parameterized by a set of reduction rules ρ₁, ..., ρₖ.
    Each reduction rule is applied after each transfer function. Note that
    reduction rules can access to all abstractions and can reduce many values
    at the same time.
*)

open Sig.Abstraction.Value
open Sig.Reduction.Value

(** Create a pair of two value abstractions. *)
module MakeValuePair(V1:VALUE)(V2:VALUE) : VALUE with type t = V1.t * V2.t

(** Create a reduced product from an n-tuple value abstraction and a list
    of reduction rules.
*)
module Make(V:VALUE)(R:sig val rules: (module VALUE_REDUCTION) list end) : VALUE with type t = V.t

(** Create a reduced product from a list of value abstractions and a list
    of reduction rules.
*)
val make :
    (module VALUE) list ->
    (module VALUE_REDUCTION) list ->
    (module VALUE)
