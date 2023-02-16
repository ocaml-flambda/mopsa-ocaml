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

(** Generic non-relational abstraction.

    This combiner lifts a non-relational value abstraction 𝒱 into an
    abstract domain of partial environments 𝐕↛𝒱 from variables to values.

    The concretization of the domain is:
    γ(X) = { ρ | dom(ρ) ⊆ dom(X) ∧ ∀ v ∈ dom(ρ): ρ(v) ∈ γᵥ(X(v)) }
*)

open Core.All
open Sig.Abstraction.Value
open Mopsa_utils


(** {2 Identifier for the non-relation domain} *)
(** ****************************************** *)

(** Identifier of a non-relational domain *)
type _ id +=
  | D_nonrel :
      (module VALUE with type t = 'v) ->
      (var,'v) Lattices.Partial_map.map id


(** {2 Variable's context} *)
(** ********************** *)

(** The context of a variable keeps (flow-insensitive) information about the
   variable that can pushed by external domains and consumed by the value
   abstraction.

   This is useful to implement a widening with thresholds: external
   heuristics discover the theresholds and put them in the context of the
   variable. When [widen] is called on a the value of a variable, it is enriched
   with its context. *)

val var_ctx_key : ('a,'a ctx VarMap.t) ctx_key
(** Access key to the map of variables contexts *)

val add_var_ctx : var -> ('a,'v) ctx_key -> 'v -> 'a ctx -> 'a ctx
(** Add a context to a variable *)

val find_var_ctx_opt : var -> ('a,'v) ctx_key -> 'a ctx -> 'v option
(** Find the context attached to a variable *)

val find_var_ctx : var -> ('a,'v) ctx_key -> 'a ctx -> 'v
(** Find the context attached to a variable *)

val remove_var_ctx : var -> ('a,'v) ctx_key -> 'a ctx -> 'a ctx
(** Remove the context attached to a variable *)

(** {2 Variable bounds} *)
(** ******************* *)

(** The bounds of a variable is an invariant about its value that is always valid.
    It is put in the context of the variable and is used to refine its value whenever it
    changes. *)

(** Context for saving the bounds of a variable *)
val var_bounds_ctx : ('a,constant) ctx_key

(** Add the bounds of a variable to a context *)
val add_var_bounds_ctx : var -> constant -> 'a ctx -> 'a ctx

(** Add the bounds of a variable to a flow *)
val add_var_bounds_flow : var -> constant -> 'a flow -> 'a flow

(** Remove the bounds of a variable from a context *)
val remove_var_bounds_ctx : var -> 'a ctx -> 'a ctx

(** Remove the bounds of a variable from a flow *)
val remove_var_bounds_flow : var -> 'a flow -> 'a flow

(** Find the bounds of a variable in a context *)
val find_var_bounds_ctx_opt : var -> 'a ctx -> constant option


(** {2 Non-relational domain} *)
(** ************************* *)

(** Create a non-relational domain from a value abstraction *)
module Make(Value: VALUE) :
  Sig.Abstraction.Simplified.SIMPLIFIED
  with type t = (var,Value.t) Lattices.Partial_map.map
