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

(** Signature of reduction rules for product evaluations

    In a reduced product, evaluations of member domains are combined by
    conjunction. Since each domain [Di] can return disjunctive evaluations
    [(e_i1, f_i1) ∨ ... ], the overall evaluation of [D1 ∧ ... ∧ Dn] is
    translated into a disjunctive normal form
    [(e_11 ∧ ... ∧ e_n1, f_11 ∩ ... ∩ f_n1) ∨ ...]

    Each resulting conjunction, called a product evaluation, represents the
    evaluations of the domains on the same state partition. Since expressions
    can not be combined, a transfer function [F] performed over a product
    evaluation [(e_1 ∧ ... ∧ e_n, f)] is equivalent to
    [(F e_1 f) ∩ ... ∩ (F e_n f)], which is inefficient.

    The goal of a reduction rule is to reduce a product evaluation
    [(e_1 ∧ ... ∧ e_n, f)] into a more efficient evaluation [(e', f')] by
    keeping the most precise evaluation and eventually keep information about
    the other ones in the abstract state (such as equality with [e']).
 *)

open Core.All



(** Manager used by reduction rules *)
type 'a eval_reduction_man = {
  get_man : 't. 't id -> ('a, 't) man; (** Get the manger of a domain *)
}


(** Signature of an evaluation reduction rule *)
module type EVAL_REDUCTION =
sig
  val name   : string
  (** Name of the reduction rule *)

  val reduce :
    expr -> ('a,'b) man -> 'a eval_reduction_man ->
    'a flow -> expr list -> 'a flow -> 'a eval option
  (** [reduce e man input_flow results output_flow] reduces a product evaluation *)
end


(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

(** Register a new eval reduction *)
val register_eval_reduction : (module EVAL_REDUCTION) -> unit

(** Find an eval reduction by its name *)
val find_eval_reduction : string -> (module EVAL_REDUCTION)

(** List all eval reductions *)
val eval_reductions : unit -> string list
