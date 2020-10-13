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

(** Signature of a value abstraction. *)

open Core.All


(*==========================================================================*)
(**                          {2 Value manager}                              *)
(*==========================================================================*)

(** Manager of value abstractions *)
type ('v,'t) value_man = {
  bottom : 'v;
  top    : 'v;
  is_bottom : 'v -> bool;
  subset : 'v -> 'v -> bool;
  join : 'v -> 'v -> 'v;
  meet : 'v -> 'v -> 'v;
  print : printer -> 'v -> unit;
  get  : 'v -> 't;
  set  : 't -> 'v -> 'v;
  eval : expr -> 'v;
  avalue : 'r. 'r avalue_kind -> 'v -> 'r;
  ask : 'a 'r. ('a,'r) query -> 'r;
}

(*==========================================================================*)
(**                        {2 Valued expressions}                           *)
(*==========================================================================*)

(** Valued exprssions annotate each node in the tree of the expression with
    its abstract value *)
type 'v vexpr

val empty_vexpr : 'v vexpr
(** Empty valued expression *)

val singleton_vexpr : expr -> 'v -> 'v vexpr -> 'v vexpr
(** Singleton representing a leaf expression and its value *)

val root_vexpr : 'v vexpr -> 'v vexpr
(** Get the root value expressions *)

val add_vexpr : expr -> 'v -> 'v vexpr -> 'v vexpr -> 'v vexpr
(** Attache a value to a sub-expression in a value expression *)

val refine_vexpr : expr -> 'v -> 'v vexpr -> 'v vexpr
(** Change the value of a sub-expression in a value expression *)

val find_vexpr : expr -> 'v vexpr -> 'v * 'v vexpr
(** Find the value of a sub-expression in a valued expression.
 ** Raises [Not_found] if the sub-expression is not found. *)

val find_vexpr_opt : expr -> 'v vexpr -> ('v * 'v vexpr) option
(** Same as [find_ctx_opt] but returns [None] if the sub-expression is not found *)

val map_vexpr : ('v -> 's) -> 'v vexpr -> 's vexpr
(** Map the value of each sub-expression *)

val fold_root_vexpr : ('a -> expr -> 'v -> 'v vexpr -> 'a) -> 'a -> 'v vexpr -> 'a
(** Fold over the direct sub-expressions only *)

val fold_vexpr : ('a -> expr -> 'v -> 'v vexpr -> 'a) -> 'a -> 'v vexpr -> 'a
(** Fold over all sub-expression *)

val map2_vexpr : ('v -> 't) -> ('s -> 't) -> ('v -> 's -> 't) -> 'v vexpr -> 's vexpr -> 't vexpr
(** Combine two valued expressions *)

val merge_vexpr : ('v -> 'v -> 'v) -> 'v vexpr -> 'v vexpr -> 'v vexpr
(** Combine two valued expressions *)



(*==========================================================================*)
(**                          {2 Value domain}                               *)
(*==========================================================================*)


module type VALUE =
sig

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  type t
  (** Type of the abstract value. *)

  val id : t id
  (** Identifier of the value domain *)

  val accept_type : typ -> bool
  (** Predicate of types abstracted by the value domain *)

  val name : string
  (** Name of the value domain *)

  val display : string
  (** Display name used in debug messages *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: 'a ctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Forward semantics} *)
  (** ********************* *)

  (** The forward semantics define how expressions are evaluated into
      abstract values *)

  val eval : ('v,t) value_man -> expr -> t
  (** Forward evaluation of expressions *)

  val avalue : 'r avalue_kind -> t -> 'r option
  (** Create an avalue over-approximating a given abstract value *)


  (** {2 Backward semantics} *)
  (** ********************** *)

  (** Backward semantics define how a refinement in the abstract value of an
      expression impact the abstract values of the sub-expressions *)

  val backward : ('v,t) value_man -> expr -> t vexpr -> 'v -> t vexpr
  (** Backward evaluation of expressions.
      [backward man e ve r] refines the values [ve] of the sub-expressions
      such that the evaluation of the expression [e] is in [r]
      i.e., we filter the sub-values [ve] knowing that, after applying
      the evaluating the expression, the result is in [r]
  *)

  val filter : bool -> typ -> t -> t
  (** Keep abstract values that represent a given truth value *)


  val compare : ('v,t) value_man -> operator -> bool -> expr -> t -> expr -> t -> (t * t)
  (** Backward evaluation of boolean comparisons.
      [compare man op true e1 v1 e2 v2] returns (v1',v2') where:
       - v1' abstracts the set of v  in v1 such that v1' op v' is true for some v' in v2'
       - v2' abstracts the set of v' in v2 such that v2' op v' is true for some v  in v1'
       i.e., we filter the abstract values v1 and v2 knowing that the test is true
  *)



  (** {2 Extended semantics} *)
  (** ********************** *)


  (** Extended semantics define the evaluation of mixed-types expressions.
      When an expression is composed of sub-expressions with different types,
      several abstraction may cooperate to compute the evaluation.
      The previous transfer functions can't be used, because they are defined
      over one abstraction only. *)

  val eval_ext : ('v,t) value_man -> expr -> 'v option
  (** Extend evaluation of expressions returning the global abstract value of [e].
      Note that the type of [e] may not satisfy the predicate [accept_type]. *)

  val backward_ext : ('v,t) value_man -> expr -> 'v vexpr -> 'v -> 'v vexpr option
  (** Extend backward evaluation of an heterogenous expression's sub-parts *)

  val compare_ext : ('v,t) value_man -> operator -> bool -> expr -> 'v -> expr -> 'v -> ('v * 'v) option
  (** Extend comparison between heterogenous expressions *)

  (** {2 Communication handlers } *)
  (** *************************** *)

  val ask : ('v,t) value_man -> ('a,'r) query -> 'r option
  (** Handler of queries *)


  (** {2 Pretty printer} *)
  (** ****************** *)

  val print: printer -> t -> unit
  (** Printer of an abstract element. *)

end

val default_filter : bool -> typ -> 't -> 't
val default_backward : ('v,'t) value_man -> expr -> 't vexpr -> 'v -> 't vexpr
val default_compare : ('v,'t) value_man -> operator -> bool -> expr -> 't -> expr -> 't -> ('t * 't)

(** Helper module defining default transfer functions *)
module DefaultValueFunctions :
sig
  val filter : bool -> typ -> 't -> 't
  val backward : ('v,'t) value_man -> expr -> 't vexpr -> 'v -> 't vexpr
  val compare : ('v,'t) value_man -> operator -> bool -> expr -> 't -> expr -> 't -> ('t * 't)
  val eval_ext : ('v,'t) value_man -> expr -> 'v option
  val backward_ext : ('v,'t) value_man -> expr -> 'v vexpr -> 'v -> 'v vexpr option
  val compare_ext : ('v,'t) value_man -> operator -> bool -> expr -> 'v -> expr -> 'v -> ('v * 'v) option
  val avalue : 'r avalue_kind -> 't -> 'r option
  val ask : ('v,'t) value_man -> ('a,'r) query -> 'r option
end

(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)

val register_value_abstraction : (module VALUE) -> unit
(** Register a new value abstraction *)

val find_value_abstraction : string -> (module VALUE)
(** Find a value abstraction by its name *)

val mem_value_abstraction : string -> bool
(** Check if an abstraction exists *)

val value_abstraction_names : unit -> string list
(** Get the list of registered abstractions *)
