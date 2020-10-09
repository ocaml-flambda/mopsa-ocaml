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

(** Simplified signature of a value abstraction. *)

open Core.All


(*==========================================================================*)
(**                          {2 Value domain}                               *)
(*==========================================================================*)


module type SIMPLIFIED_VALUE =
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

  val constant : constant -> typ -> t
  (** Evaluation of constants *)

  val unop : operator -> typ -> t -> typ -> t
  (** Evaluation of unary operators *)

  val binop : operator -> typ -> t -> typ -> t -> typ -> t
  (** Evaluation of binary operators *)

  val filter : bool -> typ -> t -> t
  (** Filter of truth values *)

  val avalue : 'r avalue_kind -> t -> 'r option
  (** Cast to avalues *)

  (** {2 Backward semantics} *)
  (** ********************** *)

  val backward_unop  : operator -> typ -> t -> typ -> t -> t
  (** Backward evaluation of unary operators *)

  val backward_binop : operator -> typ -> t -> typ -> t -> typ -> t -> t * t
  (** Backward evaluation of binary operators *)

  val compare : operator -> bool -> typ -> t -> typ -> t -> (t * t)
  (** Backward evalaution of comparisons *)

  (** {2 Pretty printer} *)
  (** ****************** *)

  val print: printer -> t -> unit
  (** Printer of an abstract element. *)

end


(** Some default transfer functions *)
val default_backward_unop : operator -> typ -> 't -> typ -> 't -> 't
val default_backward_binop : operator -> typ -> 't -> typ -> 't -> typ -> 't -> 't * 't
val default_filter : bool -> typ -> 't -> 't
val default_compare : operator -> bool -> typ -> 't -> typ -> 't -> ('t * 't)

(** Template module with default transfer functions *)
module DefaultValueFunctions :
sig
  val filter : bool -> typ -> 't -> 't
  val backward_unop : operator -> typ -> 't -> typ -> 't -> 't
  val backward_binop : operator -> typ -> 't -> typ -> 't -> typ -> 't -> 't * 't
  val compare : operator -> bool -> typ -> 't -> typ -> 't -> ('t * 't)
  val avalue : 'r avalue_kind -> 't -> 'r option
end

(** Functor to create a value abstraction from a simplified value abstraction *)
module MakeValue(V:SIMPLIFIED_VALUE) : Value.VALUE with type t = V.t

(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)

val register_simplified_value_abstraction : (module SIMPLIFIED_VALUE) -> unit
(** Register a new simplifed value domain *)
