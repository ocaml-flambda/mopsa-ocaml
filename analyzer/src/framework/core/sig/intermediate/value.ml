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

(** General-purpose signature of a value abstraction. *)

open Ast.All
open Manager
open Context
open Id
open Query


module type VALUE =
sig

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  type t
  (** Type of an abstract value. *)

  val id : t value
  (** Identifier of the value abstraction *)

  val name : string
  (** Name of the value abstraction *)

  val display : string
  (** Debug display name used by the non-relational lifter *)

  val zone : Zone.zone
  (** Language zone in which the value abstraction is defined *)

  val accept_expr : expr -> bool
  (** Filter expressions that can be handled by the value abstraction *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Lattice predicates} *)
  (** ********************** *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: uctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Pretty printing} *)
  (** ******************* *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Forward semantics} *)
  (** ********************* *)

  val of_constant : constant -> t
  (** Create a singleton abstract value from a constant. *)

  val unop : operator -> t -> t
  (** Forward evaluation of unary operators. *)

  val binop : operator -> t -> t -> t
  (** Forward evaluation of binary operators. *)

  val filter : t -> bool -> t
  (** Keep values that may represent the argument truth value *)


  (** {2 Backward semantics} *)
  (** ********************** *)

  val bwd_unop : operator -> t -> t -> t
  (** Backward evaluation of unary operators.
      [bwd_unop op x r] returns x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)

  val bwd_binop : operator -> t -> t -> t -> (t * t)
  (** Backward evaluation of binary operators.
      [bwd_binop op x y r] returns (x',y') where
      - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
      - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
      i.e., we filter the abstract values x and y knowing that, after
      applying the operation op, the result is in r
  *)


  val compare : operator -> t -> t -> bool -> (t * t)
  (** Backward evaluation of boolean comparisons. [compare op x y true] returns (x',y') where:
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       [compare op x y false] is similar, but assumes that the test is false
  *)


  (** {2 Evaluation query} *)
  (** ******************** *)

  module EvalQuery : Query.ArgQuery
    with type arg = expr
    and type ret = t

end


(*==========================================================================*)
(**                       {2 Low-level lifter}                              *)
(*==========================================================================*)

(** Lift a general-purpose signature to a low-level one *)
module MakeLowlevel(Value:VALUE) : Lowlevel.Value.VALUE with type t = Value.t =
struct

  (* Trivial lifts *)
  type t = Value.t
  let id = Value.id
  let name = Value.name
  let display = Value.display
  let zone = Value.zone
  let accept_expr = Value.accept_expr
  let bottom = Value.bottom
  let top = Value.top
  let is_bottom = Value.is_bottom
  let subset = Value.subset
  let join = Value.join
  let meet = Value.meet
  let widen = Value.widen
  let print = Value.print


  (** {2 Forward semantics} *)
  (** ********************* *)

  let of_constant const =
    Value.of_constant const

  let unop man op v =
    Value.unop op (man.vget v)

  let binop man op v1 v2 =
    Value.binop op (man.vget v1) (man.vget v2)

  let filter man v b =
    Value.filter (man.vget v) b


  (** {2 Backward semantics} *)
  (** ********************** *)

  let bwd_unop man op v r =
    Value.bwd_unop op (man.vget v) (man.vget r)

  let bwd_binop man op v1 v2 r =
    Value.bwd_binop op (man.vget v1) (man.vget v1) (man.vget r)

  let compare man op v1 v2 b =
    Value.compare op (man.vget v1) (man.vget v1) b


  (** {2 Evaluation query} *)
  (** ******************** *)

  module EvalQuery = Value.EvalQuery

end


(*==========================================================================*)
(**                         {2 Registration}                                *)
(*==========================================================================*)

let register_value v =
  let module V = (val v : VALUE) in
  let module VL = MakeLowlevel(V) in
  Lowlevel.Value.register_value (module VL)


(*==========================================================================*)
(**                  {2 Default backward functions} *)
(*==========================================================================*)

let default_bwd_unop op x r =
  x

let default_bwd_binop op x y r =
  (x, y)

let default_compare op x y b =
  (x, y)
