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
open Context
open Id
open Query
open Channel
open Lowlevel


module type VALUE =
sig

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  type t
  (** Type of the abstract value. *)

  val id : t id
  (** Identifier of the value domain *)

  val name : string
  (** Name of the value domain *)

  val display : string
  (** Display name used in debug messages *)

  val zones : Zone.zone list
  (** Zones in which the value abstraction is defined *)

  val mem_type : typ -> bool
  (** Types abstracted by the domain *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


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

  val widen: uctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Forward semantics} *)
  (** ********************* *)

  val constant : constant -> t
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

  val predicate : operator -> t -> bool -> t
  (** Backward evaluation of unary boolean predicates.
      [predicate op x true] returns the subset of x such that x is
      true.
      [predicate op x false] is similar, but assumes that the predicate is false
  *)

  val compare : operator -> t -> t -> bool -> (t * t)
  (** Backward evaluation of boolean comparisons. [compare op x y true] returns (x',y') where:
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       [compare op x y false] is similar, but assumes that the test is false
  *)


end



(** Lift a general-purpose signature to a low-level one *)
module MakeLowlevel(Value:VALUE) : Lowlevel.VALUE with type t = Value.t =
struct

  (* Trivial lifts *)
  type t = Value.t
  let id = Value.id
  let name = Value.name
  let display = Value.display
  let zones = Value.zones
  let mem_type = Value.mem_type
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

  let constant t c = Value.constant c

  let unop man t op a = lift_simplified_unop Value.unop man t op a

  let binop man t op a b = lift_simplified_binop Value.binop man t op a b

  let filter man a b = lift_simplified_filter Value.filter man a b


  (** {2 Backward semantics} *)
  (** ********************** *)

  let bwd_unop man t op v r = lift_simplified_bwd_unop Value.bwd_unop man t op v r

  let bwd_binop man t op v1 v2 r = lift_simplified_bwd_binop Value.bwd_binop man t op v1 v2 r

  let predicate man t op v b = lift_simplified_predicate Value.predicate man t op v b

  let compare man t op v1 v2 b = lift_simplified_compare Value.compare man t op v1 v2 b


  (** {2 Evaluation query} *)
  (** ******************** *)

  let ask man q = None


end


(*==========================================================================*)
(**                  {2 Default backward functions} *)
(*==========================================================================*)

let default_bwd_unop op x r =
  x

let default_bwd_binop op x y r =
  (x, y)

let default_predicate op x r =
  x

let default_compare op x y b =
  (x, y)


(*==========================================================================*)
(**                         {2 Registration}                                *)
(*==========================================================================*)

let values : (module VALUE) list ref = ref []

let register_value v = values := v :: !values

let find_value name =
  List.find (fun v ->
      let module V = (val v : VALUE) in
      compare V.name name = 0
    ) !values

let mem_value name =
  List.exists (fun v ->
      let module V = (val v : VALUE) in
      compare V.name name = 0
    ) !values

let names () =
  List.map (fun v ->
      let module V = (val v : VALUE) in
      V.name
    ) !values
