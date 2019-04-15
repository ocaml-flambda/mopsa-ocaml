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
open Channel


module type VALUE =
sig

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  type t
  (** Type of the abstract value. *)

  val id : t value
  (** Identifier of the value domain *)

  val name : string
  (** Name of the value domain *)

  val display : string
  (** Display name used in debug messages *)

  val zones : Zone.zone list
  (** Zones in which the value abstraction is defined *)

  val types : typ list
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

  val of_constant : typ -> constant -> t
  (** Create a singleton abstract value from a constant. *)

  val unop : typ -> operator -> t -> t
  (** Forward evaluation of unary operators. *)

  val binop : typ -> operator -> t -> t -> t
  (** Forward evaluation of binary operators. *)

  val filter : t -> bool -> t
  (** Keep values that may represent the argument truth value *)


  (** {2 Backward semantics} *)
  (** ********************** *)

  val bwd_unop : typ -> operator -> t -> t -> t
  (** Backward evaluation of unary operators.
      [bwd_unop op x r] returns x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)

  val bwd_binop : typ -> operator -> t -> t -> t -> (t * t)
  (** Backward evaluation of binary operators.
      [bwd_binop op x y r] returns (x',y') where
      - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
      - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
      i.e., we filter the abstract values x and y knowing that, after
      applying the operation op, the result is in r
  *)


  val compare : typ -> operator -> t -> t -> bool -> (t * t)
  (** Backward evaluation of boolean comparisons. [compare op x y true] returns (x',y') where:
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       [compare op x y false] is similar, but assumes that the test is false
  *)


  (** {2 Query handler} *)
  (** ***************** *)

  val ask : 'r query -> (expr -> t) -> 'r option


  (** {2 Reduction refinement} *)
  (** ************************ *)

  val refine : channel -> t -> t with_channel



end


(*==========================================================================*)
(**                      {2 Low-level lifters}                              *)
(*==========================================================================*)

let lift_unop unop (man:('a,'t) vman) (t:typ) (op:operator) (a:'a) : 't = unop t op (man.vget a)

let lift_binop binop man t op a b = binop t op (man.vget a) (man.vget b)

let lift_filter filter man v b =  filter (man.vget v) b

let lift_bwd_unop bwd_unop man t op v r = bwd_unop t op (man.vget v) (man.vget r)

let lift_bwd_binop bwd_binop man t op a b r = bwd_binop t op (man.vget a) (man.vget b) (man.vget r)

let lift_compare compare man t op a b r = compare t op (man.vget a) (man.vget b) r

let lift_ask ask man q = ask q (fun e -> man.veval e |> man.vget)

let lift_refine refine man channel v =
  refine channel (man.vget v) |>
  Channel.bind @@ fun r ->

  man.vset r v |>
  Channel.return

let leaf_cast :type t s. ('a,t) vman -> t value -> s value -> 'a -> s option =
  fun man id1 id2 a ->
    match Id.veq id1 id2 with
    | Some Eq.Eq -> Some (man.vget a)
    | None -> None

(** Lift a general-purpose signature to a low-level one *)
module MakeLowlevel(Value:VALUE) : Lowlevel.VALUE with type t = Value.t =
struct

  (* Trivial lifts *)
  type t = Value.t
  let id = Value.id
  let name = Value.name
  let display = Value.display
  let zones = Value.zones
  let types = Value.types
  let bottom = Value.bottom
  let top = Value.top
  let is_bottom = Value.is_bottom
  let subset = Value.subset
  let join = Value.join
  let meet = Value.meet
  let widen = Value.widen
  let print = Value.print

  let cast man id a = leaf_cast man Value.id id a


  (** {2 Forward semantics} *)
  (** ********************* *)

  let of_constant = Value.of_constant

  let unop man t op a = lift_unop Value.unop man t op a

  let binop man t op a b = lift_binop Value.binop man t op a b

  let filter man a b = lift_filter Value.filter man a b


  (** {2 Backward semantics} *)
  (** ********************** *)

  let bwd_unop man t op v r = lift_bwd_unop Value.bwd_unop man t op v r

  let bwd_binop man t op v1 v2 r = lift_bwd_binop Value.bwd_binop man t op v1 v2 r

  let compare man t op v1 v2 b = lift_compare Value.compare man t op v1 v2 b


  (** {2 Evaluation query} *)
  (** ******************** *)

  let ask man q = lift_ask Value.ask man q


  (** {2 Reduction refinement} *)
  (** ************************ *)

  let refine man channel a =
    lift_refine Value.refine man channel a

end


(*==========================================================================*)
(**                  {2 Default backward functions} *)
(*==========================================================================*)

let default_bwd_unop t op x r =
  x

let default_bwd_binop t op x y r =
  (x, y)

let default_compare t op x y b =
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
