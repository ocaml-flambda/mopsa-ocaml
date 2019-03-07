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

(** Abstraction of values. *)

open Ast.Typ
open Ast.Operator
open Ast.Constant
open Ast.Expr
open Eq
open Id

module type VALUE =
sig

  (*==========================================================================*)
                        (** {2 Lattice structure} *)
  (*==========================================================================*)

  include Lattice.Sig.LATTICE

  val zone : Zone.zone
  (** Language zone in which the value abstraction is defined *)

  val id : t value
  val name : string
  val display : string
  val identify : 'a value -> (t, 'a) eq option

  val of_constant : typ -> constant -> t
  (** Create a singleton abstract value from a constant. *)

  (*==========================================================================*)
                          (** {2 Forward semantics} *)
  (*==========================================================================*)

  val unop : typ -> operator -> t -> t
  (** Forward evaluation of unary operators. *)

  val binop : typ -> operator -> t -> t -> t
  (** Forward evaluation of binary operators. *)

  val filter : typ -> t -> bool -> t
  (** Keep values that may represent the argument truth value *)

  (*==========================================================================*)
                          (** {2 Backward operators} *)
  (*==========================================================================*)

  val bwd_unop : typ -> operator -> t -> t -> t
  (** Backward evaluation of unary operators.
      [bwd_unop op x r] returns x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x

       it is safe, as first approximation, to implement it as the identity:
       let bwd_unop _ x _ = x
     *)

  val bwd_binop : typ -> operator -> t -> t -> t -> (t * t)
  (** Backward evaluation of binary operators.
      [bwd_binop op x y r] returns (x',y') where
      - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
      - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
      i.e., we filter the abstract values x and y knowing that, after
      applying the operation op, the result is in r

      it is safe, as first approximation, to implement it as the identity:
      let bwd_binop _ x y _ = (x,y)
  *)


  val compare : typ -> operator -> t -> t -> bool -> (t * t)
  (** Backward evaluation of boolean comparisons. [compare op x y true] returns (x',y') where:
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       [compare op x y false] is similar, but assumes that the test is false

       a safe, but not precise implementation, would be:
       compare _ _ x y = (x,y)
  *)


  (*==========================================================================*)
                             (** {2 Queries } *)
  (*==========================================================================*)

  val ask : 'a Query.query -> (expr -> t) -> 'a option

end


(*==========================================================================*)
(**                         {2 Registration} *)
(*==========================================================================*)


let values : (module VALUE) list ref = ref []

let register_value v = values := v :: !values


(*==========================================================================*)
(**                  {2 Default backward functions} *)
(*==========================================================================*)

let default_bwd_unop (t: typ) (op:operator) (x:'a) (r:'a) : 'a =
  x

let default_bwd_binop (t: typ) (op:operator) (x:'a) (y:'a) (r:'a) : ('a*'a) =
  (x, y)

let default_compare (t: typ) (op:operator) (x:'a) (y:'a) : ('a*'a) =
  (x, y)
