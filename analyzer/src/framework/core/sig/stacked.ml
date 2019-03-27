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

(** Unified stacked domain signature.

    Domains implementing the STACK signature represent parameterized
    abstractions. Their γ function, lattice operators and transfer functions
    depend on argument abstractions. Unlike classic OCaml functors, argument
    abstractions are not fixed at module creation. Instead, stacked domains
    receive a record encapsulation of the transfer functions of their argument
    abstractions at runtime. This allows sharing the arguments among other
    stacked domains (e.g, in a reduced product).

*)


open Ast
open Program
open Expr
open Stmt
open Context
open Flow
open Manager
open Eval
open Log
open Post
open Zone
open Id
open Interface



(** Unified signature of stacked abstract domains *)
module type STACK =
sig

  (** {2 Structure and identification} *)
  (** ******************************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t did
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)


  (** {2 Interface of transfer functions} *)
  (** *********************************** *)

  val exec_interface : zone interface
  (** Interface of the [exec] transfer function *)

  val eval_interface : (zone * zone) interface
  (** Interface of the eval transfer function *)


  (** {2 Special values} *)
  (** ****************** *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Predicates} *)
  (** ************** *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t * 's -> t * 's -> 's sub_man -> bool * 's * 's
  (** [subset (a1, s1) (a2, s2) man] tests whether [a1] is related to
     (or included in) [a2] and unifies the sub-tree elements [s1] and
     [s2]. *)


  (** {2 Operators} *)
  (** ************* *)

  val join: t * 's -> t * 's -> 's sub_man -> t * 's * 's
  (** [join (a1, s1) (a2, s2) man] computes an upper bound of [a1]
      and [a2] and unifies the sub-tree elements [s1] and [s2]. *)

  val meet: t * 's -> t * 's -> 's sub_man -> t * 's * 's
  (** [meet (a1, s1) (a2, s2) man] computes a lower bound of [a1] and
      [a2] and unifies the sub-tree elements [s1] and [s2]. *)

  val widen:
      uctx -> t * 's -> t * 's -> 's sub_man -> t * bool * 's * 's
  (** [widen ctx (a1, s1) (a2, s2) man] computes an upper bound of
      [a1] and [a2] that ensures stabilization of ascending chains and
      unifies the sub-tree elements [s1] and [s2]. *)

  val merge: t -> t * log -> t * log -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two post-conditions
      [post1] and [post2] using a common pre-condition [pre] after a fork-join
      trajectory in the abstraction DAG.

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions.
  *)


  (** {2 Printing} *)
  (** ************ *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t) man -> 'a flow -> 'a flow option
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> ('a, t, 's) stack_man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> ('a, t, 's) stack_man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)

end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let stacks : (module STACK) list ref = ref []

let register_stack_domain dom =
  stacks := dom :: !stacks

let find_stack name =
  List.find (fun stack ->
      let module S = (val stack : STACK) in
      compare S.name name = 0
    ) !stacks

let names () =
  List.map (fun st ->
      let module S = (val st : STACK) in
      S.name
    ) !stacks



(****************************************************************************)
(**                          {2 Stack instance}                             *)
(****************************************************************************)

module type STACKINST =
sig
end
