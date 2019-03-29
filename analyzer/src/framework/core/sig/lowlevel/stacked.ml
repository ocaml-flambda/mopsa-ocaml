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
    depend on argument abstractions. Stacks extend classic OCaml functors by
    allowing shared argument abstractions.

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
open Abstraction


(** Unified signature of stacked abstract domains *)
module type STACK =
sig

  (** {2 Declaration header} *)
  (** ********************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t did
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Interface of the domain *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val merge: ('a,t) man -> 'a * log -> 'a * log -> 'a
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)




  val subset: ('a,t) man -> ('a,'s) man -> 'a -> 'a -> bool * 'a * 'a
  (** [subset (a1, s1) (a2, s2)] tests whether [a1] is related to
      (or included in) [a2] and unifies the sub-tree elements [s1] and
      [s2]. *)


  val join: ('a,t) man -> ('a,'s) man -> 'a -> 'a -> 'a * 'a
  (** [join (a1, s1) (a2, s2)] computes an upper bound of [a1]
      and [a2] and unifies the sub-tree elements [s1] and [s2]. *)

  val meet: ('a,t) man -> ('a,'s) man -> 'a -> 'a -> 'a * 'a
  (** [meet (a1, s1) (a2, s2)] computes a lower bound of [a1] and
      [a2] and unifies the sub-tree elements [s1] and [s2]. *)

  val widen:
    uctx -> ('a,t) man -> ('a,'s) man -> 'a -> 'a -> 'a * 'a * bool
  (** [widen ctx (a1, s1) (a2, s2) man] computes an upper bound of
      [a1] and [a2] that ensures stabilization of ascending chains and
      unifies the sub-tree elements [s1] and [s2]. *)


  val init : program -> ('a, t) man -> ('a,'s) man -> 'a flow -> 'a flow option
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> ('a,'s) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> ('a,'s) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> ('a,'s) man -> 'a flow -> 'r option
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
