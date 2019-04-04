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

(** Low level signature of stacked domains. Similar to the general-purpose
    stacked domain signature, except that lattice operators are defined on
    the global abstraction.
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


(** Low level signature of stacked abstract domains *)
module type STACK =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t domain
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Interface of the domain *)


  (** {2 Lattice special values} *)
  (** ************************** *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Pretty printing} *)
  (** ******************* *)

  val print: ('a,t) man -> Format.formatter -> 'a -> unit
  (** Printer of an abstract element. *)


  (** {2 Lattice predicates} *)
  (** ********************** *)

  val is_bottom: ('a,t) man -> ('a,'s) man -> 'a -> bool
  (** [is_bottom man a] tests whether [a] is bottom or not. *)

  val subset: ('a,t) man -> ('a,'s) man -> 'a -> 'a -> bool * 'a * 'a


  (** {2 Lattice operators} *)
  (** ********************* *)

  val join: ('a,t) man -> ('a,'s) man -> 'a -> 'a -> t * 'a * 'a

  val meet: ('a,t) man -> ('a,'s) man -> 'a -> 'a -> t * 'a * 'a

  val widen: ('a,t) man -> ('a,'s) man -> uctx -> 'a -> 'a -> t * 'a * 'a * bool

  val merge: ('a,t) man -> 'a -> 'a * log -> 'a * log -> t
  (** [merge man pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t) man -> 'a flow -> 'a flow option
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> ('a,'s) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)

end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let stacks : (module STACK) list ref = ref []

let register_stack dom =
  stacks := dom :: !stacks

let find_stack name =
  List.find (fun dom ->
      let module S = (val dom : STACK) in
      compare S.name name = 0
    ) !stacks

let names () =
  List.map (fun dom ->
      let module S = (val dom : STACK) in
      S.name
    ) !stacks
