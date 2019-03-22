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

(** Unified domain signature.

    The signature DOMAIN is useful for domains that are not parameterized by
    other domains and that require a full accessing to the analyzer. In other
    words, their concretization function γ, their lattice operators and their
    transfer functions do not depend on other external abstractions.

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
open Eq
open Id
open Interface



(** Unified signature of an abstract domain *)
module type DOMAIN =
sig

  (** {2 Structure and identification} *)
  (** ******************************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t domain
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val identify : 'a domain -> (t, 'a) eq option
  (** Check the identity of the domain *)


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

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)


  (** {2 Operators} *)
  (** ************* *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: uctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)

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

  val exec : zone -> stmt -> ('a, t) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)

end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let domains : (module DOMAIN) list ref = ref []

let register_domain dom =
  domains := dom :: !domains

let find_domain name =
  List.find (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let names () =
  List.map (fun dom ->
      let module D = (val dom : DOMAIN) in
      D.name
    ) !domains
