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

(** The signature DOMAIN is the unified and general-purpose interface for
    domains that are not parameterized by other domains. In other
    words, the concretization function γ, the lattice operators and the
    transfer functions do not depend on other external abstractions. Domains
    implementing this signature have full access to the analyzer: global
    abstraction, flows, zoned transfer functions of all other domains.
*)


open Ast
open Program
open Expr
open Stmt
open Context
open Flow
open Manager
open Eval
open Query
open Log
open Post
open Zone
open Id
open Interface
open Channel


(*==========================================================================*)
(**                            {2 Signature}                                *)
(*==========================================================================*)


(** Unified signature of an abstract domain *)
module type DOMAIN =
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

  val merge: t -> t * log -> t * log -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)


  (** {2 Pretty printing} *)
  (** ******************* *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t) man -> 'a flow -> 'a flow
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)


  (** {2 Reduction refinement} *)
  (** ************************ *)

  val refine : channel -> ('a,t) man -> 'a flow -> 'a flow with_channel


end



(*==========================================================================*)
(**                        {2 Low-level lifters}                            *)
(*==========================================================================*)

let lift_unop f man a = f (man.get a)

let lift_binop f man a a' = f (man.get a) (man.get a')

let lift_merge merge man pre (post1,log1) (post2,log2) =
  merge
    (man.get pre)
    (man.get post1, man.get_log log1)
    (man.get post2, man.get_log log2)

let lift_print print man fmt a = print fmt (man.get a)


(** Cast a unified signature into a low-level signature *)
module MakeLowlevelDomain(D:DOMAIN) : Lowlevel.DOMAIN with type t = D.t =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  type t = D.t

  let id = D.id

  let name = D.name

  let interface = D.interface


  (** {2 Lattice special values} *)
  (** ************************** *)

  let bottom = D.bottom

  let top = D.top


  (** {2 Lattice predicates} *)
  (** ********************** *)

  let is_bottom man a = lift_unop D.is_bottom man a

  let subset man a a' = lift_binop D.subset man a a'


  (** {2 Lattice operators} *)
  (** ********************* *)

  let join man a a' = lift_binop D.join man a a'

  let meet man a a' = lift_binop D.meet man a a'

  let widen man ctx a a' = lift_binop (D.widen ctx) man a a'

  let merge man pre (post1,log1) (post2,log2) =
    lift_merge D.merge man pre (post1,log1) (post2,log2)


  (** {2 Pretty printing} *)
  (** ******************* *)

  let print man fmt a = lift_print D.print man fmt a


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init = D.init

  let exec = D.exec

  let eval = D.eval

  let ask = D.ask


  (** {2 Reduction refinement} *)
  (** ************************ *)

  let refine = D.refine


end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let register_domain dom =
  let module D = (val dom : DOMAIN) in
  let module DL = MakeLowlevelDomain(D) in
  Lowlevel.register_domain (module DL)
