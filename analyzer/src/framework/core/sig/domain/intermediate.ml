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
open Lattice
open Eval
open Query
open Log
open Post
open Zone
open Id
open Interface
open Channel


(*==========================================================================*)
(**                         {2 Domain manager}                              *)
(*==========================================================================*)


(** Managers provide access to full analyzer, i.e. (i) the lattice
    operators of the global abstraction ['a], (ii) the transfer functions
    over ['a flow] and (iii) accessors to the domain's abstract element ['t]
    within ['a].
*)
type ('a, 't) man = ('a,'t) Lowlevel.man = {
  (* Lattice operators over global abstract elements ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Analyzer transfer functions *)
  post : ?zone:zone -> stmt -> 'a flow -> 'a post;
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> (expr, 'a) eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;

  (** Accessors to the domain's merging logs *)
  get_log : log -> log;
  set_log : log -> log -> log;
}




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

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

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

  val merge: uctx -> t -> t * log -> t * log -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)


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

  val refine : channel -> ('a,t) man -> 'a flow -> 'a flow with_channel
  (** Refinement using reduction channel *)

end



(*==========================================================================*)
(**                        {2 Low-level lifters}                            *)
(*==========================================================================*)

let lift_unop f man ctx a = f (man.get a)

let lift_binop f man ctx a a' = f (man.get a) (man.get a')

let lift_widen f man ctx a a' = f ctx (man.get a) (man.get a')


(** Cast a unified signature into a low-level signature *)
module MakeLowlevelDomain(D:DOMAIN) : Lowlevel.DOMAIN with type t = D.t =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  type t = D.t

  let id = D.id

  let name = D.name

  let interface = D.interface

  let bottom = D.bottom

  let top = D.top

  let is_bottom = D.is_bottom

  let print = D.print


  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset man ctx a a' = lift_binop D.subset man ctx a a'

  let join man ctx a a' = lift_binop D.join man ctx a a'

  let meet man ctx a a' = lift_binop D.meet man ctx a a'

  let widen man ctx a a' = lift_widen D.widen man ctx a a'

  let merge = D.merge


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init = D.init

  let exec zone stmt man flow =
    D.exec zone stmt man flow |>
    Option.lift @@ Lowlevel.log_post_stmt stmt man

  let eval = D.eval

  let ask = D.ask

  let refine = D.refine


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

let mem_domain name =
  List.exists (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let names () =
  List.map (fun dom ->
      let module D = (val dom : DOMAIN) in
      D.name
    ) !domains


(*==========================================================================*)
(**                        {2 Utility functions}                            *)
(*==========================================================================*)

let log_post_stmt = Lowlevel.log_post_stmt

let set_domain_env = Lowlevel.set_domain_env

let get_domain_env = Lowlevel.get_domain_env

let map_domain_env = Lowlevel.map_domain_env

let mem_domain_env = Lowlevel.mem_domain_env

let assume = Lowlevel.assume

let assume_eval = Lowlevel.assume_eval

let assume_post = Lowlevel.assume_post

let switch = Lowlevel.switch

let switch_eval = Lowlevel.switch_eval

let switch_post = Lowlevel.switch_post

let exec_eval = Lowlevel.exec_eval

let post_eval = Lowlevel.post_eval

let post_eval_with_cleaners = Lowlevel.post_eval_with_cleaners

let exec_stmt_on_all_flows = Lowlevel.exec_stmt_on_all_flows

let exec_block_on_all_flows = Lowlevel.exec_block_on_all_flows
