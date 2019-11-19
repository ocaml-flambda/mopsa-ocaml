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

(** Low level signature of domains. Similar to the general-purpose domain
    signature, except that lattice operators are defined on the global
    abstraction.
*)


open Ast
open Program
open Expr
open Stmt
open Context
open Token
open Lattice
open Flow
open Eval
open Query
open Post
open Zone
open Id
open Interface
open Log
open Manager
open Channel
open Alarm


(*==========================================================================*)
(**                         {2 Domain manager}                              *)
(*==========================================================================*)


type ('a, 't) man = ('a,'t) Manager.man = {
  (* Lattice operators over global abstract elements ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Analyzer transfer functions *)
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  post : ?zone:zone -> stmt -> 'a flow -> 'a post;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> 'a eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;

  (** Accessors to the domain's merging logs *)
  get_log : log -> log;
  set_log : log -> log -> log;
}



(*==========================================================================*)
(**                            {2 Signature}                                *)
(*==========================================================================*)


(** Low-level signature of an abstract domain *)
module type DOMAIN =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t id
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Interface of the domain *)

  val alarms : alarm_class list
  (** List of alarm classes detected by the domain *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val is_bottom: t -> bool
  (** [is_bottom man a] tests whether [a] is bottom or not. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val subset: ('a,t) man -> uctx -> 'a -> 'a -> bool
  (** [subset man a1 a2] provides a partial order relation over
      elements of the domain by testing whether [a1] is related to (or
     included in) [a2]. *)

  val join: ('a,t) man -> uctx -> 'a -> 'a -> t
  (** [join man a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: ('a,t) man -> uctx -> 'a -> 'a -> t
  (** [meet man a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: ('a,t) man -> uctx -> 'a -> 'a -> t
  (** [widen man ctx a1 a2] computes an upper bound of [a1] and [a2] that
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


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t) man -> 'a flow -> 'a flow
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a flow -> 'a eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)

  val refine : channel -> ('a,t) man -> 'a flow -> 'a flow with_channel
  (** Refinement using reduction channels *)

end



(*==========================================================================*)
(**                        {2 Utility functions}                            *)
(*==========================================================================*)


let set_env = Manager.set_env

let get_env = Manager.get_env

let map_env = Manager.map_env

let assume = Manager.assume

let assume_flow = Manager.assume_flow

let switch = Manager.switch

let exec_stmt_on_all_flows = Manager.exec_stmt_on_all_flows

let exec_block_on_all_flows = Manager.exec_block_on_all_flows

let post_to_flow = Manager.post_to_flow


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


(** Auto-logger lifter used when registering a domain *)
module AutoLogger(D:DOMAIN) : DOMAIN with type t = D.t =
struct
  include D
  let exec zone stmt man flow =
    D.exec zone stmt man flow |>
    Option.lift @@ fun res ->
    Result.map_log (fun log ->
        man.set_log (
          man.get_log log |> Log.append stmt
        ) log
      ) res
end

let domains : (module DOMAIN) list ref = ref []

let register_domain dom =
  let module D = (val dom : DOMAIN) in
  domains := (module AutoLogger(D)) :: !domains

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
