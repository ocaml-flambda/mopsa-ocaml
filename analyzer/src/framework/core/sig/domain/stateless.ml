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

(** Stateless domains are domains without an abstract element (e.g., iterators).
    Only transfer functions need to be defined.
*)

open Ast.All
open Interface
open Id
open Zone
open Lattice
open Log
open Flow
open Post
open Eval


(*==========================================================================*)
(**                         {2 Domain manager}                              *)
(*==========================================================================*)


(** Managers provide access to full analyzer, i.e. (i) the lattice
    operators of the global abstraction ['a], (ii) the transfer functions
    over ['a flow] and (iii) accessors to the domain's abstract element ['t]
    within ['a].
*)
type ('a, 't) man = ('a,'t) Intermediate.man = {
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
(**                        {2 Domain signature}                             *)
(*==========================================================================*)


module type DOMAIN =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Zoning interface *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, unit) man -> 'a flow -> 'a flow
  (** Initialization routine *)

  val exec : zone -> stmt -> ('a, unit) man -> 'a flow -> 'a post option
  (** Computation of post-conditions *)

  val eval : zone * zone -> expr -> ('a, unit) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, unit) man -> 'a flow -> 'r option
  (** Handler of queries *)

end

(** Create a full domain from a stateless domain. *)
module MakeIntermediate(D: DOMAIN) : Intermediate.DOMAIN with type t = unit =
struct

  type t = unit
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let subset _ _ = true
  let join _ _ = top
  let meet _ _ = top
  let widen _ _ _ = top
  let merge _ _ _ = top
  let print _ _ = ()

  include GenDomainId(struct
      type typ = unit
      let name = D.name
    end)

  let init = D.init

  let interface = D.interface

  let exec = D.exec
  let eval = D.eval
  let ask = D.ask
  let refine channel man flow = Channel.return flow

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

let assume = Intermediate.assume

let assume_eval = Intermediate.assume_eval

let assume_post = Intermediate.assume_post

let switch = Intermediate.switch

let switch_eval = Intermediate.switch_eval

let switch_post = Intermediate.switch_post

let exec_eval = Intermediate.exec_eval

let post_eval = Intermediate.post_eval

let post_eval_with_cleaners = Intermediate.post_eval_with_cleaners

let exec_stmt_on_all_flows = Intermediate.exec_stmt_on_all_flows

let exec_block_on_all_flows = Intermediate.exec_block_on_all_flows
