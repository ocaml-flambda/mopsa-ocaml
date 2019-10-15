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

(** Leaf domains have a simplified interface that gives access to
    their local abstractions. The global manager and the flow abstraction are
    not accessible.
*)

open Ast.All
open Lattice
open Context
open Id
open Zone
open Interface
open Token
open Channel



(*==========================================================================*)
(**                         {2 Domain manager}                              *)
(*==========================================================================*)


(** Simplified domains are given a simplified manager providing access to
    queries on the pre-condition only 
*)
type man = {
  ask : 'r. 'r Query.query -> 'r;
}




module type DOMAIN =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t id
  (** Domain identifier *)

  val name : string
  (** Domain name *)

  val zones : zone list
  (** Zones of the provided transfer functions *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


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

  val merge : t -> t * block -> t * block -> t
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

  val init : program -> t
  (** Initial abstract element *)

  val exec : uctx -> stmt -> man -> t -> t option
  (** Computation of post-conditions *)

  val ask : 'r Query.query -> t -> 'r option
  (** Handler of queries *)


  (** {2 Reduction refinement} *)
  (** ************************ *)

  val refine : channel -> t -> t with_channel


end


(** Create a full domain from a leaf. *)
module MakeIntermediate(D: DOMAIN) : Intermediate.DOMAIN with type t = D.t =
struct

  include D

  let merge pre (post1, log1) (post2, log2) =
    let block1 = Log.get_domain_block log1
    and block2 = Log.get_domain_block log2 in
    D.merge pre (post1, block1) (post2, block2)


  let init prog man flow =
    let a' = D.init prog in
    Intermediate.set_env T_cur a' man flow


  let interface = {
    iexec = {
      provides = D.zones;
      uses = [];
    };
    ieval = {
      provides = [];
      uses = [];
    }
  }

  let exec zone stmt man flow =
    match skind stmt with
    | S_assign _ | S_assume _ | S_add _ | S_remove _ | S_rename _
    | S_project _ | S_fold _ | S_expand _ | S_forget _
      ->
      let a = Intermediate.get_env T_cur man flow in

      if D.is_bottom a
      then Post.return flow |>
           Option.return

      else
        let simplified_man = {
          ask = (
            let f : type r. r Query.query -> r = fun query ->
              man.ask query flow
             in
             f
            );
        }
        in
        D.exec (Flow.get_unit_ctx flow) stmt simplified_man a |>
        Option.lift @@ fun a' ->
        Intermediate.set_env T_cur a' man flow |>
        Post.return |>
        Result.map_log (fun log ->
            man.set_log (
              man.get_log log |> Log.append stmt
            ) log
          )

    | _ -> None

  let eval zone exp man flow = None

  let ask query man flow =
    D.ask query (Intermediate.get_env T_cur man flow)

  let refine channel man flow =
    D.refine channel (Intermediate.get_env T_cur man flow) |>
    Channel.bind @@ fun a ->

    Intermediate.set_env T_cur a man flow |>
    Channel.return

end



(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let domains : (module DOMAIN) list ref = ref []


let register_domain dom =
  let module Dom = (val dom : DOMAIN) in
  let rec iter = function
    | [] -> [dom]
    | hd :: tl ->
      let module Hd = (val hd : DOMAIN) in
      if Hd.name = Dom.name
      then dom :: tl
      else hd :: iter tl
  in
  domains := iter !domains



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
