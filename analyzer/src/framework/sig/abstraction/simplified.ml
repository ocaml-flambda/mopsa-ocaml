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
open Core.All



(*==========================================================================*)
(**                         {2 Domain manager}                              *)
(*==========================================================================*)


(** Simplified domains are given a simplified manager providing access to
    queries on the pre-condition only 
*)
type 'a simplified_man = {
  exec : stmt -> 'a;
  ask  : 'r. 'r query -> 'r;
}




module type SIMPLIFIED =
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

  val exec : stmt -> t simplified_man -> uctx -> t -> t option
  (** Computation of post-conditions *)

  val ask : 'r query -> t simplified_man -> t -> 'r option
  (** Handler of queries *)


end


(** Create a standard domain from a simplified one. *)
module MakeDomain(D: SIMPLIFIED) : Domain.DOMAIN with type t = D.t =
struct

  include D

  let merge pre (post1, log1) (post2, log2) =
    let stmts1 = Log.get_log_stmts log1
    and stmts2 = Log.get_log_stmts log2 in
    D.merge pre (post1, stmts1) (post2, stmts2)


  let init prog man flow =
    let a' = D.init prog in
    set_env T_cur a' man flow


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

  let alarms = []

  let simplified_man man flow = {
    exec = (fun stmt -> man.Core.Manager.exec stmt flow |>
                        get_env T_cur man
           );
    ask = (fun query -> man.Core.Manager.ask query flow);
  }

  let exec zone stmt man flow =
    let a = get_env T_cur man flow in
    if D.is_bottom a
    then
      Post.return flow |>
      OptionExt.return
    else
      D.exec stmt (simplified_man man flow) (Flow.get_unit_ctx flow) a |>
      OptionExt.lift @@ fun a' ->
      set_env T_cur a' man flow |>
      Post.return |>
      Cases.map_log (fun log ->
          man.set_log (
            man.get_log log |> Log.add_stmt_to_log stmt
          ) log
        )

  let eval zone exp man flow = None

  let ask query man flow =
    D.ask query (simplified_man man flow) (get_env T_cur man flow)

end



(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let domains : (module SIMPLIFIED) list ref = ref []


let register_simplified_domain dom =
  let module Dom = (val dom : SIMPLIFIED) in
  let rec iter = function
    | [] -> [dom]
    | hd :: tl ->
      let module Hd = (val hd : SIMPLIFIED) in
      if Hd.name = Dom.name
      then dom :: tl
      else hd :: iter tl
  in
  domains := iter !domains



let find_simplified_domain name =
  List.find (fun dom ->
      let module D = (val dom : SIMPLIFIED) in
      compare D.name name = 0
    ) !domains


let mem_simplified_domain name =
  List.exists (fun dom ->
      let module D = (val dom : SIMPLIFIED) in
      compare D.name name = 0
    ) !domains


let simplified_domain_names () =
  List.map (fun dom ->
      let module D = (val dom : SIMPLIFIED) in
      D.name
    ) !domains
