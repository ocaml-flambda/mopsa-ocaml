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

open Core.All



(*==========================================================================*)
(**                         {2 Domain manager}                              *)
(*==========================================================================*)


(** Simplified domains are given a simplified manager providing access to
    queries on the pre-condition only 
*)
type ('a,'t) simplified_man = {
  exec : stmt -> 't;
  ask  : 'r. ('a,'r) query -> 'r;
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

  val widen: 'a ctx -> t -> t -> t
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

  val exec : stmt -> ('a,t) simplified_man -> 'a ctx -> t -> t option
  (** Computation of post-conditions *)

  val ask : ('a,'r) query -> ('a,t) simplified_man -> 'a ctx -> t -> 'r option
  (** Handler of queries *)


  (** {2 Printing} *)
  (** ************ *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)

  val pretty_print : pprinter -> expr -> t -> unit
  (** Pretty printer of expressions *)

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
