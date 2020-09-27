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

(** Simplified domains

    The signature [SIMPLIFIED] provides a minimal interface to implement
    abstract domains that don't require communication with other domains.
    This is particularly useful for abstractions that are at the leaves of
    the abstraction DAG.

    Lattice operations and transfer functions operate on the abstract element
    of the domain without being able to access to the top-level abstraction.
    The manager is therefore not accessible, only a simplified version is
    provided that can be used to perform queries on the pre-state.
*)

open Core.All



(*==========================================================================*)
(**                        {1 Simplified manager}                           *)
(*==========================================================================*)


(** Simplified managers provide access to the pre-state and can be used to
    perform queries or execute statements. 
*)
type ('a,'t) simplified_man = {
  exec : stmt -> 't;          (** execute a statement on the pre-state *)
  ask  : 'r. ('a,'r) query -> 'r;  (** ask a query on the pre-state *)
}


(*==========================================================================*)
(**                           {1 Signature}                                 *)
(*==========================================================================*)


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
  (** Pretty printer of expression value *)

end


(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

val register_simplified_domain : (module SIMPLIFIED) -> unit
(** Register a new simplified domain *)


val find_simplified_domain : string -> (module SIMPLIFIED)
(** Find a simplified domain by its name. Raise [Not_found] if no domain is found *)

val mem_simplified_domain : string -> bool
(** [mem_simplified_domain name] checks whether a simplified domain with name
    [name] is registered *)
 
val simplified_domain_names : unit -> string list
(** Return the names of registered simplified domains *) 
