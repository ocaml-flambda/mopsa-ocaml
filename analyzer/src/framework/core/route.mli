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


(** Routes to select sub-trees of the abstraction when interpreting commands *)

open Semantic

type domain = string

(** Routes *)
type route =
  | Below    of domain   (** Sub-tree below a domain *)
  | Semantic of semantic (** Sub-tree identified by a semantic *)

val compare_route : route -> route -> int
(** Compare two routes *)

val pp_route : Format.formatter -> route -> unit
(** Print a route *)

val toplevel : route
(** Toplevel tree *)

type routing_table
(** Routing table providing the domains that are part of a route *) 

val empty_routing_table : routing_table
(** Empty routing table *)

val resolve_route : route -> routing_table -> domain list
(** Resolve a route into domains *)

val add_route : route -> domain -> routing_table -> routing_table
(** Add a route between a route and a domain *)

val add_routes : route -> domain list -> routing_table -> routing_table
(** Add a set of routing_table linking a route and a set of domains *)

val get_routes : routing_table -> route list
(** Get all routing_table in a routing table *)

val join_routing_table : routing_table -> routing_table -> routing_table
(** Join two routing_table table *)

val pp_routing_table : Format.formatter -> routing_table -> unit
(** Print a routing table *)

module DomainSet : SetExtSig.S with type elt = domain
(** Set of domains *)
