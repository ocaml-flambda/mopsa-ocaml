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

(** Generic query mechanism for extracting information from domains. *)

open Lattice
open Context
open Ast.Var
open Ast.Addr
open Mopsa_utils

(** Extensible type of queries *)
type ('a,_) query = ..

val join_query : ?ctx:'a ctx option -> ?lattice:'a lattice option -> (('a,'r) query -> 'r -> 'r -> 'r)
(** Join two queries *)

val meet_query : ?ctx:'a ctx option -> ?lattice:'a lattice option -> (('a,'r) query -> 'r -> 'r -> 'r)
(** Meet two queries *)


(** {1 Registration} *)
(** **************** *)

(** Pool of registered queries *)
type query_pool = {
  pool_join : 'a 'r. ('a,'r) query -> 'r -> 'r -> 'r;
  pool_meet : 'a 'r. ('a,'r) query -> 'r -> 'r -> 'r;
}

(** Registraction info for new queries *)
type query_info = {
  join : 'a 'r. query_pool -> ('a,'r) query -> 'r -> 'r -> 'r;
  meet : 'a 'r. query_pool -> ('a,'r) query -> 'r -> 'r -> 'r;
}

val register_query : query_info -> unit
(** Register a new query *)

(** Pool of registered lattice queries.
    Lattice queries are queries that return elements of the global abstract
    state lattice.
    Join/meet operators are enriched with the lattice and the context so that
    we can compute join/meet over the abstract elements.
*)
type lattice_query_pool = {
  pool_join : 'a 'r. 'a ctx -> 'a lattice -> ('a,'r) query -> 'r -> 'r -> 'r;
  pool_meet : 'a 'r. 'a ctx -> 'a lattice -> ('a,'r) query -> 'r -> 'r -> 'r;
}

(** Registration info for new lattice queries *)
type lattice_query_info = {
  join : 'a 'r. lattice_query_pool -> 'a ctx -> 'a lattice -> ('a,'r) query -> 'r -> 'r -> 'r;
  meet : 'a 'r. lattice_query_pool -> 'a ctx -> 'a lattice -> ('a,'r) query -> 'r -> 'r -> 'r;
}

val register_lattice_query : lattice_query_info -> unit
(** Register a new lattice query *)



(** {1 Common queries} *)
(** ****************** *)

type ('a, _) query +=
  | Q_defined_variables : string option -> ('a,var list) query
  (** Extract the list of defined variables, in a given function call site, or in all scopes *)
  | Q_allocated_addresses : ('a,addr list) query
  (** Query to extract the list of addresses allocated in the heap *)
  | Q_variables_linked_to : Ast.Expr.expr -> ('a, VarSet.t) query
  (** Query to extract the auxiliary variables related to an expression *)
