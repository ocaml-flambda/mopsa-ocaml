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

(** Extensible type of query kinds *)
type ('a,_) query_kind = ..

(** Queries *)
type ('a,'r) query = {
  query_kind : ('a,'r) query_kind;   (** Kind of the query *)
  query_lattice : 'a lattice option; (** Lattice of the abstract element
                                         overwhich the query was performed.
                                         This is necessary for queries that
                                         embed abstract elements in their
                                         replies. *)
}

val mk_query : ?lattice:'a lattice option -> ('a,'r) query_kind -> ('a,'r) query
(** Create a query from a kind and an optional lattice *)

val qkind : ('a,'r) query -> ('a,'r) query_kind
(** Get the kind of the query *)

val qlattice : ('a,'r) query -> 'a lattice option
(** Get the lattice of the query *)


(** {1 Registration} *)

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

val join_query : ('a,'r) query ->'r -> 'r -> 'r
(** Join two queries *)

val meet_query : ('a,'r) query -> 'r -> 'r -> 'r
(** Meet two queries *)


(** {1 Common queries} *)

type ('a, _) query_kind += Q_variables_linked_to : Ast.Expr.expr -> ('a, Ast.Var.VarSet.t) query_kind
(** Query to extract the auxiliary variables related to an expression *)
