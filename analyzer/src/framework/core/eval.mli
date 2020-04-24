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

(** Eval - abstract evaluations of expressions *)

open Lattice
open Flow
open Ast.Stmt
open Ast.Expr
open Context
open Cases
open Log

type 'a eval = ('a, expr) cases

val return : ?cleaners:block -> ?log:log -> expr option -> 'a flow -> 'a eval

val singleton : ?cleaners:block -> expr -> 'a flow -> 'a eval

val empty_singleton : 'a flow -> 'a eval

val join : 'a eval  -> 'a eval  -> 'a eval

val meet : 'a eval  -> 'a eval  -> 'a eval

val join_list : empty:(unit -> 'a eval) -> 'a eval list -> 'a eval

val meet_list : empty:(unit -> 'a eval) -> 'a eval list -> 'a eval

val print: Format.formatter -> 'a eval -> unit

val add_cleaners : stmt list -> 'a eval  -> 'a eval

val get_ctx : 'a eval -> 'a ctx

val set_ctx : 'a ctx -> 'a eval -> 'a eval

val get_callstack : 'a eval -> Callstack.cs

val copy_ctx : 'a eval -> 'a eval -> 'a eval

val bind : (expr -> 'a flow -> ('a,'r) cases) -> 'a eval -> ('a,'r) cases

val apply : (expr -> 'a flow -> 'b) -> ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a eval -> 'b

val map : (expr -> expr) -> 'a eval -> 'a eval

val remove_duplicates : 'a lattice -> 'a eval -> 'a eval

val cardinal : 'a eval -> int
