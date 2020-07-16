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
open Cases
open Semantic

type expr_rewrite =
  | Return  of expr
  | Forward of expr * semantic

type 'a rewrite = ('a,expr_rewrite) cases

val compare_expr_rewrite : expr_rewrite -> expr_rewrite -> int

val pp_expr_rewrite : Format.formatter -> expr_rewrite -> unit

val get_expr : expr_rewrite -> expr

val get_semantic_opt : expr_rewrite -> semantic option

val singleton : ?cleaners:stmt list -> expr_rewrite -> 'a flow -> 'a rewrite

val return_singleton : ?cleaners:block -> expr -> 'a flow -> 'a rewrite

val forward_singleton : ?cleaners:block -> expr -> semantic:semantic-> 'a flow -> 'a rewrite

val empty_singleton : 'a flow -> 'a rewrite

val return_eval : 'a Eval.eval -> 'a rewrite

val forward_eval : 'a Eval.eval -> semantic:semantic -> 'a rewrite

val join_list : empty:(unit -> 'a rewrite) -> 'a rewrite list -> 'a rewrite

val meet : 'a rewrite -> 'a rewrite -> 'a rewrite

val meet_list : empty:(unit -> 'a rewrite) -> 'a rewrite list -> 'a rewrite

val print : Format.formatter -> 'a rewrite -> unit

val remove_duplicates : 'a lattice -> 'a rewrite -> 'a rewrite
