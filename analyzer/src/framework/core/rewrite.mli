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

(** Rewrite - expression transformations returned by domains evaluations *)

open Lattice
open Flow
open Ast.Stmt
open Ast.Expr
open Cases
open Semantic

(****************************************************************************)
(**                     {1 Transformed expressions}                         *)
(****************************************************************************)

(** Transformed expressions *)
type expr_rewrite =
  | Return  of expr (** Final transformation *)
  | Forward of expr * semantic (** Dependent transformation in a given semantic *)

val compare_expr_rewrite : expr_rewrite -> expr_rewrite -> int
(** Compare two transformed expressions *)

val pp_expr_rewrite : Format.formatter -> expr_rewrite -> unit
  
val get_expr : expr_rewrite -> expr

val get_semantic_opt : expr_rewrite -> semantic option


(****************************************************************************)
(**                  {1 Disjunctive Expression rewrite}                     *)
(****************************************************************************)

type 'a rewrite = ('a,expr_rewrite) cases
(** Disjunctive expression rewrite *)

val reval_singleton : ?cleaners:stmt list -> expr -> 'a flow -> 'a rewrite
(** Re-evaluate a singleton expression *)

val return_singleton : ?cleaners:block -> expr -> 'a flow -> 'a rewrite
(** Return an expression as a final transformation *)

val forward_singleton : ?cleaners:block -> expr -> semantic:semantic-> 'a flow -> 'a rewrite
(** Forward a transformation to domains implementing a given semantic *)

val empty_singleton : 'a flow -> 'a rewrite
(** Transformation to an empty expression *)

val reval_eval : 'a Eval.eval -> 'a rewrite
(** Re-evaluate all cases in an evaluation *)

val return_eval : 'a Eval.eval -> 'a rewrite
(** Return all cases in an evaluation as a final transformation *)

val forward_eval : 'a Eval.eval -> semantic:semantic -> 'a rewrite
(** Forward all cases in an evaluation to domains implementing a given semantic *)

val join : 'a rewrite -> 'a rewrite -> 'a rewrite
(** Join two rewrites *)

val join_list : empty:(unit -> 'a rewrite) -> 'a rewrite list -> 'a rewrite
(** Join a list of rewrites *)

val meet : 'a rewrite -> 'a rewrite -> 'a rewrite
(** Intersect two rewrites *)

val meet_list : empty:(unit -> 'a rewrite) -> 'a rewrite list -> 'a rewrite
(** Intersect a list of rewrites *)

val print : Format.formatter -> 'a rewrite -> unit
(** Print a rewrite *)

val remove_duplicates : 'a lattice -> 'a rewrite -> 'a rewrite
(** Remove duplicate cases in a rewrite *)
