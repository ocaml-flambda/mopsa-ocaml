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

(** Generic visitors for statements and expressions. *)

open Expr
open Var
open Stmt


type parts = {
  exprs : expr list; (** child expressions *)
  stmts : stmt list; (** child statements *)
}


type 'a structure = parts * (parts -> 'a)

val leaf : 'a -> 'a structure


type 'a vinfo = {
  compare : 'a TypeExt.compare;
  print   : 'a TypeExt.print;
  visit : ('a -> 'a structure) -> 'a -> 'a structure;
}

val register_expr_with_visitor : expr vinfo -> unit

val register_expr_visitor : ((expr -> expr structure) -> expr -> expr structure) -> unit

val register_stmt_with_visitor : stmt vinfo -> unit

val register_stmt_visitor : ((stmt -> stmt structure) -> stmt -> stmt structure) -> unit

val split_expr : expr ->expr structure

val split_stmt : stmt -> stmt structure

val is_leaf_expr : expr -> bool

val is_leaf_stmt : stmt -> bool

(** Kinds of returned actions by a visitor *)
type 'a action =
  | Keep of 'a       (** Keep the result *)
  | VisitParts of 'a (** Continue visiting the parts of the result *)
  | Visit of 'a      (** Iterate the visitor on the result *)


val map_expr :
    (expr -> expr action) ->
    (stmt -> stmt action) ->
    expr -> expr
(** [map_expr fe fs e] transforms the exprression [e] into a new one,
    by splitting [fe e] into its sub-parts, applying [map_expr fe fs] and
    [map_stmt fe fs] on them, and finally gathering the results with
    the builder of [fe e].
*)


val map_stmt :
  (expr -> expr action) ->
  (stmt -> stmt action) ->
  stmt -> stmt
(** [map_stmt fe fs s] same as [map_expr] but on statements. *)

val fold_expr :
  ('a -> expr -> 'a action) ->
  ('a -> stmt -> 'a action) ->
  'a -> expr -> 'a
(** Folding function for expressions  *)


val fold_stmt :
  ('a -> expr -> 'a action) ->
  ('a -> stmt -> 'a action) ->
  'a -> stmt -> 'a
(** Folding function for statements *)

val fold_map_expr :
  ('a -> expr -> ('a * expr) action) ->
  ('a -> stmt -> ('a * stmt) action) ->
  'a -> expr -> 'a * expr
(** Combination of map and fold for expressions *)

val fold_map_stmt :
  ('a -> expr -> ('a * expr) action) ->
  ('a -> stmt -> ('a * stmt) action) ->
  'a -> stmt -> ('a * stmt)
(** Combination of map and fold for statements *)

val expr_vars : expr -> var list
(** Extract variables from an expression *)

val stmt_vars : stmt -> var list
(** Extract variables from a statement *)
