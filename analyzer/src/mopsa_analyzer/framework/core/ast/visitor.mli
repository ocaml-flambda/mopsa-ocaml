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

(** Visitors for statements and expressions

    This module provides generic [map], [fold] and [fold_map] functions for
    statements/expressions that visits their structure. To support newly added
    statements and expressions, visitors need to be registered with functions
    [register_stmt_visitor] and [register_expr_visitor].

    A visitor of a statement/expression encodes its structure, composed of
    two parts: 
    - The first part is the direct sub-node of the statement/expression, 
    e.g. [x] and [e] for the statement [S_assign(x,e)]. 
    - The second part is a builder function that reconstructs the statement
    given its sub-nodes.
    
    Here is an example of registering the visitor of the assignment statement
    {[
      let () =
        register_stmt_visitor
          (fun next s ->
             match skind s with
             | S_assign(x,e) ->
               (* Sub-nodes *)
               { exprs = [x;e]; stmts = [] },
               (* Builder *)
               (function | {exprs = [x';e']} -> {s with skind = S_assign(x',e')}
                         | _ -> assert false)
             | _ -> next s)
    ]}
*)

open Mopsa_utils
open Expr
open Var
open Stmt


type parts = {
  exprs : expr list; (** sub-expressions *)
  stmts : stmt list; (** sub-statements *)
}
(** Parts of a statement/expression *)


type 'a structure = parts         (** sub-nodes *) *
                    (parts -> 'a) (** builder function *)
(** Structure of a statement/expression with its parts and builder function *)

val structure_of_expr : expr -> expr structure
(** Get the structure of an expression *)

val structure_of_stmt : stmt -> stmt structure
(** Get the structure of a statement *)

val leaf : 'a -> 'a structure
(** Visitor for leaf statements/expressions that have no sub-elements *)


(****************************************************************************)
(**                            {1 Registration}                             *)
(****************************************************************************)


type 'a visit_info = {
  compare : 'a TypeExt.compare;
  (** Comparison function for ['a] *)

  print   : 'a TypeExt.print;
  (** Pretty-printer for ['a'] *)

  visit : ('a -> 'a structure) -> 'a -> 'a structure;
  (** Visitor for ['a] *)
}
(** Registration descriptor for visitors *)

val register_expr_with_visitor : expr visit_info -> unit
(** Register an expression with its visitor *)

val register_expr_visitor :
  ((expr -> expr structure) -> expr -> expr structure) -> unit
(** Register a visitor of an expression *)

val register_stmt_with_visitor : stmt visit_info -> unit
(** Register a statement with its visitor *)

val register_stmt_visitor :
  ((stmt -> stmt structure) -> stmt -> stmt structure) -> unit
(** Register a visitor of a statement *)


(****************************************************************************)
(**                         {1 Visiting iterators}                          *)
(****************************************************************************)


(** Actions of a visiting iterator *)
type 'a visit_action =
  | Keep of 'a       (** Keep the given result *)
  | VisitParts of 'a (** Continue visiting the parts of the given result *)
  | Visit of 'a      (** Iterate the visitor on the given result *)


val map_expr :
    (expr -> expr visit_action) ->
    (stmt -> stmt visit_action) ->
    expr -> expr
(** [map_expr fe fs e] transforms the expression [e] into a new one by applying
    visitor action [fe] and [fs] on its sub-expression and sub-statements
    respectively
*)


val map_stmt :
  (expr -> expr visit_action) ->
  (stmt -> stmt visit_action) ->
  stmt -> stmt
(** Similar to [map_expr] but on statements *)

val fold_expr :
  ('a -> expr -> 'a visit_action) ->
  ('a -> stmt -> 'a visit_action) ->
  'a -> expr -> 'a
(** [fold_expr fe fs e] folds the accumulated result of visitors [fe] and
    [fs] on the structure of expression [e]  *)


val fold_stmt :
  ('a -> expr -> 'a visit_action) ->
  ('a -> stmt -> 'a visit_action) ->
  'a -> stmt -> 'a
(** Similar to [fold_expr] but on statements *)

val fold_map_expr :
  ('a -> expr -> ('a * expr) visit_action) ->
  ('a -> stmt -> ('a * stmt) visit_action) ->
  'a -> expr -> 'a * expr
(** Combination of map and fold for expressions *)

val fold_map_stmt :
  ('a -> expr -> ('a * expr) visit_action) ->
  ('a -> stmt -> ('a * stmt) visit_action) ->
  'a -> stmt -> ('a * stmt)
(** Combination of map and fold for statements *)

val exists_expr : (expr -> bool) -> (stmt -> bool) -> expr -> bool
val for_all_expr : (expr -> bool) -> (stmt -> bool) -> expr -> bool

val exists_stmt : (expr -> bool) -> (stmt -> bool) -> stmt -> bool
val for_all_stmt : (expr -> bool) -> (stmt -> bool) -> stmt -> bool

val exists_child_expr : (expr -> bool) -> (stmt -> bool) -> expr -> bool
val for_all_child_expr : (expr -> bool) -> (stmt -> bool) -> expr -> bool

val exists_child_stmt : (expr -> bool) -> (stmt -> bool) -> stmt -> bool
val for_all_child_stmt : (expr -> bool) -> (stmt -> bool) -> stmt -> bool



(****************************************************************************)
(**                         {1 Utility functions}                           *)
(****************************************************************************)

val is_leaf_expr : expr -> bool
(** Test whether an expression is a leaf expression *)

val is_atomic_expr : expr -> bool
(** Test whether an expression has no sub-statement *)

val is_atomic_stmt : stmt -> bool
(** Test whether a statement has no sub-statement *)

val expr_vars : expr -> var list
(** Get all variables present in an expression *)

val stmt_vars : stmt -> var list
(** Get all variables present in a statement *)

val is_var_in_expr : var -> expr -> bool
(** Check whether a variable appears in an expression *)

val is_var_in_stmt : var -> stmt -> bool
(** Check whether a variable appears in a statement *)

(****************************************************************************)
(**                            {1 Deprecated}                               *)
(****************************************************************************)

val fold_sub_expr :
  ('a -> expr -> 'a visit_action) ->
  ('a -> stmt -> 'a visit_action) ->
  'a -> expr -> 'a
