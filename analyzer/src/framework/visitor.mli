(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Generic visitors for statements and expressions.

   To allow visiting the extensible types of statements and expressions,
   the developper should define its structure giving:
   - its parts consisting of sub-expressions and sub-statements,
   - its builder that can reconstitute the statement/expression from its parts.
*)

open Ast

(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

type parts = {
  exprs : expr list; (** child expressions *)
  stmts : stmt list; (** child statements *)
}
(** Parts are the direct sub-elements of an AST node *)

type 'a structure = parts * (parts -> 'a)
(** A structure of an extensible type ['a] is a tuple composed of two elements:
    the parts and a builder function.
*)

val split_stmt : stmt -> stmt structure
val split_expr : expr -> expr structure

(*==========================================================================*)
                        (** {2 Visitors chains} *)
(*==========================================================================*)

type 'a chain = ('a -> 'a structure) ref
(** A chain stores the head of the visitors, each of which will call
    the next one in the chain when encountering an AST node that
    can not be handled.
*)


(** Leaf nodes are identity composer *)
val leaf : 'a -> 'a structure

(** To register a visitor of new expressions, [register_exp_visitor]
    should be called with a function having two arguments:
       - a default visitor to use for unknown expressions
       - the expression to visit
*)
val register_expr_visitor :
  ((expr -> expr structure) -> expr -> expr structure) -> unit

val register_stmt_visitor :
    ((stmt -> stmt structure) -> stmt -> stmt structure) -> unit


(*==========================================================================*)
                        (** {2 Iterators} *)
(*==========================================================================*)


val map_expr :
    (expr -> expr) ->
    (stmt -> stmt) ->
    expr -> expr
(** [map_expr fe fs e] transforms the exprression [e] into a new one,
    by splitting [fe e] into its sub-parts, applying [map_expr fe fs] and
    [map_stmt fe fs] on them, and finally gathering the results with
    the builder of [fe e].
*)


val map_stmt :
  (expr -> expr) ->
  (stmt -> stmt) ->
  stmt -> stmt
(** [map_stmt fe fs s] same as [map_expr] but on statements. *)

val fold_expr :
  ('a -> expr -> 'a) ->
  ('a -> stmt -> 'a) ->
  'a -> expr -> 'a
(** Folding function for expressions  *)


val fold_stmt :
  ('a -> expr -> 'a) ->
  ('a -> stmt -> 'a) ->
  'a -> stmt -> 'a
(** Folding function for statements *)

val fold_map_expr :
  ('a -> expr -> 'a * expr) ->
  ('a -> stmt -> 'a * stmt) ->
  'a -> expr -> 'a * expr
(** Combination of map and fold for expressions *)

val fold_map_stmt :
  ('a -> expr -> 'a * expr) ->
  ('a -> stmt -> 'a * stmt) ->
  'a -> stmt -> 'a * stmt
(** Combination of map and fold for statements *)

val expr_vars : expr -> var list
(** Extract variables from an expression *)

val stmt_vars : stmt -> var list
(** Extract variables from a statement *)
