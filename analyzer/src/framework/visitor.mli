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


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

type parts = {
  exprs : Ast.expr list; (** child expressions *)
  stmts : Ast.stmt list; (** child statements *)
}
(** Parts are the direct sub-elements of an AST node *)

type 'a structure
(** A structure of an extensible type ['a] is a tuple composed of two elements:
    the parts and a builder function.
*)

(*==========================================================================*)
                        (** {2 Visitors chains} *)
(*==========================================================================*)

type 'a chain = ('a -> 'a structure) ref
(** A chain stores the head of the visitors, each of which will call
    the next one in the chain when encountering an AST node that
    can not be handled.
*)


(** Leaf nodes are identity composer *)
val leaf (x: 'a) : 'a structure

(** To register a visitor of new expressions, [register_exp_visitor]
    should be called with a function having two arguments:
       - a default visitor to use for unknown expressions
       - the expression to visit
*)
val register_expr_visitor :
  ((Ast.expr -> Ast.expr structure) -> Ast.expr -> Ast.expr structure) -> unit

val register_stmt_visitor :
    ((Ast.stmt -> Ast.stmt structure) -> Ast.stmt -> Ast.stmt structure) -> unit


(*==========================================================================*)
                        (** {2 Iterators} *)
(*==========================================================================*)


val map_expr :
    (Ast.expr -> Ast.expr) ->
    (Ast.stmt -> Ast.stmt) ->
    Ast.expr -> Ast.expr
(** [map_expr fe fs e] transforms the exprression [e] into a new one,
    by splitting [fe e] into its sub-parts, applying [map_expr fe fs] and
    [map_stmt fe fs] on them, and finally gathering the results with
    the builder of [fe e].
*)
                    

val map_stmt :
  (Ast.expr -> Ast.expr) ->
  (Ast.stmt -> Ast.stmt) ->
  Ast.stmt -> Ast.stmt =
(** [map_stmt fe fs s] same as [map_expr] but on statements. *)

val fold_expr :
  ('a -> Ast.expr -> 'a) ->
  ('a -> Ast.stmt -> 'a) ->
  'a -> Ast.expr -> 'a
(** Folding function for expressions  *)


val fold_stmt :
  ('a -> Ast.expr -> 'a) ->
  ('a -> Ast.stmt -> 'a) ->
  'a -> Ast.stmt -> 'a
(** Folding function for statements *)

val fold_map_expr :
  ('a -> Ast.expr -> 'a * Ast.expr) ->
  ('a -> Ast.stmt -> 'a * Ast.stmt) ->
  'a -> Ast.expr -> 'a * Ast.expr
(** Combination of map and fold for expressions *)

val fold_map_stmt :
  ('a -> Ast.expr -> 'a * Ast.expr) ->
  ('a -> Ast.stmt -> 'a * Ast.stmt) ->
  'a -> Ast.stmt -> 'a * Ast.stmt
(** Combination of map and fold for statements *)

val expr_vars : Ast.expr -> Ast.var list
(** Extract variables from an expression *)

val stmt_vars : Ast.stmt -> var list
(** Extract variables from a statement *)
