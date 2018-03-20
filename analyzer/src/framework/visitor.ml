(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Unified method to visit, fold or modify an AST.

   To allow visiting the extensible types of statements and expressions,
   the developper should define its structure giving:
   - its parts consisting of sub-expressions and sub-statements,
   - its builder that can reconstitute the statement/expression from its parts.
*)


let debug fmt = Debug.debug ~channel:"framework.visitor" fmt


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

(** Parts are the direct sub-elements of an AST node *)
type parts = {
  exprs : Ast.expr list; (** child expressions *)
  stmts : Ast.stmt list; (** child statements *)
}

(** A structure of an extensible type ['a] is a tuple composed of two elements:
    the parts and a builder function.
*)
type 'a structure =
  parts * (parts -> 'a)



(*==========================================================================*)
                        (** {2 Visitors chains} *)
(*==========================================================================*)

(** A chain stores the head of the visitors, each of which will call
    the next one in the chain when encountering an AST node that
    can not be handled.
*)
type 'a chain = ('a -> 'a structure) ref

(** Empty chains, that should not be reached when visiting an AST node *)
let expr_chain : Ast.expr chain = ref (fun exp ->
    Debug.fail "Unknown expression %a" Pp.pp_expr exp
  )

let stmt_chain : Ast.stmt chain = ref (fun stmt ->
    failwith "Unknown statement"
  )

(** To register a visitor of new expressions, [register_exp_visitor]
    should be called with a function having two arguments:
       - a default visitor to use for unknown expressions
       - the expression to visit
*)
let register_expr_visitor
    (v: (Ast.expr -> Ast.expr structure) -> Ast.expr -> Ast.expr structure) =
  expr_chain := v !expr_chain

let register_stmt_visitor
    (v: (Ast.stmt -> Ast.stmt structure) -> Ast.stmt -> Ast.stmt structure) =
  stmt_chain := v !stmt_chain

(** Leaf nodes maybe defined with a generic identity composer *)
let leaf (x: 'a) : 'a structure =
  {exprs = []; stmts = []}, (fun _ -> x)


let split_expr (expr : Ast.expr) : Ast.expr structure = !expr_chain expr
let split_stmt (stmt : Ast.stmt) : Ast.stmt structure = !stmt_chain stmt


(*==========================================================================*)
                        (** {2 Iterators} *)
(*==========================================================================*)


let fold_map_list (f : 'a -> 'b -> ('a * 'b)) (x0 : 'a) (l : 'b list)
  : ('a * 'b list) =
  let (xe,l') =
    List.fold_left (fun (accx, accl) z ->
        let x,z' = f accx z in
        (x, z' :: accl)
      ) (x0, []) l
  in
  (xe, List.rev l')

(** [map_expr fe fs e] transforms the exprression [e] into a new one,
    by splitting [fe e] into its sub-parts, applying [map_expr fe fs] and
    [map_stmt fe fs] on them, and finally gathering the results with
    the builder of [fe e].
*)
let rec map_expr
    (fe: Ast.expr -> Ast.expr)
    (fs: Ast.stmt -> Ast.stmt)
    (e: Ast.expr) : Ast.expr =
  let e' = fe e in
  let parts, builder = split_expr e' in
  let exprs' = List.map (map_expr fe fs) parts.exprs
  and stmts' = List.map (map_stmt fe fs) parts.stmts in
  builder {exprs = exprs'; stmts = stmts'}

(** [map_stmt fe fs s] same as [map_expr] but on statements. *)
and map_stmt
    (fe: Ast.expr -> Ast.expr)
    (fs: Ast.stmt -> Ast.stmt) s : Ast.stmt =
  let s' = fs s in
  let parts, builder = split_stmt s' in
  let exprs' = List.map (map_expr fe fs) parts.exprs
  and stmts' = List.map (map_stmt fe fs) parts.stmts in
  builder {exprs = exprs'; stmts = stmts'}

(** Folding function for expressions  *)
let rec fold_expr
    (fe: 'a -> Ast.expr -> 'a)
    (fs: 'a -> Ast.stmt -> 'a) x0 e =
  let x1 = fe x0 e in
  let parts, _ = split_expr e in
  let x2 = List.fold_left (fold_expr fe fs) x1 parts.exprs in
  List.fold_left (fold_stmt fe fs) x2 parts.stmts


(** Folding function for statements *)
and fold_stmt
    (fe: 'a -> Ast.expr -> 'a)
    (fs: 'a -> Ast.stmt -> 'a) x0 s =
  let x1 = fs x0 s in
  let parts, _ = split_stmt s in
  let x2 = List.fold_left (fold_expr fe fs) x1 parts.exprs in
  List.fold_left (fold_stmt fe fs) x2 parts.stmts

(** Combination of map and fold for expressions *)
and fold_map_expr
    (fme  : 'a -> Ast.expr -> 'a * Ast.expr)
    (fms  : 'a -> Ast.stmt -> 'a * Ast.stmt)
    (x0   : 'a)
    (expr : Ast.expr)
  :
    ('a * Ast.expr)
  =
  let x1, expr' = fme x0 expr in
  let parts, builder = split_expr expr' in
  let x2, exprs =
    fold_map_list (fun x0 (z : Ast.expr) ->
        fold_map_expr fme fms x0 z
      ) x1 parts.exprs
  in
  let x3, (stmts : Ast.stmt list) =
    fold_map_list (fun x0 (z : Ast.stmt) ->
        fold_map_stmt fme fms x0 z
      ) x2 parts.stmts
  in
  (x3,builder {exprs;stmts})

(** Combination of map and fold for statements *)
and fold_map_stmt
    (fme  : 'a -> Ast.expr -> 'a * Ast.expr)
    (fms  : 'a -> Ast.stmt -> 'a * Ast.stmt)
    (x0   : 'a)
    (stmt : Ast.stmt)
  :
    ('a * Ast.stmt)
  =
  let x1, stmt' = fms x0 stmt in
  let parts, builder = split_stmt stmt' in
  let x2, exprs =
    fold_map_list (fun x0 z ->
        fold_map_expr fme fms x0 z
      ) x1 parts.exprs
  in
  let x3, stmts =
    fold_map_list (fun x0 z ->
        fold_map_stmt fme fms x0 z
      ) x2 parts.stmts
  in
  (x3,builder {exprs;stmts})
