(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Visitor of statements and expressions. *)


open Ast

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
type 'a structure = parts * (parts -> 'a)

(*==========================================================================*)
                        (** {2 Visitors chains} *)
(*==========================================================================*)

(** A chain stores the head of the visitors, each of which will call
    the next one in the chain when encountering an AST node that
    can not be handled.
*)
type 'a chain = ('a -> 'a structure) ref


(** Leaf nodes maybe defined with a generic identity composer *)
let leaf (x: 'a) : 'a structure =
  {exprs = []; stmts = []}, (fun _ -> x)


(** Empty chains, that should not be reached when visiting an AST node *)
let expr_chain : Ast.expr chain = ref (fun exp ->
    match ekind exp with
    | E_var _ -> leaf exp
    | E_constant _ -> leaf exp
    | E_unop(unop, e) ->
      {exprs = [e]; stmts = []},
      (fun parts -> {exp with ekind = E_unop(unop, List.hd parts.exprs)})
    | E_binop(binop, e1, e2) ->
        {exprs = [e1; e2]; stmts = []},
        (fun parts -> {exp with ekind = E_binop(binop, List.hd parts.exprs, List.nth parts.exprs 1)})
    | _ ->
      Exceptions.panic "Unknown expression %a" pp_expr exp
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


let split_expr (expr : Ast.expr) : Ast.expr structure = !expr_chain expr
let split_stmt (stmt : Ast.stmt) : Ast.stmt structure = !stmt_chain stmt


(*==========================================================================*)
                        (** {2 Iterators} *)
(*==========================================================================*)

(** Kinds of returned actions by a visitor *)
type 'a action =
  | Keep of 'a       (** Keep the result *)
  | VisitParts of 'a (** Continue visiting the parts of the result *)
  | Visit of 'a      (** Iterate the visitor on the result *)


let fold_map_list (f : 'a -> 'b -> ('a * 'b)) (x0 : 'a) (l : 'b list)
  : ('a * 'b list) =
  let (xe,l') =
    List.fold_left (fun (accx, accl) z ->
        let x,z' = f accx z in
        (x, z' :: accl)
      ) (x0, []) l
  in
  (xe, List.rev l')

(** [map_expr fe fs e] transforms the expression [e] into a new one,
    by splitting [fe e] into its sub-parts, applying [map_expr fe fs] and
    [map_stmt fe fs] on them, and finally gathering the results with
    the builder of [fe e].
*)
let rec map_expr
    (fe: Ast.expr -> Ast.expr action)
    (fs: Ast.stmt -> Ast.stmt action)
    (e: Ast.expr) : Ast.expr =
  match fe e with
  | Keep e' -> e'
  | Visit e' -> map_expr fe fs e'
  | VisitParts e' ->
    let parts, builder = split_expr e' in
    let exprs' = List.map (map_expr fe fs) parts.exprs
    and stmts' = List.map (map_stmt fe fs) parts.stmts in
    builder {exprs = exprs'; stmts = stmts'}

(** [map_stmt fe fs s] same as [map_expr] but on statements. *)
and map_stmt
    (fe: Ast.expr -> Ast.expr action)
    (fs: Ast.stmt -> Ast.stmt action) s : Ast.stmt =
  match fs s with
  | Keep s' -> s'
  | Visit s' -> map_stmt fe fs s'
  | VisitParts s' ->
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
    (fme  : 'a -> Ast.expr -> 'a * Ast.expr action)
    (fms  : 'a -> Ast.stmt -> 'a * Ast.stmt action)
    (x0   : 'a)
    (expr : Ast.expr)
  :
    ('a * Ast.expr)
  =
  let x1, action = fme x0 expr in
  match action with
  | Keep expr' -> x1, expr'
  | Visit expr' -> fold_map_expr fme fms x1 expr'
  | VisitParts expr' ->
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
    (fme  : 'a -> Ast.expr -> 'a * Ast.expr action)
    (fms  : 'a -> Ast.stmt -> 'a * Ast.stmt action)
    (x0   : 'a)
    (stmt : Ast.stmt)
  :
    ('a * Ast.stmt)
  =
    let x1, action = fms x0 stmt in
    match action with
    | Keep stmt' -> x1, stmt'
    | Visit stmt' -> fold_map_stmt fme fms x1 stmt'
    | VisitParts stmt' ->
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


(** Extract variables from an expression *)
let expr_vars (e: Ast.expr) : Ast.var list =
  fold_expr
    (fun acc e ->
       match Ast.ekind e with
       | Ast.E_var(v, m) -> v :: acc
       | _ -> acc
    )
    (fun acc s -> acc)
    [] e

(** Extract variables from a statement *)
let stmt_vars (s: stmt) : var list =
  fold_stmt
    (fun acc e ->
       match Ast.ekind e with
       | Ast.E_var(v, m) -> v :: acc
       | _ -> acc
    )
    (fun acc s -> acc)
    [] s
