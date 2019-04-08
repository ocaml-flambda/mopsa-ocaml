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

(** AST visitors *)

open Expr
open Var
open Stmt

(*==========================================================================*)
(**                          {2 Structure}                                  *)
(*==========================================================================*)


(** Parts are the direct sub-elements of an AST node *)
type parts = {
  exprs : expr list; (** child expressions *)
  stmts : stmt list; (** child statements *)
}

(** A structure of an extensible type ['a] is a tuple composed of two elements:
    the parts and a builder function.
*)
type 'a structure = parts * (parts -> 'a)

let leaf (x: 'a) : 'a structure =
  {exprs = []; stmts = []}, (fun _ -> x)



(*==========================================================================*)
(**                           {2 Registration}                              *)
(*==========================================================================*)


(** Information record of an AST construct with visitors *)
type 'a vinfo = {
  compare : 'a TypeExt.compare;
  print   : 'a TypeExt.print;
  visit : ('a -> 'a structure) -> 'a -> 'a structure;
}


let expr_visit_chain = ref (fun exp ->
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
      Exceptions.panic "expr visitor: unknown expression %a" pp_expr exp
  )

let register_expr_with_visitor (info: expr vinfo) : unit =
  TypeExt.register_compare info.compare expr_compare_chain;
  TypeExt.register_print info.print expr_pp_chain;
  expr_visit_chain := info.visit !expr_visit_chain;
  ()


let register_expr_visitor v : unit =
  expr_visit_chain := v !expr_visit_chain;
  ()


let stmt_visit_chain : (stmt -> stmt structure) ref =
  ref (fun stmt ->
      match skind stmt with
      | S_program _ -> Exceptions.panic "visitor of S_program not supported"

      | S_assign(lhs, rhs) -> {
          exprs = [lhs; rhs];
          stmts = []
        } , (
            function
            | { exprs = [lhs; rhs] } -> { stmt with skind = S_assign(lhs, rhs) }
            | _ -> assert false
          )

      | S_assume cond ->  {
          exprs = [cond];
          stmts = []
        } , (
            function
            | { exprs = [cond] } -> { stmt with skind = S_assume(cond) }
            | _ -> assert false
          )

      | S_rename _
      | S_add _
      | S_remove _
      | S_forget _
      | S_project _
      | S_expand _
      | S_fold _ -> leaf stmt

      | _ -> Exceptions.panic "stmt_visit_chain: unknown statement"
    )

let register_stmt_with_visitor (info: stmt vinfo) : unit =
  TypeExt.register_compare info.compare stmt_compare_chain;
  TypeExt.register_print info.print stmt_pp_chain;
  stmt_visit_chain := info.visit !stmt_visit_chain;
  ()

let register_stmt_visitor v : unit =
  stmt_visit_chain := v !stmt_visit_chain;
  ()

let split_expr (expr : expr) : expr structure = !expr_visit_chain expr

let split_stmt (stmt : stmt) : stmt structure = !stmt_visit_chain stmt


(*==========================================================================*)
(**                            {2 Visitors}                                 *)
(*==========================================================================*)


(** Visitor actions *)
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
    (fe: expr -> expr action)
    (fs: stmt -> stmt action)
    (e: expr) : expr =
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
    (fe: expr -> expr action)
    (fs: stmt -> stmt action) s : stmt =
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
    (fe: 'a -> expr -> 'a action)
    (fs: 'a -> stmt -> 'a action) x0 e =
  match fe x0 e with
  | Keep x1 -> x1
  | Visit x1 -> fold_expr fe fs x1 e
  | VisitParts x1 ->
    let parts, _ = split_expr e in
    let x2 = List.fold_left (fold_expr fe fs) x1 parts.exprs in
    List.fold_left (fold_stmt fe fs) x2 parts.stmts


(** Folding function for statements *)
and fold_stmt
    (fe: 'a -> expr -> 'a action)
    (fs: 'a -> stmt -> 'a action) x0 s =
  match fs x0 s with
  | Keep x1 -> x1
  | Visit x1 -> fold_stmt fe fs x1 s
  | VisitParts x1 ->
    let parts, _ = split_stmt s in
    let x2 = List.fold_left (fold_expr fe fs) x1 parts.exprs in
    List.fold_left (fold_stmt fe fs) x2 parts.stmts

(** Combination of map and fold for expressions *)
and fold_map_expr
    (fme  : 'a -> expr -> ('a * expr) action)
    (fms  : 'a -> stmt -> ('a * stmt) action)
    (x0   : 'a)
    (expr : expr)
  :
    ('a * expr)
  =
  match fme x0 expr with
  | Keep (x1, expr') -> x1, expr'
  | Visit (x1, expr') -> fold_map_expr fme fms x1 expr'
  | VisitParts (x1, expr') ->
    let parts, builder = split_expr expr' in
    let x2, exprs =
      fold_map_list (fun x0 (z : expr) ->
          fold_map_expr fme fms x0 z
        ) x1 parts.exprs
    in
    let x3, (stmts : stmt list) =
      fold_map_list (fun x0 (z : stmt) ->
          fold_map_stmt fme fms x0 z
        ) x2 parts.stmts
    in
    (x3,builder {exprs;stmts})

(** Combination of map and fold for statements *)
and fold_map_stmt
    (fme  : 'a -> expr -> ('a * expr) action)
    (fms  : 'a -> stmt -> ('a * stmt) action)
    (x0   : 'a)
    (stmt : stmt)
  :
    ('a * stmt)
  =
    match fms x0 stmt with
    | Keep (x1, stmt') -> x1, stmt'
    | Visit (x1, stmt') -> fold_map_stmt fme fms x1 stmt'
    | VisitParts (x1, stmt') ->
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
let expr_vars (e: expr) : var list =
  fold_expr
    (fun acc e ->
       match ekind e with
       | E_var(v, m) -> Keep (v :: acc)
       | _ -> VisitParts acc
    )
    (fun acc s -> VisitParts acc)
    [] e

(** Extract variables from a statement *)
let stmt_vars (s: stmt) : var list =
  fold_stmt
    (fun acc e ->
       match ekind e with
       | E_var(v, m) -> Keep (v :: acc)
       | _ -> VisitParts acc
    )
    (fun acc s -> VisitParts acc)
    [] s
