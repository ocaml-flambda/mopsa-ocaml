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

open Ast.Stmt
open Ast.Expr
open Token
open Flow
open Context
open Cases
open Change
open Ast.Semantic
open Mopsa_utils


type 'a eval  = ('a,expr) cases

include Cases

let singleton ?(changes=empty_change_map) ?(cleaners=[]) ?(translations=[]) e flow =
  let e' = List.fold_left (fun acc (s,ee) -> add_expr_translation s ee acc) e translations in
  Cases.singleton ~changes ~cleaners e' flow

let add_translation semantic e evl =
  Cases.map_result (add_expr_translation semantic e) evl

let print fmt (evl: 'a eval) : unit =
  Cases.print_result (fun fmt e flow -> pp_expr fmt e) fmt evl

let remove_duplicates lattice evl =
  Cases.remove_duplicate_results
    ~equal:(fun e1 e2 ->
       (* Compare the expressions and their translations *)
       0 = Compare.pair compare_expr (SemanticMap.compare compare_expr)
         (e1,e1.etrans)
         (e2,e2.etrans)
    ) lattice evl
