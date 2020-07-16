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
open Log
open Semantic

type expr_rewrite =
  | Return  of expr
  | Forward of expr * semantic

type 'a rewrite = ('a,expr_rewrite) cases

include Cases

let get_expr = function
  | Return e -> e
  | Forward(e,_) -> e

let get_semantic_opt = function
  | Return _ -> None
  | Forward(_,s) -> Some s

let compare_expr_rewrite de1 de2 =
  match de1, de2 with
  | Return e1, Return e2             -> compare_expr e1 e2
  | Forward (e1,s1), Forward (e2,s2) -> Compare.pair compare_expr compare_semantic (e1,s1) (e2,s2)
  | _ -> compare de1 de2

let pp_expr_rewrite fmt = function
  | Return e      -> pp_expr fmt e
  | Forward (e,s) -> Format.fprintf fmt "%a {%a}" pp_expr e pp_semantic s

let return_singleton ?(cleaners=[]) exp flow = Cases.singleton (Return exp) flow ~cleaners

let forward_singleton ?(cleaners=[]) exp ~semantic flow = Cases.singleton (Forward (exp,semantic)) flow ~cleaners

let return_eval evl = Cases.map (fun e -> Return e) evl

let forward_eval evl ~semantic = Cases.map (fun e -> Forward (e,semantic)) evl

let print fmt (evl: 'a rewrite) : unit =
  Cases.print_some (fun fmt de flow -> pp_expr_rewrite fmt de) fmt evl

let remove_duplicates lattice evl = Cases.remove_duplicates compare_expr_rewrite lattice evl
