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


type 'a eval = ('a, expr*semantic) cases

let return ?(cleaners=[]) ?(log=empty_log) e flow : 'a eval = Cases.return e flow ~cleaners ~log

let singleton ?(cleaners=[]) e ~semantic flow : 'a eval = Cases.singleton (e,semantic) flow ~cleaners

let empty_singleton = empty_singleton

let join (evl1:'a eval) (evl2:'a eval) : 'a eval =
  Cases.join evl1 evl2

let meet (evl1:'a eval) (evl2:'a eval) : 'a eval =
  Cases.meet evl1 evl2

let join_list ~(empty:unit -> 'a eval) (l:'a eval list) : 'a eval =
  Cases.join_list ~empty l

let meet_list ~(empty:unit -> 'a eval) (l:'a eval list) : 'a eval =
  Cases.meet_list ~empty l

let print fmt (evl: 'a eval) : unit =
  Cases.print_some (fun fmt (e,sem) flow ->
      Format.fprintf fmt "%a {%a}" pp_expr e pp_semantic sem
    ) fmt evl

let add_cleaners cleaners evl : 'a eval =
  Cases.add_cleaners cleaners evl

let get_ctx (evl:'a eval) : 'a ctx =
  Cases.get_ctx evl

let set_ctx (ctx:'a ctx) (evl:'a eval) : 'a eval =
  Cases.set_ctx ctx evl

let get_callstack = Cases.get_callstack

let copy_ctx (src:'a eval) (dst:'a eval) : 'a eval =
  Cases.copy_ctx src dst

let bind = Cases.bind_some

let apply f join meet empty evl =
  Cases.apply
    (fun oe flow ->
       match oe with
       | Some e -> f e flow
       | None -> empty
    )
    join meet evl

let map = Cases.map

let remove_duplicates lattice evl = Cases.remove_duplicates (Compare.pair compare_expr compare_semantic) lattice evl

let cardinal = Cases.cardinal
