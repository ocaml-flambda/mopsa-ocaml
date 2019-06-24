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
open Zone
open Context
open Result


type 'a eval = ('a,expr) result

let return ?(cleaners=[]) ?(log=Log.empty) e flow : 'a eval = Result.return e flow ~cleaners ~log

let singleton ?(cleaners=[]) e flow : 'a eval = Result.singleton e flow ~cleaners


let empty flow : 'a eval = Result.empty flow

let empty_singleton = empty


let join (evl1:'a eval) (evl2:'a eval) : 'a eval =
  Result.join evl1 evl2


let meet (evl1:'a eval) (evl2:'a eval) : 'a eval =
  Result.meet evl1 evl2


let join_list ~(empty:'a eval) (l:'a eval list) : 'a eval =
  Result.join_list ~empty l


let meet_list ~(empty:'a eval) (l:'a eval list) : 'a eval =
  Result.join_list ~empty l


let print fmt (evl: 'a eval) : unit =
  Result.print (fun fmt e flow ->
      pp_expr fmt e
    ) fmt evl


let add_cleaners cleaners evl : 'a eval =
  Result.add_cleaners cleaners evl


let get_ctx (evl:'a eval) : 'a ctx =
  Result.get_ctx evl


let set_ctx (ctx:'a ctx) (evl:'a eval) : 'a eval =
  Result.set_ctx ctx evl


let copy_ctx (src:'a eval) (dst:'a eval) : 'a eval =
  Result.copy_ctx src dst


let bind = Result.bind_some


let apply f join meet empty evl =
  Result.apply
    (fun oe flow ->
       match oe with
       | Some e -> f e flow
       | None -> empty
    )
    join meet evl
