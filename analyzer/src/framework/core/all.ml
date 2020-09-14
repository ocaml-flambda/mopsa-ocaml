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

include Ast.Constant
include Ast.Expr
include Ast.Stmt
include Ast.Typ
include Ast.Program
include Ast.Frontend
include Ast.Operator
include Ast.Var
include Ast.Visitor

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end

include Alarm
module Alarm = Alarm

include Context
module Context = Context

module Cases = Cases

type 'r case = 'r Cases.case
type ('a,'r) cases = ('a,'r) Cases.cases

let bind = Cases.bind
let (>>=) = Cases.(>>=)

let bind_opt = Cases.bind_opt
let (>>=?) = Cases.(>>=?)

let bind_result = Cases.bind_result
let (>>$) = Cases.(>>$)

let bind_result_opt = Cases.bind_result_opt
let (>>$?) = Cases.(>>$?)

let bind_list = Cases.bind_list
let bind_list_opt = Cases.bind_list_opt

module Eval = Eval
type 'a eval = 'a Eval.eval

module Flow = Flow
type 'a flow = 'a Flow.flow

module Post = Post
type 'a post = 'a Post.post

let (>>%) = Post.(>>%)
let (>>%?) = Post.(>>%?)

module Log = Log
include Log

include Query

include Token

include Semantic

include Route

include Lattice

include Id

include Manager

module Soundness = Soundness

module Hook = Hook
