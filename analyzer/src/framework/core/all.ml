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

include Alarm

module Callstack = Callstack

module Context = Context

module Cases = Cases

type ('a,'r) cases = ('a,'r) Cases.cases

let bind_full = Cases.bind_full
let (>>*) = Cases.(>>*)

let bind_full_opt = Cases.bind_full_opt
let (>>*?) = Cases.(>>*?)

let bind = Cases.bind
let (>>>) = Cases.(>>>)

let bind_opt = Cases.bind_opt
let (>>>?) = Cases.(>>>?)


let bind_some = Cases.bind_some
let (>>$) = Cases.(>>$)


let bind_some_opt = Cases.bind_some_opt
let (>>$?) = Cases.(>>$?)

let bind_list = Cases.bind_list
let bind_list_opt = Cases.bind_list_opt


module Eval = Eval
type 'a eval = 'a Eval.eval

module Flow = Flow
type 'a flow = 'a Flow.flow

module Post = Post
type 'a post = 'a Post.post

module Log = Log

include Query

include Token

include Zone

include Lattice

include Id

include Interface
module Interface = Interface

module Sig = Sig

module Channel = Channel

module Soundness = Soundness

module Hook = Hook
