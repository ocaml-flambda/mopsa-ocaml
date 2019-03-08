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
module Eval = Eval
type ('e, 'a) eval = ('e, 'a) Eval.eval
module Flow = Flow
type 'a flow = 'a Flow.flow
module Post = Post
type 'a post = 'a Post.post
include Log
module Log = Log
include Eq
module Manager = Manager
include Manager
module Query = Query
type 'a query = 'a Query.query
include Token
include Zone
module Domain = Domain
include Domain.Id
include Domain.Interface
module Interface = Domain.Interface
include Domain.Sig
include Lattice.Sig
module Lattice = Lattice
