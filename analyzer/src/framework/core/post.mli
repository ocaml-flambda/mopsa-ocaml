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

(** Post-states with journaling flows. *)

open Ast.Stmt
open Token
open Flow
open Log
open Lattice.Sig

type 'a post

val return : 'a flow -> 'a post

val bind_eval : 'a lattice -> ('e -> 'a flow -> 'a post) -> ('e, 'a) Eval.eval -> 'a post

val bind_eval_flow : 'a lattice -> ('e -> 'a flow -> 'a post) -> ('e, 'a) Eval.eval -> 'a flow

val bind : ('a flow -> 'a post) -> 'a post -> 'a post

val map_log : (token -> log -> log) -> 'a post -> 'a post

val map : (token -> 'a -> log -> 'b * log) -> 'b Context.ctx -> 'a post -> 'b post

val to_flow : 'a lattice -> 'a post -> 'a flow
