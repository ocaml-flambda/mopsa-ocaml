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

(** Post - Post-states of statement execution. *)

open Ast.Stmt
open Token
open Flow
open Effect
open Context
open Cases
open Lattice
open Callstack


type 'a post = ('a, unit) cases

val join : 'a post -> 'a post -> 'a post

val join_list : empty:(unit -> 'a post) -> 'a post list -> 'a post

val meet : 'a post -> 'a post -> 'a post

val meet_list : empty:(unit -> 'a post) -> 'a post list -> 'a post

val return : ?cleaners:block -> 'a flow -> 'a post

val bind : ('a flow -> ('a,'r) cases) -> 'a post -> ('a,'r) cases
val (>>%) : 'a post -> ('a flow -> ('a,'r) cases) -> ('a,'r) cases

val bind_opt : ('a flow -> ('a,'r) cases option) -> 'a post -> ('a,'r) cases option
val (>>%?) : 'a post -> ('a flow -> ('a,'r) cases option) -> ('a,'r) cases option

val remove_duplicates : 'a lattice -> 'a post -> 'a post
