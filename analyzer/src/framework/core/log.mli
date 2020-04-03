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

(** Journal logs *)

open Ast.Stmt
open Ast.Var

type log

val concat : log -> log -> log

val empty : log

val is_empty : log -> bool

val tuple : log * log -> log

val first : log -> log

val second : log -> log

val get_domain_block : log -> block

val get_domain_inner_log : log -> log

val append : stmt -> log -> log

val append_fst : stmt -> log -> log

val append_snd : stmt -> log -> log

val print : Format.formatter -> log -> unit

val generic_domain_merge : add:(var->'b->'a->'a) -> find:(var->'a->'b) -> remove:(var->'a->'a) -> ('a*block) -> ('a*block) -> 'a*'a
