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

(** Logs are a journal of inner statements executed by the domains in
    an abstraction DAG in order to compute a post-condition. They are
    used to merge two post-conditions that diverged due to a fork-join
    trajectory in the abstraction DAG.

    Logs are presented as binary trees. This should work even when we
    have an abstraction DAG (i.e. when there are shared domains) since:

    A  x  B
     \___/
       |
       C

    is represented as:

       o
      / \
     x   C
    / \
   A   B
*)


open Ast.Stmt
open Ast.Var
open Location



type log

val pp_log : Format.formatter -> log -> unit

val empty_log : log

val is_empty_log : log -> bool

val mk_log : stmt list -> log -> log -> log

val get_left_log : log -> log

val get_right_log : log -> log

val get_log_stmts : log -> stmt list

val set_left_log : log -> log -> log

val set_right_log : log -> log -> log

val map_left_log : (log -> log) -> log -> log

val map_right_log : (log -> log) -> log -> log

val add_stmt_to_log : stmt -> log -> log

val merge_log : (stmt list -> stmt list) -> (stmt list -> stmt list) -> (stmt list -> stmt list -> stmt list) -> log -> log -> log

val concat_log : log -> log -> log

val meet_log : log -> log -> log

val join_log : log -> log -> log

val generic_domain_merge : add:(var->'b->'a->'a) -> find:(var->'a->'b) -> remove:(var->'a->'a) -> ('a*block) -> ('a*block) -> 'a*'a

type stmt_kind += S_ndet of stmt list * stmt list

val mk_ndet : stmt list -> stmt list -> range -> stmt
