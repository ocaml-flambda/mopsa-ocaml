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
*)


open Ast.Stmt
open Ast.Var
open Location



type log
(** Logs are presented as binary trees. Each node of the tree is
    associated to a domain in the abstraction and stores the statements
    executed by the domain to compute a post-condition.

    Note that this representation is also useful when we have an abstraction DAG
    (i.e. when there are shared domains) since:

    A x B 
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

val pp_log : Format.formatter -> log -> unit
(** Print a log *)

val compare_log : log -> log -> int
(** Compare two logs *)

val empty_log : log
(** Empty log *)

val is_empty_log : log -> bool
(** Check whether a log is empty *)

val mk_log : stmt list -> log -> log -> log
(** [mk_log stmts left right] creates a log tree with a root
    containing [stmts] and having [left] and [right] as the two
    sub-trees *)

val get_left_log : log -> log
(** [get_left_log log] returns the left sub-tree of [log] *)

val get_right_log : log -> log
(** [get_right_log log] returns the right sub-tree of [log] *)

val get_log_stmts : log -> stmt list
(** [get_log_stmts log] returns the statements at the root of [log] *)

val set_left_log : log -> log -> log
(** [get_left_log left log] sets [left] as the left sub-tree of [log] *)

val set_right_log : log -> log -> log
(** [get_right_log right log] sets [right] as the right sub-tree of [log] *)

val map_left_log : (log -> log) -> log -> log
(** [map_left_log log] is equivalent to [set_left_log (f (get_left_log log)) log] *)

val map_right_log : (log -> log) -> log -> log
(** [map_right_log log] is equivalent to [set_right_log (f (get_right_log log)) log] *)

val add_stmt_to_log : stmt -> log -> log
(** [add_stmt_to_log stmt log] adds [stmt] to the statements of the root of [log] *)

val merge_log : (stmt list -> stmt list) -> (stmt list -> stmt list) -> (stmt list -> stmt list -> stmt list) -> log -> log -> log
(** [merge_log f1 f2 f log1 log2] combines [log1] and [log2] *)

val concat_log : log -> log -> log
(** [concat_log log1 log2] merges [log1] and [log2] by concatenating logged statements *)

val meet_log : log -> log -> log
(** [merge_log log1 log2] computes the logs of two intersected post-states *)

val join_log : log -> log -> log
(** [merge_log log1 log2] computes the logs of two joined post-states *)

val generic_domain_merge : add:(var->'b->'a->'a) -> find:(var->'a->'b) -> remove:(var->'a->'a) -> ('a*block) -> ('a*block) -> 'a*'a
(** Generic merge operator *)

type stmt_kind += S_ndet of stmt list * stmt list
(** Non-deterministic statement combining two blocks of statements (used in [join_log]) *)

val mk_ndet : stmt list -> stmt list -> range -> stmt
(** Creates a non-deterministic statement *)
