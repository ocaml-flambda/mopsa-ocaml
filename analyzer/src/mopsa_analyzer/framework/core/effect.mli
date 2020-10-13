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

(** Effects are a journal of all statements executed by the domains when
    computing a post-condition.

    They are used to merge two post-conditions that diverged due to a fork-join
    trajectory in the abstraction DAG.
*)


open Ast.Stmt
open Ast.Var


(** {1 Effects}*)

type effect =
  | Effect_empty
  | Effect_block of stmt list
  | Effect_seq of effect list
  | Effect_join of effect * effect
  | Effect_meet of effect * effect


val pp_effect : Format.formatter -> effect -> unit
(** Print a effect *)

val compare_effect : effect -> effect -> int
(** Compare two effects *)

val empty_effect : effect
(** Empty effect *)

val is_empty_effect : effect -> bool
(** Check whether a effect is empty *)

val join_effect : effect -> effect -> effect
(** Compute the union of two effects *)

val meet_effect : effect -> effect -> effect
(** Compute the intersection of two effects *)

val fold_stmt_effect : ('a -> stmt -> 'a) -> 'a -> effect -> 'a
(** Fold over the statements in the effect *)

(** {1 Effect trees}*)

(** Effects are presented as binary trees. Each node of the tree is
    associated to a domain in the abstraction and stores the statements
    executed by the domain when computing a post-condition.

    Note that this representation is also useful when we have an abstraction DAG
    (i.e. when there are shared domains) thanks to the compose (stack) operator:

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
type teffect =
  | Teffect_empty
  | Teffect_node of effect * teffect * teffect

val empty_teffect : teffect
(** Create an empty effects tree *)

val mk_teffect : effect -> teffect -> teffect -> teffect
(** [mk_teffect effect left right] creates an effect tree with a root
    containing [effect] and having [left] and [right] as the two
    sub-trees *)

val pp_teffect : Format.formatter -> teffect -> unit
(** Print an effects tree *)

val compare_teffect : teffect -> teffect -> int
(** Compare two effects trees *)

val is_empty_teffect : teffect -> bool
(** Check whether an effects tree is empty *)

val get_left_teffect : teffect -> teffect
(** [get_left_teffect te] returns the left sub-tree of [te] *)

val get_right_teffect : teffect -> teffect
(** [get_right_teffect te] returns the right sub-tree of [te] *)

val get_root_effect : teffect -> effect
(** [get_log_stmts te] returns the effect at the root node of [te] *)

val set_left_teffect : teffect -> teffect -> teffect
(** [get_left_teffect left te] sets [left] as the left sub-tree of [te] *)

val set_right_teffect : teffect -> teffect -> teffect
(** [get_right_teffect right te] sets [right] as the right sub-tree of [te] *)

val map_left_teffect : (teffect -> teffect) -> teffect -> teffect
(** [map_left_teffect f te] is equivalent to [set_left_teffect (f (get_left_teffect te)) te] *)

val map_right_teffect : (teffect -> teffect) -> teffect -> teffect
(** [map_right_teffect f te] is equivalent to [set_right_teffect (f (get_right_teffect te)) te] *)

val add_stmt_to_teffect : stmt -> teffect -> teffect
(** [add_stmt_to_teffect stmt teffect] adds [stmt] to the the effects of the root no of [te] *)

val merge_teffect : (effect -> effect) -> (effect -> effect) -> (effect -> effect -> effect) -> teffect -> teffect -> teffect
(** [merge_teffect f1 f2 f te1 te2] combines [te1] and [te2] *)

val concat_teffect : old:teffect -> recent:teffect -> teffect
(** [concat_teffect old recent] puts effects in [old] before those in [recent] *)

val meet_teffect : teffect -> teffect -> teffect
(** [merge_teffect te1 te2] computes the effects of two intersected post-states *)

val join_teffect : teffect -> teffect -> teffect
(** [merge_teffect te1 te2] computes the effects of two joined post-states *)

val fold_stmt_teffect : ('a -> stmt -> 'a) -> 'a -> teffect -> 'a
(** Fold over the statements in the effects tree *)

(** {1 Generic merge} *)

(** Effect of a statement in terms of modified and removed variables *)
type var_effect = {
  modified: VarSet.t;
  removed: VarSet.t;
}

val generic_merge :
  add:(var->'b->'a->'a) ->
  find:(var->'a->'b) ->
  remove:(var->'a->'a) ->
  ?custom:(stmt -> var_effect option) ->
  ('a*effect) -> ('a*effect) -> 'a*'a
(** Generic merge operator.
    [generic_merge ~add ~find ~remove ~custom (a1,e1) (a2,e2)] applies a generic
    merge of states [a1] and [a2]:
    - It searches for modified variables in one state's effects, gets their
    value using [find] and adds them to the other state using [add].
    - It searches for removed variables in one state's effects and remove them
    from the other state using [remove].
    This function handles common statements (assign,assume,add,remove,fold,expand
    and rename). Other statements can be handled using the [custom] function that
    returns the [var_effect] of a given statement.
*)
