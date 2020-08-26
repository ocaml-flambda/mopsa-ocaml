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

(** Cases - data structure for case-based transfer functions.

    Transfer functions use [Cases] to return partitioned results. Cases are
    encoded as DNF formulas. Each individual case comes with a flow, a set
    of alarms, computation logs and cleaner statements. To represent suspended
    computations, the result of a case can be empty.
*)


open Token
open Ast.Stmt
open Flow
open Log
open Context
open Callstack



(** Cases of results ['r] over an abstraction ['a] *)
type ('a,'r) cases


(****************************************************************************)
(**                      {2 Utility functions}                              *)
(****************************************************************************)

val return : ?cleaners:block -> ?log:log -> 'r option -> 'a flow -> ('a,'r) cases
(** Create a singleton case. *)


val singleton : ?cleaners:block -> 'r -> 'a flow -> ('a,'r) cases
(** Create a singleton non-empty cas.e *)


val empty_singleton : 'a flow -> ('a,'r) cases
(** Create a singleton empty case. *)


val get_ctx : ('a,'r) cases -> 'a ctx
(** [get_ctx c] returns the context of cases [c]. *)


val set_ctx : 'a ctx -> ('a,'r) cases -> ('a,'r) cases
(** [set_ctx ctx c] changes the context of cases [c] to [ctx]. *)


val get_callstack: ('a,'r) cases -> callstack
(** [get_callstack c] returns the callstack of cases [c]. *)


val copy_ctx : ('a,'r) cases -> ('a,'s) cases -> ('a,'s) cases
(** [copy_ctx c1 c2] changes the context of cases [c2] to the context of [c1]. *)


val opt_clean_cur_only : bool ref
(** Option to apply cleaners on T_cur only *)

val concat_cleaners : block -> block -> block
(** Concatenate two cleaners *)

val add_cleaners : block -> ('a,'r) cases -> ('a,'r) cases
(** [add_cleaners block c] adds cleaner statements [block] to cases [c]. *)


(** Map outputs of a result *)
val map : ('r->'s) -> ('a,'r) cases -> ('a,'s) cases
(** [map f c] replaces each result [ri] of case [ci] in [c] with [f ri]. *)


val map_opt : ('r -> 's option option) -> ('a,'r) cases -> ('a,'s) cases
(** [map_opt f c] replaces each maps each result [ri] of case [ci] in
    [c] with [Option.get (f ri)]. If [f ri] returns [None], the case is
    removed.
*)


val map_log : (log -> log) -> ('a,'r) cases -> ('a,'r) cases
(** [map_log f c] applies function [f] to logs in [c]. *)


val set_log : log -> ('a,'r) cases -> ('a,'r) cases
(** Set the logs of cases *)

val clear_log : ('a,'r) cases -> ('a,'r) cases
(** Remove logs *)

val apply : ('r option -> 'a flow -> 'b) -> ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) -> ('a,'r) cases -> 'b
(** [apply f join meet c] collapses cases [c] to a single value by
    applying [f] on each case [ci] in [c] and merging outputs using
    [join] and [meet].
*)

val apply_some : ('r -> 'a flow -> 'b) -> ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) -> 'b -> ('a,'r) cases -> 'b
(** [apply_some f join meet bottom c] collapses cases [c] to a single value by
    applying [f] on non-empty each case [ci] in [c] and merging outputs using
    [join] and [meet].
*)

val apply_full : ('r option -> 'a flow -> log -> block -> 'b) -> ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) -> ('a,'r) cases -> 'b
(** Similar to [apply] but passes to [f] also logs and cleaners *)


val fold : ('r option -> 'a flow -> 'b -> 'b) -> ('a,'r) cases -> 'b -> 'b

val fold_some : ('r -> 'a flow -> 'b -> 'b) -> ('a,'r) cases -> 'b -> 'b

val for_all : ('r option -> 'a flow -> bool) -> ('a,'r) cases -> bool

val for_all_some : ('r -> 'a flow -> bool) -> ('a,'r) cases -> bool

val exists : ('r option -> 'a flow -> bool) -> ('a,'r) cases -> bool

val exists_some : ('r -> 'a flow -> bool) -> ('a,'r) cases -> bool

val print :
  (Format.formatter -> 'r option -> 'a flow -> unit) ->
  Format.formatter -> ('a,'r) cases -> unit
(** Pretty printer of cases *)


val print_some :
  (Format.formatter -> 'r -> 'a flow -> unit) ->
  Format.formatter ->
  ('a,'r) cases ->
  unit
(** Pretty printer of non-empty cases *)



(****************************************************************************)
(**                         {2 Lattice operators}                           *)
(****************************************************************************)

val join : ('a,'r) cases -> ('a,'r) cases -> ('a,'r) cases
(** Join two cases. *)


val meet : ('a,'r) cases -> ('a,'r) cases -> ('a,'r) cases
(** Meet two cases. *)


val join_list : empty:(unit -> ('a,'r) cases) -> ('a,'r) cases list -> ('a,'r) cases
(** Join a list of cases. *)


val meet_list : empty:(unit -> ('a,'r) cases) -> ('a,'r) cases list -> ('a,'r) cases
(** Meet a list of cases. *)


val map_fold_conjunctions :
  ('a flow * log -> 'a flow * log -> 'a flow) ->
  ('a,'r) cases -> ('a,'r) cases
(** [map_fold_conjunctions f c] folds [f] on the flows in each conjunction in [c]. *)


(****************************************************************************)
(**                        {2 Monadic binders}                              *)
(****************************************************************************)

val bind_full_opt :
  ('r option -> 'a flow -> log -> stmt list -> ('a,'s) cases option) ->
  ('a,'r) cases ->
  ('a,'s) cases option
(** [bind_full_opt f c] bind each case in [c] using [f]. All
    properties of the case (i.e. result, flow, logs and cleaners) are
    passed to [f].
*)


val (>>*?) :
  ('a,'r) cases ->
  ('r option -> 'a flow -> log -> stmt list -> ('a,'s) cases option) ->
  ('a,'s) cases option
(** Bind operator for [bind_full_opt] *)



val bind_full :
  ('r option -> 'a flow -> log -> stmt list -> ('a,'s) cases) ->
  ('a,'r) cases ->
  ('a,'s) cases


val (>>*) :
  ('a,'r) cases ->
  ('r option -> 'a flow -> log -> stmt list -> ('a,'s) cases) ->
  ('a,'s) cases
(** Bind operator for [bind_full] *)



val bind_opt :
  ('r option -> 'a flow -> ('a,'s) cases option) ->
  ('a,'r) cases ->
  ('a,'s) cases option
(** Same as [bind_full_opt], but without passing cleaners and logs to the partial transfer function. *)


val (>>=?) :
  ('a,'r) cases ->
  ('r option -> 'a flow -> ('a,'s) cases option) ->
  ('a,'s) cases option
(** Bind operator for [bind_opt] *)



val bind :
  ('r option -> 'a flow -> ('a,'s) cases) ->
  ('a,'r) cases ->
  ('a,'s) cases
(** Same as [bind_full], but without passing cleaners and logs to the transfer function. *)


val (>>=) :
  ('a,'r) cases ->
  ('r option -> 'a flow -> ('a,'s) cases) ->
  ('a,'s) cases
(** Bind operator for [bind] *)



val bind_some_opt :
  ('r -> 'a flow -> ('a,'s) cases option) ->
  ('a,'r) cases ->
  ('a,'s) cases option
(** Same as [bind_opt], but without passing empty outputs to the partial transfer function.
    Identify function is applied to empty outputs.
*)


val (>>$?) :
  ('a,'r) cases ->
  ('r -> 'a flow -> ('a,'s) cases option) ->
  ('a,'s) cases option
(** Bind operator for [bind_some_opt] *)


val bind_some :
  ('r -> 'a flow -> ('a,'s) cases) ->
  ('a,'r) cases ->
  ('a,'s) cases
(** Same as [bind], but without passing empty outputs to the transfer function.
    Identify function is applied to empty outputs.
*)


val (>>$) :
  ('a,'r) cases ->
  ('r -> 'a flow -> ('a,'s) cases) ->
  ('a,'s) cases
(** Bind operator for [bind_some] *)



val bind_list_opt :
  'r list -> ('r -> 'a flow -> ('a,'s) cases option) ->
  'a flow -> ('a, 's list) cases option
(** Bind a list of results with a partial transfer function. *)


val bind_list :
  'r list -> ('r -> 'a flow -> ('a,'s) cases) ->
  'a flow -> ('a, 's list) cases
(** Bind a list of results with a transfer function *)


val remove_duplicates : ('r option -> 'r option  -> int) -> 'a Lattice.lattice -> ('a,'r) cases -> ('a,'r) cases
(** Remove duplicate results *)

val remove_duplicates_some : ('r -> 'r  -> int) -> 'a Lattice.lattice -> ('a,'r) cases -> ('a,'r) cases
(** Remove non-empty duplicate results *)


val cardinal : ('a,'r) cases -> int
(** Return the number of results *)
