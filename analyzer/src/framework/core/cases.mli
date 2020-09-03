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
open Lattice

type cleaners = StmtSet.t

(** A single case of a computation *)
type 'r case =
  | Result of 'r * log * cleaners
  (** Actual result of the computation, with logs and cleaners *)

  | Empty
  (** Empty results due to non-terminating computations (e.g. alarms) *)

  | NotHandled
  (** This means that the domain can't process this case. The analyzer
      can ask other domains to handle it. *)

type ('a,'r) cases
(** Cases of results ['r] over an abstraction ['a] *)

val case : 'r case -> 'a flow -> ('a,'r) cases
(** Create a case. *)

val return : ?log:log -> ?cleaners:stmt list -> 'r -> 'a flow -> ('a,'r) cases
val singleton : ?log:log -> ?cleaners:stmt list -> 'r -> 'a flow -> ('a,'r) cases
(** Create a case with a single non-empty result. *)

val empty : ?bottom:bool -> 'a flow -> ('a,'r) cases
(** Create a case with an empty case. *)

val not_handled : 'a flow -> ('a,'r) cases
(** Create a non-handled case to be forwarded to other domains  *)

val remove_duplicates : ('r case -> 'r case  -> int) -> 'a Lattice.lattice -> ('a,'r) cases -> ('a,'r) cases
val remove_duplicate_results : ('r -> 'r  -> int) -> 'a Lattice.lattice -> ('a,'r) cases -> ('a,'r) cases
(** Remove duplicate results *)

val cardinal : ('a,'r) cases -> int
(** Return the number of results *)


(** {1 Cleaners} *)
(** ************ *)

val opt_clean_cur_only : bool ref
(** Option to apply cleaners on T_cur only *)

val add_cleaners : stmt list -> ('a,'r) cases -> ('a,'r) cases
(** [add_cleaners block c] adds cleaner statements [block] to cases [c]. *)

val get_case_cleaners : 'r case -> cleaners
(** Get the set of cleaners attached to a case *)

val set_case_cleaners : cleaners -> 'r case -> 'r case
(** Set the set of cleaners attached to a case *)

val set_cleaners : stmt list -> ('a,'r) cases -> ('a,'r) cases
(** Set the same cleaners for all cases *)

(** {1 Context} *)
(** *********** *)

val get_ctx : ('a,'r) cases -> 'a ctx
(** [get_ctx c] returns the context of cases [c]. *)

val set_ctx : 'a ctx -> ('a,'r) cases -> ('a,'r) cases
(** [set_ctx ctx c] changes the context of cases [c] to [ctx]. *)

val copy_ctx : ('a,'r) cases -> ('a,'s) cases -> ('a,'s) cases
(** [copy_ctx c1 c2] changes the context of cases [c2] to the context of [c1]. *)

val get_callstack: ('a,'r) cases -> callstack
(** [get_callstack c] returns the callstack of cases [c]. *)


(** {1 Logs} *)
(** ******** *)

val get_case_log : 'r case -> log
(** Get the logs attached to a case *)

val set_case_log : log -> 'r case -> 'r case
(** Set the logs attached to a case *)

val map_log : (log -> log) -> ('a,'r) cases -> ('a,'r) cases
(** [map_log f c] replaces each log [l] in [c] with [f l]. *)

val set_log : log -> ('a,'r) cases -> ('a,'r) cases
(** Set the same logs for all cases *)


(** {1 Lattice operators} *)
(** ********************* *)

val join : ('a,'r) cases -> ('a,'r) cases -> ('a,'r) cases
(** Join two cases. *)

val meet : ('a,'r) cases -> ('a,'r) cases -> ('a,'r) cases
(** Meet two cases. *)

val join_list : empty:(unit -> ('a,'r) cases) -> ('a,'r) cases list -> ('a,'r) cases
(** Join a list of cases. *)

val meet_list : empty:(unit -> ('a,'r) cases) -> ('a,'r) cases list -> ('a,'r) cases
(** Meet a list of cases. *)


(** {1 Iterators} *)
(** ************* *)

val map: ('r case -> 'a flow -> 's case * 'a flow) -> ('a,'r) cases -> ('a,'s) cases
(** [map f c] replaces each case [ci*flow] in [c] with [f ci flow] *)

val map_result : ('r -> 's) -> ('a,'r) cases -> ('a,'s) cases
(** [map_result f c] is similar to [map f c], except that empty and
    not-handled cases are kept unmodified. *)

val map_conjunction:
  (('r case * 'a flow) list -> ('s case * 'a flow) list) ->
  ('a,'r) cases -> ('a,'s) cases
(** [map_conjunction f c] replaces each conjunction [conj] in [c] with [f conj] *)

val map_disjunction:
  (('r case * 'a flow) list -> ('s case * 'a flow) list) ->
  ('a,'r) cases -> ('a,'s) cases
(** [map_disjunction f c] replaces each disjunction [disj] in [c] with [f disj] *)

val reduce :
  ('r case -> 'a flow -> 'b) ->
  join:('b -> 'b -> 'b) ->
  meet:('b -> 'b -> 'b) ->
  ('a,'r) cases -> 'b
(** [reduce f ~join ~meet c] collapses cases in [c] to a single value by
    applying [f] on each case and merging outputs using [join] and [meet].
*)

val reduce_result :
  ('r -> 'a flow -> 'b) ->
  join:('b -> 'b -> 'b) ->
  meet:('b -> 'b -> 'b) ->
  bottom:'b ->
  ('a,'r) cases -> 'b
(** [reduce_result f ~join ~meet bottom c] is similar to [reduce f
    join meet c], except that empty and not-handled cases are replaced
    with [bottom].
*)

val fold : ('b -> 'r case -> 'a flow -> 'b) -> 'b -> ('a,'r) cases -> 'b
(** Fold over the flattened list of cases *)

val fold_result : ('b -> 'r -> 'a flow -> 'b) -> 'b -> ('a,'r) cases -> 'b
(** Fold over the flattened list of results *)

val partition : ('r case -> 'a flow -> bool) -> ('a,'r) cases -> ('a,'r) cases option * ('a,'r) cases option
(** [partition f cases] separates cases that verify or not predicate [f]  *)

val for_all : ('r case -> 'a flow -> bool) -> ('a,'r) cases -> bool
(** Check whether a predicate is valid over all cases *)

val for_all_result : ('r -> 'a flow -> bool) -> ('a,'r) cases -> bool
(** Check whether a predicate is valid over all results *)

val exists : ('r case -> 'a flow -> bool) -> ('a,'r) cases -> bool
(** Check whether a predicate is valid for at least one case *)

val exists_result : ('r -> 'a flow -> bool) -> ('a,'r) cases -> bool
(** Check whether a predicate is valid for at least one result *)

val print :
  (Format.formatter -> 'r case -> 'a flow -> unit) ->
  Format.formatter -> ('a,'r) cases -> unit
(** Pretty printer of cases *)


val print_result :
  (Format.formatter -> 'r -> 'a flow -> unit) ->
  Format.formatter ->
  ('a,'r) cases ->
  unit
(** Pretty printer of results *)




(****************************************************************************)
(**                        {2 Monadic binders}                              *)
(****************************************************************************)

val bind_opt :
  ('r case -> 'a flow -> ('a,'s) cases option) ->
  ('a,'r) cases ->
  ('a,'s) cases option
(** [bind_opt f cases] substitutes each case [(c,flow)] in [cases]
    with [f c flow]. If the function returns [None], the case becomes
    [NotHandled]. Logs and cleaners returned by [f] are concatenated
    with the previous ones in [cases]. *)

val (>>=?) :
  ('a,'r) cases ->
  ('r case -> 'a flow -> ('a,'s) cases option) ->
  ('a,'s) cases option
(** Bind operator for [bind_opt] *)

val bind :
  ('r case -> 'a flow -> ('a,'s) cases) ->
  ('a,'r) cases ->
  ('a,'s) cases
(** [bind f cases] substitutes each case [(c,flow)] in [cases] with [f c flow].
    Logs and cleaners returned by [f] are concatenated with the previous ones in [cases]. *)

val (>>=) :
  ('a,'r) cases ->
  ('r case -> 'a flow -> ('a,'s) cases) ->
  ('a,'s) cases
(** Bind operator for [bind] *)

val bind_result_opt :
  ('r -> 'a flow -> ('a,'s) cases option) ->
  ('a,'r) cases ->
  ('a,'s) cases option
  (** Same as [bind_opt], but empty and not-handled cases are preserved and are not passed to [f]. *)

val (>>$?) :
  ('a,'r) cases ->
  ('r -> 'a flow -> ('a,'s) cases option) ->
  ('a,'s) cases option
(** Bind operator for [bind_result_opt] *)

val bind_result :
  ('r -> 'a flow -> ('a,'s) cases) ->
  ('a,'r) cases ->
  ('a,'s) cases
(** Same as [bind], but empty and not-handled cases are preserved and are not passed to [f]. *)

val (>>$) :
  ('a,'r) cases ->
  ('r -> 'a flow -> ('a,'s) cases) ->
  ('a,'s) cases
(** Bind operator for [bind_result] *)

val bind_conjunction :
  (('r case * 'a flow) list -> ('a,'s) cases) ->
  ('a,'r) cases -> ('a,'s) cases
(** [bind_conjunction f cases] substitutes each conjunction of cases [conj] in [cases] with [f conj].
    Logs and cleaners returned by [f] are concatenated with the previous ones in [cases]. *)

val bind_conjunction_result :
  ('r list -> 'a flow -> ('a,'s) cases) ->
  'a lattice ->
  ('a,'r) cases -> ('a,'s) cases
(** Same as [bind_conjunction] f cases] but empty and not-handled cases are preserved and are not passed to [f]. *)

val bind_disjunction :
  (('r case * 'a flow) list -> ('a,'s) cases) ->
  ('a,'r) cases -> ('a,'s) cases
(** [bind_disjunction f cases] substitutes each disjunction of cases [disj] in [cases] with [f disj].
    Logs and cleaners returned by [f] are concatenated with the previous ones in [cases]. *)

val bind_disjunction_result :
  ('r list -> 'a flow -> ('a,'s) cases) ->
  'a lattice ->
  ('a,'r) cases -> ('a,'s) cases
(** Same as [bind_disjunction] f cases] but empty and not-handled cases are preserved and are not passed to [f]. *)

val bind_list_opt :
  'r list -> ('r -> 'a flow -> ('a,'s) cases option) ->
  'a flow -> ('a, 's list) cases option
(** Bind a list of results with a partial transfer function. *)

val bind_list :
  'r list -> ('r -> 'a flow -> ('a,'s) cases) ->
  'a flow -> ('a, 's list) cases
(** Bind a list of results with a transfer function *)
