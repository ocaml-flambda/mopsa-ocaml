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

(** Alarms - issues found by the analyzer in a program

    During the analysis, domains can perform checks to verify whether some
    property of interest is preserved. Domains declare the list of checks that
    they are responsible for in their header, and can generate 4 types of
    diagnostics for a given check and regarding a given location range:

    - [Safe] indicates that the property of interest is preserved for all
    execution paths reaching the check location.
    - [Error] represent issues that are proven true alarms for all execution
    paths.
    - [Warning] is used for potential alarms that maybe infeasible in some
    execution paths.
    - [Unreachable] indicates that the check location can't be reached in
    any execution path.
 *)

open Mopsa_utils
open Lattice
open Context
open Location
open Callstack
open Format


(** {1 Checks} *)
(** ********** *)

(** Domains can add new checks by extending the type [check] and registering
    them using [register_check].
    Note that checks should be simple variant constructs without any argument.
 *)

type check = ..

val pp_check : Format.formatter -> check -> unit
(** Print a check *)

val register_check : ((formatter -> check -> unit) -> formatter -> check -> unit) -> unit
(** Register a check with its printer *)


(** {1 Alarms} *)
(** ********** *)

(** Alarms are issues related to a check in a given range and a given
    callstack.  Domains can add new kinds of alarms to store information
    related to the issue, such suspect values that raised the alarm.
    Nevertheless, domains can use the generic alarm [A_generic of check] if
    they don't have addition static information to attach to the alarm.
*)

type alarm_kind = ..

type alarm_kind += A_generic of check

type alarm = {
  alarm_kind      : alarm_kind;
  alarm_check     : check;
  alarm_range     : range;
  alarm_callstack : callstack;
}

val mk_alarm : alarm_kind -> callstack -> range -> alarm
(** Create an alarm *)

val check_of_alarm : alarm -> check
(** Return the check associate to an alarm *)

val range_of_alarm : alarm -> range
(** Return the range of an alarm *)

val callstack_of_alarm : alarm -> callstack
(** Return the callstack of an alarm *)

val compare_alarm : alarm -> alarm -> int
(** Compare two alarms *)

val pp_alarm : Format.formatter -> alarm -> unit
(** Print an alarm *)

val compare_alarm_kind : alarm_kind -> alarm_kind -> int
(** Compare two kinds of alarms *)

val pp_alarm_kind : Format.formatter -> alarm_kind -> unit
(** Print an alarm kind *)

val join_alarm_kind : alarm_kind -> alarm_kind -> alarm_kind option
(** Join two alarm kinds by merging the static information attached to them *)

val register_alarm_compare : ((alarm_kind -> alarm_kind -> int) -> alarm_kind -> alarm_kind -> int) -> unit
(** Register a comparison function for alarms *)

val register_alarm_pp : ((formatter -> alarm_kind -> unit) -> formatter -> alarm_kind -> unit) -> unit
(** Register a print function for alarms *)

val register_alarm_check : ((alarm_kind -> check) -> alarm_kind -> check) -> unit
(** Register a function giving the check of an alarm *)

val register_alarm_join : ((alarm_kind -> alarm_kind -> alarm_kind option) -> alarm_kind -> alarm_kind -> alarm_kind option) -> unit
(** Register a join function for alarms *)

(** Registration record for a new kind of alarms *)
type alarm_info = {
  check   : (alarm_kind -> check) -> alarm_kind -> check;
  compare : (alarm_kind -> alarm_kind -> int) -> alarm_kind -> alarm_kind -> int;
  print   : (formatter -> alarm_kind -> unit) -> formatter -> alarm_kind -> unit;
  join    : (alarm_kind -> alarm_kind -> alarm_kind option) -> alarm_kind -> alarm_kind -> alarm_kind option;
}

val register_alarm : alarm_info -> unit
(** Register a new kind of alarms *)


(** {1 Diagnostic} *)
(** ************** *)

(** A diagnostic gives the status of all alarms raised at the program
    location for the same check *)

module AlarmSet : SetExtSig.S with type elt = alarm
(** Set of alarms *)

module CallstackSet : SetExtSig.S with type elt = callstack
(** Set of callstacks *)

(** Kind of a diagnostic *)
type diagnostic_kind =
  | Warning     (** Some executions may have issues *)
  | Safe        (** All executions are safe *)
  | Error       (** All executions do have issues *)
  | Info        (** Some execution has an issue *)
  | Unimplemented (** Some execution hits an unimplemented feature *)
  | Unreachable (** No execution reaches the check point *)

type diagnostic = {
  diag_range : range;
  diag_check : check;
  diag_kind : diagnostic_kind;
  diag_alarms : AlarmSet.t;
  diag_callstacks : CallstackSet.t;
}

val mk_safe_diagnostic : check -> callstack -> range -> diagnostic
(** Create a diagnostic that says that a check is safe *)

val mk_error_diagnostic : alarm -> diagnostic
(** Create a diagnostic that says that a check is unsafe *)

val mk_warning_diagnostic : check -> callstack -> range -> diagnostic
(** Create a diagnostic that says that a check maybe unsafe *)

val mk_unreachable_diagnostic : check -> callstack -> range -> diagnostic
(** Create a diagnostic that says that a check is unreachable *)

val mk_info_diagnostic : alarm -> diagnostic

val mk_unimplemented_diagnostic : check -> callstack -> range -> diagnostic


val pp_diagnostic_kind : Format.formatter -> diagnostic_kind -> unit
(** Print a diagnostic kind *)

val pp_diagnostic : Format.formatter -> diagnostic -> unit
(** Print a diagnostic *)

val subset_diagnostic : diagnostic -> diagnostic -> bool
(** Check whether a diagnostic is covered by another *)

val join_diagnostic : diagnostic -> diagnostic -> diagnostic
(** Compute the union of two diagnostics *)

val meet_diagnostic : diagnostic -> diagnostic -> diagnostic
(** Compute the intersection of two diagnostics *)

val add_alarm_to_diagnostic : alarm -> diagnostic -> diagnostic
(** Add an alarm to a diagnostic *)

val compare_diagnostic : diagnostic -> diagnostic -> int
(** Compare two diagnostics *)


(** {1 Soundness assumption} *)
(** ************************ *)

(** When a domain can't ensure total soundness when analyzing a piece of
    code, it can emit assumptions under which the result is still sound.
*)

(** Scope of an assumption *)
type assumption_scope =
  | A_local of range (** Local assumptions concern a specific location range
                         in the program *)
  | A_global         (** Global assumptions can concern the entire program *)

(** Domains can add new kinds of assumptions by extending the type
    [assumption_kind] *)
type assumption_kind = ..

(** Generic assumptions for specifying potential unsoundness due to
    unsupported statements/expressions *)
type assumption_kind += A_ignore_unsupported_stmt of Ast.Stmt.stmt
type assumption_kind += A_ignore_unsupported_expr of Ast.Expr.expr

type assumption = {
  assumption_scope : assumption_scope;
  assumption_kind  : assumption_kind;
}

val register_assumption : assumption_kind TypeExt.info -> unit
(** Register a new kind of assumptions *)

val pp_assumption_kind : formatter -> assumption_kind -> unit
(** Print an assumption kind *)

val pp_assumption : formatter -> assumption -> unit
(** Print an assumption *)

val compare_assumption_kind : assumption_kind -> assumption_kind -> int
(** Compare two assumption kinds *)

val compare_assumption : assumption -> assumption -> int
(** Compare two assumptions *)

val mk_global_assumption : assumption_kind -> assumption
(** Create a global assumption *)

val mk_local_assumption : assumption_kind -> range -> assumption
(** Create a local assumption *)


(** {1 Analysis report} *)
(** ******************* *)

(** Alarms are organized in a report that maps each range and each check to a
    diagnostic. A report also contains the set of soundness assumptions made
    by the domains.
*)

module RangeMap : MapExtSig.S with type key = range

module CheckMap : MapExtSig.S with type key = check

module AssumptionSet : SetExtSig.S with type elt = assumption

module DiagnosticSet : SetExtSig.S with type elt = diagnostic

type report = {
  report_diagnostics : diagnostic CheckMap.t RangeMap.t;
  report_assumptions : AssumptionSet.t;
}

val empty_report : report
(** Return an empty report *)

val is_empty_report : report -> bool
(** Checks whether a report is empty *)

val is_safe_report : report -> bool
(** Checks whether a report is safe, i.e. it doesn't contain an error or a
   warning *)

val is_sound_report : report -> bool
(** Checks whether a report is sound *)

val pp_report : Format.formatter -> report -> unit
(** Print a report *)

val subset_report : report -> report -> bool
(** [subset_report r1 r2] checks whether report [r1] is included in [r2] *)

val join_report : report -> report -> report
(** Compute the union of two reports *)

val meet_report : report -> report -> report
(** Compute the intersection of two reports *)

val filter_report : (diagnostic -> bool) -> report -> report
(** [filter_report p r] keeps only diagnostics in report [r] that
    verify predicate [p] *)

val singleton_report : alarm -> report
(** Create a report with a single true alarm *)

val add_alarm : ?warning:bool -> alarm -> report -> report
(** [add_alarm a r] adds alarm [a] to a report [r].
    If a diagnostic exists for the same range and the same check as [a],
    [join_diagnostic] is used to join it with an error diagnostic containing [a].
*)

val set_diagnostic : diagnostic -> report -> report
(** [set_diagnostic d r] adds diagnostic [d] to [r].
    Any existing diagnostic for the same range and the same check as [d] is removed.
 *)

val add_diagnostic : diagnostic -> report -> report
(** [add_diagnostic d r] adds diagnostic [d] to [r].
    If a diagnostic exists for the same range and the same check as [d],
    [join_diagnostic] is used to join it with [d].
 *)

val remove_diagnostic : diagnostic -> report -> report
(** Remove a diagnostic from a report *)

val find_diagnostic : range -> check -> report -> diagnostic
(** [find_diagnostic range chk r] finds the diagnostic of check [chk] at
    location [range] in report [r] *)

val diagnostics_of_report : report -> DiagnosticSet.t
(** Return the set of diagnostics in a report *)

val exists_report : (diagnostic -> bool) -> report -> bool
(** Check whether any diagnostic verifies a predicate *)

val forall_report : (diagnostic -> bool) -> report -> bool
(** Check whether all diagnostics verify a predicate *)

val count_alarms : report -> int * int
(** Count the number of alarms and warnings in a report *)

val group_diagnostics_by_range : DiagnosticSet.t -> DiagnosticSet.t RangeMap.t
(** Group diagnostics by their range *)

val group_diagnostics_by_check : DiagnosticSet.t -> DiagnosticSet.t CheckMap.t
(** Group diagnostics by their check *)

val add_assumption : assumption -> report -> report
(** Add an assumption to a report *)

val add_global_assumption : assumption_kind -> report -> report
(** Add a global assumption to a report *)

val add_local_assumption : assumption_kind -> range -> report -> report
(** Add a local assumption to a report *)

val map2zo_report :
  (diagnostic -> diagnostic) ->
  (diagnostic -> diagnostic) ->
  (diagnostic -> diagnostic -> diagnostic) ->
  report -> report -> report

val fold2zo_report :
  (diagnostic -> 'b -> 'b) ->
  (diagnostic -> 'b -> 'b) ->
  (diagnostic -> diagnostic -> 'b -> 'b) ->
  report -> report -> 'b -> 'b
