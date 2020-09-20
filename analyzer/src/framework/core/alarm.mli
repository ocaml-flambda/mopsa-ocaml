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

open Lattice
open Context
open Location
open Callstack
open Format


(** {1 Checks} *)
(** ********** *)

type check = ..

val pp_check : Format.formatter -> check -> unit

val register_check : ((formatter -> check -> unit) -> formatter -> check -> unit) -> unit


(** {1 Alarms} *)
(** ********** *)

type alarm_kind = ..

type alarm_kind += A_instance of check

type alarm = {
  alarm_kind      : alarm_kind;
  alarm_check     : check;
  alarm_range     : range;
  alarm_callstack : callstack;
}

val mk_alarm : alarm_kind -> callstack -> range -> alarm

val check_of_alarm : alarm -> check

val range_of_alarm : alarm -> range

val callstack_of_alarm : alarm -> callstack

val compare_alarm : alarm -> alarm -> int

val pp_alarm : Format.formatter -> alarm -> unit

val compare_alarm_kind : alarm_kind -> alarm_kind -> int

val pp_alarm_kind : Format.formatter -> alarm_kind -> unit

val join_alarm_kind : alarm_kind -> alarm_kind -> alarm_kind option

val register_alarm_compare : ((alarm_kind -> alarm_kind -> int) -> alarm_kind -> alarm_kind -> int) -> unit

val register_alarm_pp : ((formatter -> alarm_kind -> unit) -> formatter -> alarm_kind -> unit) -> unit

val register_alarm_check : ((alarm_kind -> check) -> alarm_kind -> check) -> unit

val register_alarm_join : ((alarm_kind -> alarm_kind -> alarm_kind option) -> alarm_kind -> alarm_kind -> alarm_kind option) -> unit

type alarm_info = {
  check   : (alarm_kind -> check) -> alarm_kind -> check;
  compare : (alarm_kind -> alarm_kind -> int) -> alarm_kind -> alarm_kind -> int;
  print   : (formatter -> alarm_kind -> unit) -> formatter -> alarm_kind -> unit;
  join    : (alarm_kind -> alarm_kind -> alarm_kind option) -> alarm_kind -> alarm_kind -> alarm_kind option;
}

val register_alarm : alarm_info -> unit


(** {1 Diagnostic} *)
(** ************** *)

module AlarmSet : SetExtSig.S with type elt = alarm

module CallstackSet : SetExtSig.S with type elt = callstack

type diagnostic_kind =
  | Warning
  | Safe
  | Error
  | Unreachable

type diagnostic = {
  diag_range : range;
  diag_check : check;
  diag_kind : diagnostic_kind;
  diag_alarms : AlarmSet.t;
  diag_callstacks : CallstackSet.t;
}

val mk_safe_diagnostic : check -> callstack -> range -> diagnostic

val mk_error_diagnostic : alarm -> diagnostic

val mk_warning_diagnostic : check -> callstack -> range -> diagnostic

val mk_unreachable_diagnostic : check -> callstack -> range -> diagnostic

val pp_diagnostic : Format.formatter -> diagnostic -> unit

val subset_diagnostic : diagnostic -> diagnostic -> bool

val join_diagnostic : diagnostic -> diagnostic -> diagnostic

val meet_diagnostic : diagnostic -> diagnostic -> diagnostic

val add_alarm_to_diagnostic : alarm -> diagnostic -> diagnostic

val compare_diagnostic : diagnostic -> diagnostic -> int


(** {1 Soundness assumption} *)
(** ************************ *)

type assumption_scope =
  | A_global
  | A_local of range

type assumption_kind = ..

type assumption_kind += A_ignore_unsupported_stmt of Ast.Stmt.stmt
type assumption_kind += A_ignore_unsupported_expr of Ast.Expr.expr

type assumption = {
  assumption_scope : assumption_scope;
  assumption_kind  : assumption_kind;
}

val register_assumption : assumption_kind TypeExt.info -> unit

val pp_assumption_kind : formatter -> assumption_kind -> unit

val pp_assumption : formatter -> assumption -> unit

val compare_assumption_kind : assumption_kind -> assumption_kind -> int

val compare_assumption : assumption -> assumption -> int

val mk_global_assumption : assumption_kind -> assumption

val mk_local_assumption : assumption_kind -> range -> assumption

(** {1 Analysis report} *)
(** ******************* *)

module RangeMap : MapExtSig.S with type key = range

module CheckMap : MapExtSig.S with type key = check

module AssumptionSet : SetExtSig.S with type elt = assumption

type report = {
  report_diagnostics : diagnostic CheckMap.t RangeMap.t;
  report_assumptions : AssumptionSet.t;
}

val empty_report : report

val is_empty_report : report -> bool

val is_safe_report : report -> bool

val is_sound_report : report -> bool

val pp_report : Format.formatter -> report -> unit

val subset_report : report -> report -> bool

val join_report : report -> report -> report

val meet_report : report -> report -> report

val filter_report : (diagnostic -> bool) -> report -> report

val singleton_report : alarm -> report

val add_alarm : ?warning:bool -> alarm -> report -> report

val set_diagnostic : diagnostic -> report -> report

val add_diagnostic : diagnostic -> report -> report

val remove_diagnostic : range -> check -> report -> report

val find_diagnostic : range -> check -> report -> diagnostic

val alarms_of_report : report -> AlarmSet.t

val exists_report : (diagnostic -> bool) -> report -> bool

val forall_report : (diagnostic -> bool) -> report -> bool

val count_alarms : report -> int * int

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

val group_alarms_by_range : AlarmSet.t -> AlarmSet.t RangeMap.t

val group_alarms_by_check : AlarmSet.t -> AlarmSet.t CheckMap.t

val add_assumption : assumption -> report -> report

val add_global_assumption : assumption_kind -> report -> report

val add_local_assumption : assumption_kind -> range -> report -> report
