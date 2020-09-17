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

(** {1 Diagnosis} *)
(** ************* *)

module AlarmSet : SetExtSig.S with type elt = alarm

type diagnosis =
  | Safe
  | Error       of AlarmSet.t
  | Warning     of AlarmSet.t
  | Unreachable

val pp_diagnosis : Format.formatter -> diagnosis -> unit

val subset_diagnosis : diagnosis -> diagnosis -> bool

val join_diagnosis : diagnosis -> diagnosis -> diagnosis

val meet_diagnosis : diagnosis -> diagnosis -> diagnosis

val add_alarm_to_diagnosis : alarm -> diagnosis -> diagnosis


(** {1 Soundness hypothesis} *)
(** ************************ *)

type hypothesis_scope =
  | Hypothesis_global
  | Hypothesis_local of range

type hypothesis_kind = ..

type hypothesis = {
  hypothesis_scope : hypothesis_scope;
  hypothesis_kind  : hypothesis_kind;
}

val register_hypothesis : hypothesis_kind TypeExt.info -> unit

val pp_hypothesis : formatter -> hypothesis -> unit

val compare_hypothesis : hypothesis -> hypothesis -> int


(** {1 Analysis report} *)
(** ******************* *)

module RangeMap : MapExtSig.S with type key = range

module CheckMap : MapExtSig.S with type key = check

module HypothesisSet : SetExtSig.S with type elt = hypothesis

type alarms_report = {
  alarms    : diagnosis CheckMap.t RangeMap.t;
  soundness : HypothesisSet.t;
}

val empty_alarms_report : alarms_report

val is_empty_alarms_report : alarms_report -> bool

val pp_alarms_report : Format.formatter -> alarms_report -> unit

val subset_alarms_report : alarms_report -> alarms_report -> bool

val join_alarms_report : alarms_report -> alarms_report -> alarms_report

val meet_alarms_report : alarms_report -> alarms_report -> alarms_report

val count_alarms : alarms_report -> int * int

val singleton_alarms_report : alarm -> alarms_report

val add_alarm_to_report : alarm -> alarms_report -> alarms_report

val map2zo_alarms_report :
  (range -> check -> diagnosis -> diagnosis) ->
  (range -> check -> diagnosis -> diagnosis) ->
  (range -> check -> diagnosis -> diagnosis -> diagnosis) ->
  alarms_report -> alarms_report -> alarms_report

val fold2zo_alarms_report :
  (range -> check -> diagnosis -> 'b -> 'b) ->
  (range -> check -> diagnosis -> 'b -> 'b) ->
  (range -> check -> diagnosis -> diagnosis -> 'b -> 'b) ->
  alarms_report -> alarms_report -> 'b -> 'b

val alarms_report_to_set : alarms_report -> AlarmSet.t

val group_alarms_set_by_range : AlarmSet.t -> AlarmSet.t RangeMap.t

val group_alarms_set_by_check : AlarmSet.t -> AlarmSet.t CheckMap.t

val find_diagnosis : range -> check -> alarms_report -> diagnosis

val find_alarms : range -> check -> alarms_report -> AlarmSet.t

val set_diagnosis : range -> check -> diagnosis -> alarms_report -> alarms_report

val exists_diagnosis : (range -> check -> diagnosis -> bool) -> alarms_report -> bool

val forall_diagnosis : (range -> check -> diagnosis -> bool) -> alarms_report -> bool

val filter_alarms_report : (range -> check -> diagnosis -> bool) -> alarms_report -> alarms_report
