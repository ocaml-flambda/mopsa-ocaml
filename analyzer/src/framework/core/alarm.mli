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


(** {1 Alarm categories} *)
(** ******************** *)

type alarm_category = ..

val pp_alarm_category : Format.formatter -> alarm_category -> unit

val register_alarm_category : ((formatter -> alarm_category -> unit) -> formatter -> alarm_category -> unit) -> unit


(** {1 Alarms} *)
(** ********** *)

type alarm = ..

val category_of_alarm : alarm -> alarm_category

val compare_alarm : alarm -> alarm -> int

val pp_alarm : Format.formatter -> alarm -> unit

val register_alarm_compare : ((alarm -> alarm -> int) -> alarm -> alarm -> int) -> unit

val register_alarm_pp : ((formatter -> alarm -> unit) -> formatter -> alarm -> unit) -> unit

val register_alarm_category : ((alarm -> alarm_category) -> alarm -> alarm_category) -> unit

type alarm_info = {
  category : (alarm -> alarm_category) -> alarm -> alarm_category;
  compare  : (alarm -> alarm -> int) -> alarm -> alarm -> int;
  print    : (formatter -> alarm -> unit) -> formatter -> alarm -> unit;
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


(** {1 Range report} *)
(** **************** *)

module AlarmCategoryMap : MapExtSig.S with type key = alarm_category

module CallstackSet : SetExtSig.S with type elt = callstack

type 'a range_report = {
  range_alarms     : diagnosis AlarmCategoryMap.t;
  range_env        : 'a option;
  range_callstacks : CallstackSet.t;
}

val pp_range_report : (formatter -> 'a -> unit) -> formatter -> 'a range_report -> unit

val join_range_report : 'a lattice -> 'a ctx -> 'a range_report -> 'a range_report -> 'a range_report

val meet_range_report : 'a lattice -> 'a ctx -> 'a range_report -> 'a range_report -> 'a range_report


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

type 'a report = {
  report_map       : 'a range_report RangeMap.t;
  report_soundness : hypothesis Dnf.t;
}

val pp_report : (formatter -> 'a -> unit) -> Format.formatter -> 'a report -> unit

val subset_report : 'a lattice -> 'a ctx -> 'a report -> 'a report -> bool

val join_report : 'a lattice -> 'a ctx -> 'a report -> 'a report -> 'a report

val meet_report : 'a lattice -> 'a ctx -> 'a report -> 'a report -> 'a report

val count_alarms : 'a report -> int * int

