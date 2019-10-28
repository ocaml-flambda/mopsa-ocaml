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

(** Alarms reporting potential errors inferred by abstract domains. *)

open Location

type alarm_category = ..
(** Categories of alarms *)

type alarm_detail = ..
(** Detail information of alarms *)

type alarm
(** Alarms *)

val mk_alarm : alarm_category -> ?detail:alarm_detail -> ?cs:Callstack.cs -> range -> alarm
(** Create an alarm instance *)

val get_alarm_category : alarm -> alarm_category
(** Get the category of an alarm *)

val get_alarm_detail : alarm -> alarm_detail
(** Get the detail of an alarm *)

val get_alarm_trace : alarm -> range * Callstack.cs
(** Get the trace of an alarm *)

val register_alarm_category: alarm_category TypeExt.info -> unit
(** Register a new alarm category *)

val register_alarm_detail: alarm_detail TypeExt.info -> unit
(** Register a new alarm detail *)

val pp_alarm : Format.formatter -> alarm -> unit
(** Pretty print an alarm *)

val pp_alarm_category : Format.formatter -> alarm_category -> unit
(** Pretty print an alarm category *)


module AlarmSet : SetExtSig.S with type elt = alarm
(** Sets of alarms *)

module AlarmMap :
sig
  type t
  val cardinal : t -> int
  val of_set : AlarmSet.t -> t
  val print : Format.formatter -> t -> unit
end
(** Maps from alarms categories to sets of alarms *)


