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

(** Alarms are potential bugs in the target program. They are reported
    by abstract domains during the analysis. *)



(** {2 Alarm classes} *)
(** ***************** *)

(** Classes of reported alarms. This extensible type should contain
   simple constructors without arguments to simplify declaration of
   alarms handled by abstract domains. *)
type alarm_class = ..


(** Pretty printer of alarm classes *)
val pp_alarm_class : Format.formatter -> alarm_class -> unit


(** Register an new alarm class. There is no need for registering a
    compare function, since classes are simple constructors without
    arguments. *)
val register_alarm_class : alarm_class TypeExt.print -> unit



(** {2 Alarm messages} *)
(** **************** *)

(** Alarm messages provide details about an alarm, such as values of expression, etc.*)
type alarm_message = ..


(** Chaining function to get the class of an alarm message *)
type alarm_classifier = (alarm_message -> alarm_class) -> alarm_message -> alarm_class


(** Get the class of an alarm message *)
val classify_alarm_message : alarm_message -> alarm_class


(** Compare two alarm messages *)
val compare_alarm_message : alarm_message -> alarm_message -> int


(** Pretty printer of a collection of alarm messages of the same alarm class raised at the same program location. *)
val pp_grouped_alarm_message : alarm_class -> Format.formatter -> alarm_message list -> unit


(** Registration information of a grouped alarm message *)
type grouped_alarm_message_info = {
  classifier: alarm_classifier;
  compare : alarm_message TypeExt.compare;
  print : (Format.formatter -> alarm_message list -> alarm_class -> unit) -> Format.formatter -> alarm_message list -> alarm_class -> unit;
}

(** Registration information of a non-grouped alarm message *)
type alarm_message_info = {
  classifier: alarm_classifier;
  compare : alarm_message TypeExt.compare;
  print : alarm_message TypeExt.print;
}


(** Register a new alarm message *)
val register_alarm_message : alarm_message_info -> unit


(** Register a new grouped alarm message *)
val register_grouped_alarm_message : grouped_alarm_message_info -> unit



(** {2 Alarm instance} *)
(** ****************** *)

(** Alarm instance *)
type alarm


(** Create an alarm instance *)
val mk_alarm : alarm_message -> Callstack.callstack -> Location.range -> alarm

(** Get the class of an alarm *)
val get_alarm_class : alarm -> alarm_class

(** Get the message of an alarm *)
val get_alarm_message : alarm -> alarm_message

(** Get the range of an alarm *)
val get_alarm_range : alarm -> Location.range

(** Get the callstack of an alarm *)
val get_alarm_callstack : alarm -> Callstack.callstack

(** Compare two alarms *)
val compare_alarm : alarm -> alarm -> int


(** Sets of alarms *)
module AlarmSet : SetExtSig.S with type elt = alarm


(** {2 Alarm indexation} *)
(** ******************** *)

module type SETMAP =
sig
  type k
  type t
  val of_set : AlarmSet.t -> t
  val cardinal : t -> int
  val bindings : t -> (k*AlarmSet.t) list
  val fold : (k -> AlarmSet.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (k -> AlarmSet.t -> unit) -> t -> unit
end

module RangeMap : SETMAP with type k = Location.range
module ClassMap : SETMAP with type k = alarm_class


val index_alarm_set_by_range : AlarmSet.t -> RangeMap.t

val index_alarm_set_by_class : AlarmSet.t -> ClassMap.t

val count_alarms : AlarmSet.t -> int
