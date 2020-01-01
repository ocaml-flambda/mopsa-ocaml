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

(** Alarms of potential runtime errors inferred by the analysis. *)



(** {2 Alarm classes} *)
(** ******************** *)

(** Alarm are categorized into a finite set of classes *)
type alarm_class = ..


(** Pretty printer of alarm classes *)
val pp_alarm_class : Format.formatter -> alarm_class -> unit


(** Register an new alarm class. There is no need for registering a
    compare function, since classes are simple variants without
    arguments. *)
val register_alarm_class : alarm_class TypeExt.print -> unit



(** {2 Alarm bodies} *)
(** **************** *)

(** Alarm body provides details about the context of the alarm, such
    as expression intervals, expected values, etc.*)
type alarm_body = ..


(** Compare two alarm bodies *)
val compare_alarm_body : alarm_body -> alarm_body -> int


(** Pretty printer of alarm body *)
val pp_alarm_body : Format.formatter -> alarm_body -> unit


(** Chaining function to get the class of an alarm body *)
type alarm_classifier = (alarm_body -> alarm_class) -> alarm_body -> alarm_class


(** Get the class of an alarm body *)
val classify_alarm_body : alarm_body -> alarm_class



(** Registration information of an alarm body *)
type alarm_body_info = {
  classifier: alarm_classifier;
  compare : alarm_body TypeExt.compare;
  print : alarm_body TypeExt.print;
}
  

(** Register a new alarm body *)
val register_alarm_body : alarm_body_info -> unit



(** {2 Alarm instance} *)
(** ****************** *)

(** Alarm instance *)
type alarm


(** Create an alarm instance *)
val mk_alarm : alarm_body -> ?cs:Callstack.cs -> Location.range -> alarm


(** Get the class of an alarm *)
val get_alarm_class : alarm -> alarm_class

(** Get the body of an alarm *)
val get_alarm_body : alarm -> alarm_body

(** Get the range of an alarm *)
val get_alarm_range : alarm -> Location.range

(** Get the callstack of an alarm *)
val get_alarm_callstack : alarm -> Callstack.cs

(** Compare two alarms *)
val compare_alarm : alarm -> alarm -> int

(** Pretty printer of alarms *)
val pp_alarm : Format.formatter -> alarm -> unit


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
end

module RangeMap : SETMAP with type k = Location.range
module ClassMap : SETMAP with type k = alarm_class


val index_alarm_set_by_range : AlarmSet.t -> RangeMap.t

val index_alarm_set_by_class : AlarmSet.t -> ClassMap.t

val count_alarms : AlarmSet.t -> int
