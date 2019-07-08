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



type alarm_kind = ..

type alarm_extra = ..

type alarm_level =
  | ERROR
  | WARNING


type alarm

val mk_alarm :
  alarm_kind -> ?extra:alarm_extra -> ?level:alarm_level ->
  ?cs:Callstack.cs -> Location.range -> alarm

val get_alarm_kind : alarm -> alarm_kind

val get_alarm_extra : alarm -> alarm_extra

val get_alarm_trace : alarm -> Location.range * Callstack.cs

val get_alarm_level : alarm -> alarm_level

val register_alarm_kind: alarm_kind TypeExt.info -> unit
(** Register a new alarm kind *)

val register_alarm_extra: alarm_extra TypeExt.info -> unit
(** Register a new alarm extra *)

val pp_alarm : Format.formatter -> alarm -> unit
(** Pretty print an alarm *)

val pp_alarm_kind : Format.formatter -> alarm_kind -> unit


module AlarmSet :
sig
  type t

  val singleton : alarm -> t

  val empty : t

  val is_empty : t -> bool

  val cardinal : t -> int

  val elements : t -> alarm list

  val add : alarm -> t -> t

  val join : t -> t -> t

  val join_list : t list -> t

  val meet : t -> t -> t

  val meet_list : t list -> t

  val diff : t -> t -> t

  val print : Format.formatter -> t -> unit

end
