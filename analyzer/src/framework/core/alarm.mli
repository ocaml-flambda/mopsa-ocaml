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

type alarm_level =
  | ERROR
  | WARNING
  | PANIC


type alarm = {
  alarm_kind : alarm_kind;   (** the kind of the alarm *)
  alarm_level : alarm_level;
  alarm_trace : Location.range * Callstack.cs;
}


type alarm_info = {
  compare    : (alarm -> alarm -> int) -> alarm -> alarm -> int;
  pp_token   : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
  pp_title : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
  pp_report  : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
}

val register_alarm: alarm_info -> unit
(** Register a new alarm *)

val pp_alarm : Format.formatter -> alarm -> unit
(** Pretty print the report of an alarm *)

val pp_alarm_title : Format.formatter -> alarm -> unit

type Token.token += T_alarm of alarm

val alarm_token : alarm -> Token.token

val mk_alarm : alarm_kind ->  ?cs:Callstack.cs -> ?level:alarm_level -> Location.range -> alarm

(* val raise_alarm : alarm_kind -> Location.range -> ?level:alarm_level -> ?bottom:bool -> ('a, 't) Manager.man -> 'a Flow.flow -> 'a Flow.flow *)
