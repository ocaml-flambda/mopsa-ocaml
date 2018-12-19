(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
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
  alarm_trace : Location.range * Callstack.t;
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

type Manager.token += T_alarm of alarm

val alarm_token : alarm -> Manager.token

val mk_alarm : alarm_kind ->  ?cs:Callstack.t -> ?level:alarm_level -> Location.range -> alarm

val raise_alarm : alarm_kind -> Location.range -> ?level:alarm_level -> ?bottom:bool -> ('a, 't) Manager.man -> 'a Flow.flow -> 'a Flow.flow
