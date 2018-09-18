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
  alarm_trace : Location.range list;
}


type alarm_info = {
  compare : (alarm -> alarm -> int) -> alarm -> alarm -> int;
  print   : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
  report : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
}

val register_alarm: alarm_info -> unit
(** Register a new alarm *)

val pp_report : Format.formatter -> alarm -> unit
(** Pretty print the report of an alarm *)

type Manager.token += T_alarm of alarm

val alarm_token : alarm -> Manager.token

val mk_alarm :  ?cs:(Location.range list) -> ?level:alarm_level -> alarm_kind -> Location.range -> alarm
