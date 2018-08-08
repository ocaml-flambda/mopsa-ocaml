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
}

type alarm_info = {
  compare : (alarm -> alarm -> int) -> alarm -> alarm -> int;
  print   : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
}

val register_alarm: alarm_info -> unit

val print : Format.formatter -> alarm -> unit

val compare : alarm -> alarm -> int

type _ Query.query +=
  | Q_alarms: (alarm list) Query.query
  (** Query to extract collected alarms *)
