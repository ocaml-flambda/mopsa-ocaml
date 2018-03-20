(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Reporting of potential errors inferred by abstract domains. *)

open Ast

(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

(** Extensible type of alarm kinds, defined by domains. *)
type alarm_kind = ..

(** Severity level of an alarm. *)
type alarm_level =
  | High
  | Medium
  | Low
  | Unknown

(** An alarm *)
type alarm = {
  alarm_kind : alarm_kind;   (** the kind of the alarm *)
  alarm_range : range;       (** the range of the program where the alarm
                            was detected *)
  alarm_level : alarm_level; (** the level of the alarm *)
}

(**
   Domain query used by {!Main} to extract all alarms from the domains
    of an analysis
*)
type _ Query.query +=
  | QGetAlarms: (alarm list) Query.query



(*==========================================================================*)
                           (** {2 Printing} *)
(*==========================================================================*)

let pp_level fmt = function
  | High -> Format.pp_print_string fmt "high"
  | Medium -> Format.pp_print_string fmt "medium"
  | Low -> Format.pp_print_string fmt "low"
  | Unknown -> Format.pp_print_string fmt "unknown"

let pp_chain : (Format.formatter -> alarm -> unit) ref = ref (fun fmt alarm ->
  failwith "Pp: Unknown alarm"
  )

let register_pp pp = pp_chain := pp !pp_chain

let pp_alarm fmt alarm =
  Format.fprintf fmt "@[%a@]@\nLevel: %a@\n%a"
    !pp_chain alarm
    pp_level alarm.alarm_level
    Pp.pp_range_verbose alarm.alarm_range
