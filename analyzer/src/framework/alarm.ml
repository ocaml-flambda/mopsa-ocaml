(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Alarms reporting potential errors inferred by abstract domains. *)

open Ast
open Manager

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
  pp_token   : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
  pp_title : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
  pp_report : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
}

let compare_chain : (alarm -> alarm -> int) ref =
  ref (fun a1 a2 -> Pervasives.compare a1.alarm_kind a2.alarm_kind)

let pp_token_chain : (Format.formatter -> alarm -> unit) ref =
  ref (fun fmt alarm -> failwith "Pp: Unknown alarm")

let pp_title_chain : (Format.formatter -> alarm -> unit) ref =
  ref (fun fmt alarm -> failwith "Pp: Unknown alarm")

let pp_report_chain : (Format.formatter -> alarm -> unit) ref =
  ref (fun fmt alarm -> failwith "Pp: Unknown alarm")

let register_alarm info =
  compare_chain := info.compare !compare_chain;
  pp_token_chain := info.pp_token !pp_token_chain;
  pp_title_chain := info.pp_title !pp_title_chain;
  pp_report_chain := info.pp_report !pp_report_chain;
  ()

let compare_alarm a1 a2 =
  Compare.compose [
        (fun () -> Compare.list_compare Location.compare_range a1.alarm_trace a2.alarm_trace);
        (fun () -> Pervasives.compare a1.alarm_level a2.alarm_level);
        (fun () -> !compare_chain a1 a2)
      ]

let pp_alarm_token fmt alarm =
  Format.fprintf fmt "%a:%a"
    !pp_token_chain alarm
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ":") Location.pp_range) alarm.alarm_trace

let pp_level fmt = function
  | ERROR -> ((Debug.color "red") Format.pp_print_string) fmt "✘"
  | WARNING -> ((Debug.color "orange") Format.pp_print_string) fmt "⚠"
  | PANIC -> ((Debug.color "red") Format.pp_print_string) fmt "⛔"

let pp_alarm fmt alarm =
  Format.fprintf fmt "%a  %a in %a@\nDescription: @[%a@]@\nTrace: @[%a@]"
    pp_level alarm.alarm_level
    !pp_title_chain alarm
    Location.pp_location_verbose (alarm.alarm_trace |> List.hd |> Location.get_origin_range |> Location.range_begin)
    !pp_report_chain alarm
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") Location.pp_range_verbose) alarm.alarm_trace

type token += T_alarm of alarm

let alarm_token a = T_alarm a

let mk_alarm ?(cs = []) ?(level = WARNING) kind range =
  {
    alarm_kind = kind;
    alarm_level = level;
    alarm_trace = range :: cs;
  }


let () =
  register_token {
    compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_alarm a1, T_alarm a2 -> compare a1 a2
        | _ -> next tk1 tk2
      );
    print = (fun next fmt tk ->
        match tk with
        | T_alarm a -> pp_alarm_token fmt a
        | _ -> next fmt tk
      );
  }
