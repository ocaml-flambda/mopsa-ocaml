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
                           (** {2 Type} *)
(*==========================================================================*)

(** Extensible type of alarm kinds, defined by domains. *)
type alarm_kind = ..

type alarm_level =
  | ERROR
  | WARNING
  | PANIC
  
(** An alarm *)
type alarm = {
  alarm_kind : alarm_kind;   (** the kind of the alarm *)
  alarm_range : range;       (** the range of the program where the alarm was detected *)
  alarm_level : alarm_level;
}

let alarm_compare_chain : (alarm -> alarm -> int) ref = ref (fun a1 a2 -> compare a1.alarm_kind a2.alarm_kind)

let register_alarm_compare cmp =
  alarm_compare_chain := cmp !alarm_compare_chain

let compare_alarm a1 a2 =
  compare_composer [
    (fun () -> compare_range a1.alarm_range a2.alarm_range);
    (fun () -> !alarm_compare_chain a1 a2);
  ]



(*==========================================================================*)
                           (** {2 Query} *)
(*==========================================================================*)


(**
   Query used by {!Main} to extract all alarms from used domains.
*)
type _ Query.query +=
  | QGetAlarms: (alarm list) Query.query

let () =
  Query.(
    register_reply_manager {
      domatch = (let check : type a. a query -> (a, alarm list) eq option =
                   function
                   | QGetAlarms -> Some Eq
                   | _ -> None
                 in
                 check
                );
      join = (fun al1 al2 ->
          al1 @ al2
        );

      meet = (fun al1 al2 ->
          List.filter (fun a -> List.mem a al2) al1
        );
    }
  )

(*==========================================================================*)
                           (** {2 Printing} *)
(*==========================================================================*)

let pp_alarm_chain : (Format.formatter -> alarm -> unit) ref = ref (fun fmt alarm ->
  failwith "Pp: Unknown alarm"
  )

let register_pp_alarm pp = pp_alarm_chain := pp !pp_alarm_chain

let pp_alarm_level fmt = function
  | ERROR -> ((Debug.color "red") Format.pp_print_string) fmt "✘"
  | WARNING -> ((Debug.color "orange") Format.pp_print_string) fmt "⚠"
  | PANIC -> ((Debug.color "red") Format.pp_print_string) fmt "⛔"

let pp_alarm fmt alarm =
  Format.fprintf fmt "%a  @[%a@]@\nIn %a"
    pp_alarm_level alarm.alarm_level
    !pp_alarm_chain alarm
    Pp.pp_range_verbose alarm.alarm_range

let pp_alarm_bench fmt alarm =
  Format.fprintf fmt "{\"type\": \"%a\",\"range\": \"%a\"}"
    !pp_alarm_chain alarm
    Pp.pp_range_verbose alarm.alarm_range
