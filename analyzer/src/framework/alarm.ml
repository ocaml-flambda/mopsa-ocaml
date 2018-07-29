(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Alarms reporting potential errors inferred by abstract domains. *)

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
  alarm_level : alarm_level;
}

(** {2 Registration} *)

type alarm_info = {
  compare : (alarm -> alarm -> int) -> alarm -> alarm -> int;
  print   : (Format.formatter -> alarm -> unit) -> Format.formatter -> alarm -> unit;
}

let compare_chain : (alarm -> alarm -> int) ref =
  ref (fun a1 a2 -> compare a1.alarm_kind a2.alarm_kind)

let pp_chain : (Format.formatter -> alarm -> unit) ref =
  ref (fun fmt alarm -> failwith "Pp: Unknown alarm")

let register info =
  compare_chain := info.compare !compare_chain;
  pp_chain := info.print !pp_chain;
  ()

let compare a1 a2 = !compare_chain a1 a2

let pp_level fmt = function
  | ERROR -> ((Debug.color "red") Format.pp_print_string) fmt "✘"
  | WARNING -> ((Debug.color "orange") Format.pp_print_string) fmt "⚠"
  | PANIC -> ((Debug.color "red") Format.pp_print_string) fmt "⛔"

let print fmt alarm =
  Format.fprintf fmt "%a  @[%a@]"
    pp_level alarm.alarm_level
    !pp_chain alarm
    

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
