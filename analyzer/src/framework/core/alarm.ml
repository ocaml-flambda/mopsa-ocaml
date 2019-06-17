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

open Ast
open Token
open Lattice

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
  if a1 == a2 then 0
  else Compare.compose [
      (fun () -> Compare.pair Location.compare_range Callstack.compare a1.alarm_trace a2.alarm_trace);
      (fun () -> Pervasives.compare a1.alarm_level a2.alarm_level);
      (fun () -> !compare_chain a1 a2)
    ]

let pp_alarm_token fmt alarm =
  Format.fprintf fmt "%a:%a:%a"
    !pp_token_chain alarm
    Location.pp_range (fst alarm.alarm_trace)
    Callstack.pp_call_stack (snd alarm.alarm_trace)

let pp_level fmt = function
  | ERROR -> ((Debug.color "red") Format.pp_print_string) fmt "✘"
  | WARNING -> ((Debug.color "orange") Format.pp_print_string) fmt "⚠"
  | PANIC -> ((Debug.color "red") Format.pp_print_string) fmt "⛔"

let pp_callstack fmt (cs:Callstack.cs) =
  (* print in the style of gcc's preprocessor include stack *)
  List.iter
    (fun c -> Format.fprintf fmt "\tfrom %a: %s@\n" Location.pp_range c.Callstack.call_site c.Callstack.call_fun)
    cs

let pp_alarm fmt alarm =
  (* print using a format recognized by emacs: location first *)
  Format.fprintf fmt "%a: %a %a@\n%a"
    Location.pp_range (alarm.alarm_trace |> fst |> Location.untag_range)
    pp_level alarm.alarm_level
    !pp_title_chain alarm
    pp_callstack (alarm.alarm_trace |> snd)

let pp_alarm_title fmt alarm = !pp_title_chain fmt alarm

type token += T_alarm of alarm

let alarm_token a = T_alarm a

let mk_alarm kind ?(cs = Callstack.empty) ?(level = WARNING) range =
  {
    alarm_kind = kind;
    alarm_level = level;
    alarm_trace = (range, cs);
  }

let raise_alarm akind range ?(level = WARNING) ?(bottom=true) (lattice:'a lattice) flow =
  let cs = Callstack.get flow in
  let alarm = mk_alarm akind range ~cs in
  let tk = alarm_token alarm in
  let a1 = Flow.get tk lattice flow in
  let a2 = Flow.get T_cur lattice flow in
  let aa = lattice.join (Flow.get_ctx flow |> Context.get_unit) a1 a2 in
  let flow' = Flow.set tk aa lattice flow in
  if bottom then
    Flow.set T_cur lattice.bottom lattice flow'
  else
    flow'


let () =
  register_token {
    compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_alarm a1, T_alarm a2 -> compare_alarm a1 a2
        | _ -> next tk1 tk2
      );
    print = (fun next fmt tk ->
        match tk with
        | T_alarm a -> pp_alarm_token fmt a
        | _ -> next fmt tk
      );
  }
