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

(** Format the results of the analysis in JSON. *)

open Mopsa_utils
open Yojson.Basic
open ArgExt
open Core.All
open Callstack
open Location
open Common

module AlarmKindSet = SetExt.Make(struct type t = alarm_kind let compare = compare_alarm_kind end)


let print out json =
  let channel =
    match out with
    | None -> stdout
    | Some file ->
       open_out_gen [Open_creat; Open_wronly; Open_creat] 0o644 file
  in
  Yojson.Basic.pretty_to_channel channel json

let render_pos pos =
  let file = Location.get_pos_file pos in
  let line = Location.get_pos_line pos in
  let column = Location.get_pos_column pos in
  `Assoc [
    "file", `String file;
    "line", `Int line;
    "column", `Int column;
  ]


let render_range range =
  if not @@ is_orig_range @@ untag_range range then
    `Assoc []
  else
    `Assoc [
        "start", render_pos (Location.get_range_start range);
        "end", render_pos (Location.get_range_end range)
      ]

let render_call (c:callsite)  =
  `Assoc [
    "function", `String c.call_fun_orig_name;
    "range", render_range c.call_range;
  ]

let render_callstack cs  =
  `List (List.map render_call cs)

let aggregate_alarms report =
  RangeCallStackMap.fold
    (fun (range, cs) checks acc ->
       CheckMap.fold
         (fun check diag acc ->
            match diag.diag_kind with
            | Safe | Unreachable -> acc
            | Error | Warning ->
              let csl = AlarmSet.elements diag.diag_alarms |>
                        List.map callstack_of_alarm in
              (range,check,csl) :: acc
         ) checks acc
    ) report.report_diagnostics []

let render_check check =
  let title = Format.asprintf "%a" pp_check check in
  `String title

let render_alarm_messages kinds =
  `String (Format.asprintf "%a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") pp_alarm_kind) kinds)

let render_alarms report =
  RangeCallStackMap.fold
    (fun (range, cs) checks acc ->
      CheckMap.fold
        (fun check diag (safe, total, acc) ->
          match diag.diag_kind with
          | Safe when not !opt_show_safe_checks ->
             safe + 1, total + 1, acc

          | Safe | Error | Warning ->
              (* Get the set of alarms kinds and callstacks *)
              let kinds =
                AlarmSet.fold
                  (fun a kinds ->
                     AlarmKindSet.add a.alarm_kind kinds
                  ) diag.diag_alarms AlarmKindSet.empty in
              (* Join alarm kinds *)
              let rec iter = function
                | [] -> []
                | hd::tl ->
                  let hd',tl' = iter_with hd tl in
                  hd'::iter tl'
              and iter_with a = function
                | [] -> a,[]
                | hd::tl ->
                  match join_alarm_kind a hd with
                  | None    ->
                    let a',tl' = iter_with a tl in
                    a',hd::tl'
                  | Some aa ->
                    let aa',tl' = iter_with aa tl in
                    aa',tl'
              in
              let kinds' = iter (AlarmKindSet.elements kinds) in
              let json_diag =
                `Assoc [
                    "kind", `String (Format.asprintf "%a" pp_diagnostic_kind diag.diag_kind);
                    "title", render_check check;
                    "messages", render_alarm_messages kinds';
                    "range", render_range range;
                    "callstack", render_callstack diag.diag_callstack
                  ]
              in
              (if diag.diag_kind = Safe then safe + 1 else safe),
              total + 1,
              json_diag :: acc
          | _ -> safe, total, acc
        ) checks acc
    ) report.report_diagnostics (0, 0, [])


let render_soudness_assumtion h =
  let msg = Format.asprintf "%a" pp_assumption_kind h.assumption_kind in
  match h.assumption_scope with
  | A_global ->
    `Assoc [
      "message", `String msg;
    ]

  | A_local r ->
    `Assoc [
      "range", render_range r;
      "message", `String msg;
    ]


let render_var var  =
  `String var.vname

let render_value value  =
  `String value

let render_env (var,value)  =
  `Assoc [
    "var", render_var var;
    "val"   , render_value value;
  ]


let report man flow ~time ~files ~out : unit =
  let rep = Flow.get_report flow in
  let safe, total, checks = render_alarms rep in 
  let json  = `Assoc [
      "success", `Bool true;
      "time", `Float time;
      "mopsa_version", `String Version.version;
      "mopsa_dev_version", `String Version.dev_version;
      "files", `List (List.map (fun f -> `String f) files);
      "selectivity", `String (Format.asprintf "%d/%d" safe total);
      "checks", `List checks;
      "assumptions", `List (AssumptionSet.elements rep.report_assumptions |> List.map render_soudness_assumtion );
    ]
  in
  print out json


let panic exn ~btrace ~time ~files ~out =
  let open Exceptions in
  let error,range,cs =
    match exn with
    | Panic (msg, loc) -> msg, None, None
    | PanicAtLocation (range, msg, loc) -> msg,Some range,None
    | PanicAtFrame (range, cs, msg, loc) -> msg,Some range,Some cs
    | SyntaxError(range, msg) -> msg,Some range,None
    | SyntaxErrorList l -> String.concat ", " (List.map snd l),None,None
    |  _ -> Printexc.to_string exn,None,None
  in
  let assoc =
    [ "success", `Bool false;
      "time", `Float time;
      "mopsa_version", `String Version.version;
      "mopsa_dev_version", `String Version.dev_version;
      "files", `List (List.map (fun f -> `String f) files);
      "exception", `String error;
      "backtrace", `String btrace; ]
    @ (match range with
        | None -> []
        | Some r -> [ "range", render_range r ] )
    @ (match cs with
        | None -> []
        | Some c -> [ "callstack", render_callstack c ] )
  in
  print out (`Assoc assoc)


let help (args:arg list) ~out =
  let json  = `List (
      args |>
      List.map (fun arg ->
          `Assoc [
            "key", `String arg.key;
            "doc", `String arg.doc;
            "category", `String arg.category;
            "default", `String arg.default;
            "type", `String (
              match arg.spec with
              | ArgExt.Bool _ -> "bool"
              | ArgExt.Set _ -> "set"
              | ArgExt.Clear _ -> "clear"
              | ArgExt.Unit _ -> "unit"
              | ArgExt.Unit_delayed _ -> "unit"
              | ArgExt.Unit_exit _ -> "unit"
              | ArgExt.String _ -> "string"
              | ArgExt.Set_string _ -> "string"
              | ArgExt.String_list _ -> "string list"
              | ArgExt.Set_string_list _ -> "string list"
              | ArgExt.Int _ -> "int"
              | ArgExt.Set_int _ -> "int"
              | ArgExt.Symbol (l, _) -> "symbol:" ^ (String.concat "," l)
              | ArgExt.Symbol_delayed (l, _) -> "symbol:" ^ (String.concat "," l)
              | ArgExt.Symbol_exit (l, _) -> "symbol:" ^ (String.concat "," l)
            )
          ]
        )
    )
  in
  print out json

let list_domains (domains:string list) ~out =
  let json = `List (
      domains |>
      List.map (fun d -> `String d)
    )
  in
  print out json

let list_reductions (reductions:string list) ~out =
  let json = `List (
      reductions |>
      List.map (fun d -> `String d)
    )
  in
  print out json

let list_checks checks ~out =
  let json = `List (
      List.map render_check checks
    )
  in
  print out json

let list_hooks hooks ~out =
  let json = `List (
      hooks |>
      List.map (fun d -> `String d)
    )
  in
  print out json

let print printer ~range ~out =
  let json =
    `Assoc [
       "print", print_object_to_json (get_printed_object printer);
       "range", render_range range;
     ]
  in
  print out json
