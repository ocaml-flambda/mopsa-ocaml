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

open Yojson.Basic
open ArgExt
open Core.Alarm
open Core.Soundness
open Callstack


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

let aggregate_alarms alarms =
  (* Iterate first on the alarm classes *)
  let cls_map = index_alarm_set_by_class alarms in
  ClassMap.fold
    (fun cls alarms acc ->
       (* Then iterate on the location ranges within each class *)
       let range_map = index_alarm_set_by_range alarms in
       RangeMap.fold
         (fun range alarms acc ->
            let csl = AlarmSet.elements alarms |>
                      List.map get_alarm_callstack in
            (cls,range,csl) :: acc
         ) range_map acc
    ) cls_map []

let render_alarm_class alarm =
  let title = Format.asprintf "%a" pp_alarm_class alarm in
  `String title

let render_alarm (cls,range,csl) =
  `Assoc [
    "title", render_alarm_class cls;
    "range", render_range range;
    "callstacks", `List (List.map render_callstack csl);
  ]

let render_warning w  =
  match w.warn_range with
  | None ->
    `Assoc [
      "message", `String w.warn_message;
    ]

  | Some r ->
    `Assoc [
      "message", `String w.warn_message;
      "range", render_range r;
    ]
    

let render_var var  =
  `String var.Ast.Var.vname

let render_value value  =
  `String value

let render_env (var,value)  =
  `Assoc [
    "var", render_var var;
    "val"   , render_value value;
  ]


let report ?(flow=None) man alarms time files out : unit =
  let json  = `Assoc [
      "success", `Bool true;
      "time", `Float time;
      "files", `List (List.map (fun f -> `String f) files);
      "alarms", `List (aggregate_alarms alarms |> List.map render_alarm);
      "warnings", `List (List.map render_warning (get_warnings ()));
    ]
  in
  print out json


let panic ~btrace exn files time out =
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


let help (args:arg list) out =
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
              | ArgExt.String _ -> "string"
              | ArgExt.Set_string _ -> "string"
              | ArgExt.String_list _ -> "string list"
              | ArgExt.Set_string_list _ -> "string list"
              | ArgExt.Int _ -> "int"
              | ArgExt.Set_int _ -> "int"
              | ArgExt.Symbol (l, _) -> "symbol:" ^ (String.concat "," l)
            )
          ]
        )
    )
  in
  print out json

let list_domains (domains:string list) out =
  let json = `List (
      domains |>
      List.map (fun d -> `String d)
    )
  in
  print out json

let list_alarms alarms out =
  let json = `List (
      List.map render_alarm_class alarms
    )
  in
  print out json

let list_hooks hooks out =
  let json = `List (
      hooks |>
      List.map (fun d -> `String d)
    )
  in
  print out json

let print range printer flow out =
  printer Format.str_formatter flow;
  let str = Format.flush_str_formatter () in
  let json =
    `Assoc [
       "channel", `String "print";
       "range", render_range range;
       "msg", `String str;
     ]
  in
  print out json
