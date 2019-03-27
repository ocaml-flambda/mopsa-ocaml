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

let print json out =
  let channel =
    match out with
    | None -> stdout
    | Some file -> open_out file
  in
  to_channel channel json

let render_pos pos : json =
  let file = Location.get_pos_file pos in
  let line = Location.get_pos_line pos in
  let column = Location.get_pos_column pos in
  `Assoc [
    "file", `String file;
    "line", `Int line;
    "column", `Int column;
  ]


let render_range range : json =
  `Assoc [
    "start", render_pos (Location.get_range_start range);
    "end", render_pos (Location.get_range_end range)
  ]

let render_call (c:Core.Callstack.call) : json =
  `Assoc [
    "function", `String c.call_fun;
    "range", render_range c.call_site;
  ]

let render_callstack cs : json =
  `List (List.map render_call cs)

let render_alarm alarm : json =
  let title =
    let () = Core.Alarm.pp_alarm_title Format.str_formatter alarm in
    Format.flush_str_formatter ()
  in
  let range, cs = alarm.Core.Alarm.alarm_trace in
  `Assoc [
    "title", `String title;
    "range", render_range range;
    "callstack", render_callstack cs;
  ]

let render man alarms time files out : unit =
  let json : json = `Assoc [
      "success", `Bool true;
      "time", `Float time;
      "files", `List (List.map (fun f -> `String f) files);
      "alarms", `List (List.map render_alarm alarms);
    ]
  in
  print json out


let panic ?(btrace="<none>") exn files out =
  let open Exceptions in
  let error =
    match exn with
    | Panic (msg, loc) -> msg
    | PanicAt (range, msg, loc) -> msg
    | SyntaxError(range, msg) -> msg
    | SyntaxErrorList l -> String.concat ", " (List.map snd l)
    |  _ -> Printexc.to_string exn
  in
  let json : json = `Assoc [
      "success", `Bool false;
      "files", `List (List.map (fun f -> `String f) files);
      "exception", `String error;
      "backtrace", `String btrace;
    ]
  in
  print json out

let help (args:(Arg.key * Arg.spec * Arg.doc * string) list) out =
  let json : json = `List (
      args |>
      List.map (fun (key,spec,doc,default) ->
          `Assoc [
            "key", `String key;
            "doc", `String doc;
            "default", `String default;
            "type", `String (
              match spec with
              | Arg.Bool _ -> "bool"
              | Arg.Set _ -> "set"
              | Arg.Clear _ -> "clear"
              | Arg.Unit _ -> "unit"
              | Arg.String _ -> "string"
              | Arg.Set_string _ -> "string"
              | Arg.Int _ -> "int"
              | Arg.Set_int _ -> "int"
              | Arg.Float _ -> "float"
              | Arg.Set_float _ -> "float"
              | Arg.Symbol (l, _) -> "symbol:" ^ (String.concat "," l)
              | _ -> Exceptions.panic ~loc:__LOC__ "unsupported option"
            )
          ]
        )
    )
  in
  print json out

let list_domains (domains:string list) out =
  let json : json = `List (
      domains |>
      List.map (fun d -> `String d)
    )
  in
  print json out
