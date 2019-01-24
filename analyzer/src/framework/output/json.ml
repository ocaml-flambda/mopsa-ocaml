(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
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

let range_to_string range =
  let file = Location.get_range_file range in
  let line = Location.get_range_line range in
  file ^ ":" ^ (string_of_int line)

let render man alarms time files out =
  let json : json = `Assoc [
      "success", `Bool true;
      "time", `Float time;
      "files", `List (List.map (fun f -> `String f) files);
      "alarms", `List (List.map (fun alarm ->
          let title =
            let () = Alarm.pp_alarm_title Format.str_formatter alarm in
            Format.flush_str_formatter ()
          in
          let range, cs = alarm.Alarm.alarm_trace in
          `Assoc [
            "title", `String title;
            "range", `String (range_to_string range);
          ]
        ) alarms);
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
