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
            "callstack", `List (List.map (fun range ->
                `String (range_to_string range)
              ) cs)
          ]
        ) alarms);
    ]
  in
  print json out


let panic ?(btrace="<none>") exn files out =
  let json : json = `Assoc [
      "success", `Bool false;
      "files", `List (List.map (fun f -> `String f) files);
      "exception", `String (Printexc.to_string exn);
      "backtrace", `String btrace;
    ]
  in
  print json out
