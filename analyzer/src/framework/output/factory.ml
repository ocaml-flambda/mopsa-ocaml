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

(** Render the output of an analysis depending on the selected engine. *)

type format =
  | F_text (* Textual output *)
  | F_json (* Formatted output in JSON *)


(* Command line option *)
(* ------------------- *)

let opt_format = ref F_text
let opt_file = ref None
let opt_display_lastflow = ref false

(* Result rendering *)
(* ---------------- *)

(* Print collected alarms in the desired output format *)
let report man flow time files =
  let alarms = Core.Flow.fold (fun acc tk env ->
      match tk with
      | Core.Alarm.T_alarm a -> a :: acc
      | _ -> acc
    ) [] flow
  in
  let return_v = if List.length alarms > 0 then 1 else 0 in
  let lf = if !opt_display_lastflow then Some flow else None in
  let _ = match !opt_format with
    | F_text -> Text.report ~flow:lf man alarms time files !opt_file
    | F_json -> Json.report man alarms time files !opt_file in
  return_v

let panic ?btrace exn files =
  match !opt_format with
  | F_text -> Text.panic ?btrace exn files !opt_file
  | F_json -> Json.panic ?btrace exn files !opt_file

let help (args:ArgExt.arg list) =
  match !opt_format with
  | F_text -> Text.help args !opt_file
  | F_json -> Json.help args !opt_file

let list_domains (domains:string list) =
  match !opt_format with
  | F_text -> Text.list_domains domains !opt_file
  | F_json -> Json.list_domains domains !opt_file
