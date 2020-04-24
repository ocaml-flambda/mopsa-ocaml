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

(** Debug - Conditional debugging with channel filtering. *)

let channels = ref []
let print_warnings = ref true
let print_color = ref true
let print_all = ref false
let colors = [
  ("white", 1);
  ("red", 9);
  ("green", 0x28);
  ("yellow", 0xbe);
  ("blue", 4);
  ("magenta", 5);
  ("fushia", 13);
  ("orange", 0xd0);
  ("teal", 6);
]

let add_channel ch =
  if ch = "all" then
    print_all := true
  else
    let ch' = ch ^ "$" |>
              Str.global_replace (Str.regexp_string ".") "\\."  |>
              Str.global_replace (Str.regexp_string "_") ".*"
    in
    let re = Str.regexp ch' in
    channels := re :: !channels


(* Parse a list of channels separated by ',' *)
let parse opt =
  Str.split (Str.regexp ",") opt |>
  List.iter add_channel

let set_channels opt =
  let () = channels := [] in
  parse opt

let can_print channel =
  !print_all ||
  !channels |> List.exists (fun re -> Str.string_match re channel 0)

(** Gives a random map of channel colors *)
let random_color channel =
  (Hashtbl.hash channel mod 26) * 9 + 2

let color c pp fmt x =
  if !print_color then
    let code = try List.assoc c colors with Not_found -> failwith "Unknwon color" in
    Format.fprintf fmt "\027[1;38;5;%dm%a\027[0m" code pp x
  else
    Format.fprintf fmt "%a" pp x

let color_str c fmt s = color c Format.pp_print_string fmt s

let bold pp fmt x =
  if !print_color then
    Format.fprintf fmt "\027[1m%a\027[0m" pp x
  else
    Format.fprintf fmt "%a" pp x


let debug ?(channel = "debug") fmt =
  if can_print channel then
    Format.kasprintf (fun str ->
        if !print_color then
          Format.printf "\027[1;38;5;%dm[%s %.3f]\027[0m @[%s@]@." (random_color channel) channel (Sys.time ()) str
        else
          Format.printf "[%s %.3f] @[%s@]@." channel (Sys.time ()) str
      ) fmt
  else
    Format.ifprintf Format.std_formatter fmt


let info fmt =
    if can_print "info" then
      Format.kasprintf (fun str ->
          Format.printf "[%a] %s@." (color_str "orange") "*" str
      ) fmt
  else
    Format.ifprintf Format.std_formatter fmt

let plurial_list fmt l = if List.length l <= 1 then () else Format.pp_print_string fmt "s"
let plurial_int fmt n = if n <= 1 then () else Format.pp_print_string fmt "s"

let panic fmt =
  Format.kasprintf (fun str ->
        Format.printf "%a: %s@." (color_str "red") "panic" str
      ) fmt

let panic_at range fmt =
  Format.kasprintf (fun str ->
      Format.printf "%a: panic: %s@." (color "red" Location.pp_range) range str
    ) fmt


let warn fmt =
  if !print_warnings then
    Format.kasprintf (fun str ->
        Format.printf "%a: %s@." (color_str "orange") "warning" str
      ) fmt
  else
    Format.ifprintf Format.std_formatter fmt


let warn_at range fmt =
    if !print_warnings then
      Format.kasprintf (fun str ->
          Format.printf "%a: warning: %s@." (color "orange" Location.pp_range) range str
        ) fmt
    else
      Format.ifprintf Format.std_formatter fmt



(* simple detection of terminal coloring capabilities, using TERM variable;
   able to detect whether we are runnig under emacs and force no-color
 *)
let terminal_has_colors () =
  try match Sys.getenv "TERM" with
      | "dumb" | "" -> false
      | _ -> true (* by default *)
  with Not_found -> false

let () = print_color := terminal_has_colors ()
