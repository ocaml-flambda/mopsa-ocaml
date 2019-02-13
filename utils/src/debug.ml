(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Debug - Conditional debugging with channel filtering. *)

let channels = ref []
let print_color = ref true
let print_all = ref false
let colors = [
  ("red", 160);
  ("green", 0x28);
  ("yellow", 0xbe);
  ("blue", 4);
  ("magenta", 0x5c);
  ("fushia", 13);
  ("orange", 0xd0);
  ("Teal", 6);
  ("LightSlateBlue", 105);
  ("DarkOliveGreen3", 113);
  ("IndianRed", 167);
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
let random_color channel = (Hashtbl.hash channel mod 26) * 10 + 2

let color c pp fmt x =
  if !print_color then
    let code = try List.assoc c colors with Not_found -> failwith "Unknwon color" in
    Format.fprintf fmt "\027[1;38;5;%dm%a\027[0m" code pp x
  else
    Format.fprintf fmt "%a" pp x

let color_str c fmt s = color c Format.pp_print_string fmt s

let debug ?(channel = "debug") fmt =
  if can_print channel then
    Format.kasprintf (fun str ->
        if !print_color then
          Format.eprintf "\027[1;38;5;%dm[%s %.3f]\027[0m @[%s@]@." (random_color channel) channel (Sys.time ()) str
        else
          Format.eprintf "[%s %.3f] @[%s@]@." channel (Sys.time ()) str
      ) fmt
  else
    Format.ifprintf Format.std_formatter fmt


let warn fmt = debug ~channel:"warning" fmt

let info fmt = debug ~channel:"info" fmt

let plurial_list fmt l = if List.length l <= 1 then () else Format.pp_print_string fmt "s"
let plurial_int fmt n = if n <= 1 then () else Format.pp_print_string fmt "s"
