(**
   Debug - Conditional debugging with channel filtering.
*)

let channels = ref []
let print_color = ref true
let print_all = ref false
let colors = [("red", 160); ("green", 0x28); ("yellow", 0xbe); ("blue", 4); ("magenta", 0x5c); ("fushia", 13); ("orange", 0xd0)]
  
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

let can_print channel =
  !print_all ||
  !channels |> List.exists (fun re -> Str.string_match re channel 0)

(** Gives a random map of channel colors *)
let random_color channel = (Hashtbl.hash channel mod 26) * 10 + 1

let color c fx fmt x =
  if !print_color then
    let code = try List.assoc c colors with Not_found -> failwith "Unknwon color" in
    Format.fprintf fmt "\027[1;38;5;%dm%a\027[0m" code fx x
  else
    Format.fprintf fmt "%a" fx x

let print_ channel fmt =
  if can_print channel then
    Format.kasprintf (fun str ->
        if !print_color then
          Format.printf "\027[1;38;5;%dm[%s %.6f]\027[0m @.@[%s@]@.@." (random_color channel) channel (Sys.time ()) str
        else
          Format.printf "[%s %.6f] @.@[%s@]@.@." channel (Sys.time ()) str
      ) fmt
  else
    Format.ifprintf Format.std_formatter fmt

let debug ?(channel = "debug") fmt = print_ channel fmt

let warn fmt =
  if can_print "warning" then
    Format.kasprintf (fun str ->
        if !print_color then
          Format.eprintf "\027[1;45m[WARN %.6f]\027[0m @.@[%s@]@.@." (Sys.time ()) str
      else
        Format.eprintf "[WARN %.6f] @.@[%s@]@.@." (Sys.time ()) str
      ) fmt
  else
    Format.ifprintf Format.std_formatter fmt


let eprint ~channel fmt =
  if can_print channel then
    Format.kasprintf (fun str ->
        Format.eprintf "%s%!" str
      ) fmt
  else
    Format.ifprintf Format.std_formatter fmt


let info fmt = print_ "info" fmt

let fail fmt =
  Format.kasprintf (fun str ->
      if !print_color then
        Format.eprintf "\027[1;41m[FAIL %.6f]\027[0m @.@[%s@]@.@." (Sys.time ()) str
      else
        Format.eprintf "[FAIL %.6f] @.@[%s@]@.@." (Sys.time ()) str;
      exit (-1)
    ) fmt
