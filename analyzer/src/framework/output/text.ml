(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Display the results of the analysis in a textual form. *)

open Format

let get_printer out =
  let formatter =
    match out with
    | None -> std_formatter
    | Some file ->
      let o = open_out file in
      formatter_of_out_channel o
  in
  let print fmt =
    kasprintf (fun str ->
        fprintf formatter "%s" str
      ) fmt
  in
  print

let render man alarms time files out =
  let print fmt = get_printer out fmt in
  print "Analysis terminated successfully@.";
  print "Time: %.3fs@." time;
  match alarms with
  | [] -> print "%a No alarm@." ((Debug.color "green") pp_print_string) "✔"; 0
  | _ ->
    print "%d alarm%a detected:@." (List.length alarms) Debug.plurial_list alarms;
    print "  @[%a@]@."
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt "@\n@\n")
         Alarm.pp_alarm
      ) alarms; 1



let panic ?btrace exn files out =
  let print fmt = get_printer out fmt in
  print "Analysis aborted@.";
  print "File%a: @[%a@]@."
    Debug.plurial_list files
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") pp_print_string) files
  ;
    print "Uncaught exception: %s@." (Printexc.to_string exn);
  (match btrace with Some x -> print "backtrace:@.%s" x | None -> ())
