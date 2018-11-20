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
  let () =
    match exn with
    | Exceptions.Panic msg -> print "Panic: %s@." msg
    | Exceptions.PanicAt (range, msg) -> print "Panic in %a: %s@." Location.pp_range range msg
    | Exceptions.SyntaxError (range, msg) -> print "Syntax error in %a: %s@." Location.pp_range range msg
    | Exceptions.UnnamedSyntaxError range -> print "Syntax error in %a@." Location.pp_range range
    | Exceptions.SyntaxErrorList l ->
      print "Syntax errors:@\n  @[%a@]@."
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
           (fun fmt (range, msg) -> fprintf fmt "%a: %s" Location.pp_range range msg
           )
        ) l
    | Exceptions.UnnamedSyntaxErrorList l ->
      print "Syntax errors:@\n  @[%a@]@."
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") Location.pp_range)
        l
    | _ -> print "Uncaught exception: %s@." (Printexc.to_string exn)
  in
  let () =
    match btrace with
    | Some x -> print "backtrace:@.%s" x
    | None -> ()
  in
  ()
