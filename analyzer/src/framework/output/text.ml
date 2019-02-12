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
  print "%a@." (Debug.color_str "green") "Analysis terminated successfully";
  print "Time: %.3fs@." time;
  match alarms with
  | [] -> print "%a No alarm@." ((Debug.color "green") pp_print_string) "✔"
  | _ ->
    print "%d alarm%a detected:@." (List.length alarms) Debug.plurial_list alarms;
    print "@[%a@]@."
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt "@\n@\n")
         Alarm.pp_alarm
      ) alarms


let panic ?btrace exn files out =
  let print fmt = get_printer out fmt in
  print "%a@." (Debug.color_str "red") "Analysis aborted";
  let () =
    match exn with
    | Exceptions.Panic (msg, "") -> print "panic: %s@." msg
    | Exceptions.Panic (msg, loc) -> print "panic raised in %s: %s@." loc msg

    | Exceptions.PanicAt (range, msg, "") -> print "panic in %a: %s@." Location.pp_range range msg
    | Exceptions.PanicAt (range, msg, loc) -> print "%a: panic raised in %s: %s@." Location.pp_range range loc msg

    | Exceptions.SyntaxError (range, msg) -> print "syntax error in %a: %s@." Location.pp_range range msg
    | Exceptions.UnnamedSyntaxError range -> print "syntax error in %a@." Location.pp_range range

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
    | Some x when String.length x > 0 -> print "Backtrace:@\n%s" x
    | _ -> ()
  in
  ()


let help (args:(Arg.key * Arg.spec * Arg.doc * string) list) out =
  let print fmt = get_printer out fmt in
  let print_default fmt d =
    if d = "" then ()
    else fprintf fmt " (default: %s)" d
  in
  print "Options:@.";
  List.iter (fun (key,spec,doc,default) ->
      match spec with
      | Arg.Symbol(l,_) ->
        print "  %s={%a} %s%a@."
          key
          (pp_print_list
             ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
             pp_print_string
          ) l
          doc
          print_default default
      | _ -> print "  %s %s%a@." key doc print_default default
    ) args

let list_domains (domains:string list) out =
  let print fmt = get_printer out fmt in
  print "Domains:@.";
  List.iter (fun d -> print "  %s@." d) domains
