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

open ArgExt
open Core.All
open Sig.Domain.Lowlevel
open Location
open Format
open Callstack


(** Command-line option to enable display of alarms call stacks *)
let opt_show_callstacks = ref false


let print out fmt =
  let formatter =
    match out with
    | None -> std_formatter
    | Some file ->
      let o = open_out file in
      formatter_of_out_channel o
  in
  kasprintf (fun str ->
      fprintf formatter "%s%!" str
    ) fmt


module AlarmMessageSet = SetExt.Make(struct type t = alarm_message let compare = compare_alarm_message end)
module CallstackSet = SetExt.Make(struct type t = callstack let compare = compare_callstack end)


(** Highlight source code at a given location range *)
let highlight_range fmt range =
  if not @@ is_orig_range @@ untag_range range then ()
  else
    (* Print source code at location range *)
    let start_pos = get_range_start range in
    let end_pos = get_range_end range in
    let file = get_pos_file start_pos in
    assert (file = get_pos_file end_pos);

    let is_bug_line i = i >= get_pos_line start_pos && i <= get_pos_line end_pos in

    (* Read the file from disk *)
    let f = open_in file in
    let rec get_bug_lines i =
      try
        let l = input_line f in
        if is_bug_line i then (i,l) :: get_bug_lines (i+1)
        else get_bug_lines (i+1)
      with End_of_file -> []
    in

    (* Highlight bug region in a line *)
    let highlight_bug fmt (i,l) =
      let safe_sub l s e =
        try String.sub l s e
        with Invalid_argument _ ->
          let () = Debug.warn_at range "issue on sub %s %d %d" l s e in
          String.sub l (min 0 s) (max 0 (min e ((String.length l) - s))) in
      let n = String.length l in
      (* prints from c1 to c2 included *)
      let c1 = get_pos_column start_pos in
      let c2 = get_pos_column end_pos in
      let s1,s2,s3 =
        if i = get_pos_line start_pos && i = get_pos_line end_pos then
          safe_sub l 0 c1,
          safe_sub l c1 (c2-c1),
          safe_sub l c2 (n-c2)
        else if i = get_pos_line start_pos && i = get_pos_line end_pos then
          safe_sub l 0 c1,
          safe_sub l c1 (n-c1),
          ""
        else if i = get_pos_line end_pos then
          "",
          safe_sub l 0 c2,
          safe_sub l c2 (n-c2)
        else
          "",
          l,
          ""
      in
      fprintf fmt "%a: %s%a%s" (Debug.bold pp_print_int) i s1 (Debug.color_str "red") s2 s3
    in

    (* Underline bug region *)
    let underline_bug fmt (i,l) =
      let n = string_of_int i |> String.length in
      let c1 = get_pos_column start_pos + n + 2 in
      let c2 = get_pos_column end_pos + n + 2 in
      let c3 = String.length l + n + 2 in
      let s1 = String.make c1 ' ' in
      let s2 = String.make (c2 - c1) '^' in
      let s3 = String.make (c3 - c2) ' ' in
      fprintf fmt "@,%s%a%s" s1 (Debug.color_str "red") s2 s3
    in

    (* Print the highlighted lines *)
    let lines = get_bug_lines 1 in
    close_in f;
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") highlight_bug fmt lines;

    (* Underline bug if the number of lines is 1 *)
    match lines with
    | [bug] -> underline_bug fmt bug
    | _ -> ()


let report ?(flow=None) man alarms time files out =
  if Soundness.is_sound ()
  then print out "%a@." (Debug.color_str "green") "Analysis terminated successfully"
  else print out "%a@." (Debug.color_str "orange") "Unsound analysis";

  let () = match flow with
    | None -> ()
    | Some f ->
      print out "Last flow =@[@\n%a@]@\n"
        (* "Context = @[@\n%a@]@\n" *)
        (Core.Flow.print man.lattice.print) f
        (* (Core.Context.print man.lattice.print) (Flow.get_ctx f) *)
  in

  let () =
    if AlarmSet.is_empty alarms
    then print out "%a No alarm@." ((Debug.color "green") pp_print_string) "✔"
    else
      (* Iterate first on the alarm classes *)
      let cls_map = index_alarm_set_by_class alarms in
      let sub_totals, total = ClassMap.fold (fun cls ss (sub_totals, total) ->

          (* Then iterate on the location ranges within each class *)
          let range_map = index_alarm_set_by_range ss in
          let sub_total = RangeMap.fold (fun range sss sub_total ->

              (* Group similar messages and callstacks *)
              let messages, callstacks = AlarmSet.fold (fun alarm (messages,callstacks) ->
                  AlarmMessageSet.add (get_alarm_message alarm) messages,
                  CallstackSet.add (get_alarm_callstack alarm) callstacks
                ) sss (AlarmMessageSet.empty, CallstackSet.empty)
              in

              (* Print the alarm instance *)
              let file_name = get_range_relative_file range in
              let fun_name = match CallstackSet.elements callstacks with
                | (c::_) :: _ -> c.call_fun_orig_name
                | _ -> "<>"
              in
              print out "@.%a: In function '%a':@.@[<v 2>%a: %a@,@,%a@,%a%a@]@.@."
                (Debug.bold pp_print_string) file_name
                (Debug.bold pp_print_string) fun_name
                (Debug.bold pp_relative_range) range
                (Debug.color "magenta" pp_alarm_class) cls
                highlight_range range
                (pp_grouped_alarm_message cls) (AlarmMessageSet.elements messages)
                (fun fmt callstacks ->
                   if not !opt_show_callstacks then ()
                   else
                     let pp_numbered_callstack i fmt cs =
                       fprintf fmt "@[<v>Call stack%a:@,%a@]"
                         (fun fmt -> function
                            | None   -> ()
                            | Some i -> fprintf fmt " %d" (i+1)
                         ) i
                         pp_callstack cs
                     in
                     match CallstackSet.elements callstacks with
                     | [] -> ()
                     | [cs] -> pp_numbered_callstack None fmt cs
                     | csl    ->
                       let csl' = List.mapi (fun i cs -> (i,cs)) csl in
                       pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
                         (fun fmt (i,cs) -> pp_numbered_callstack (Some i) fmt cs)
                         fmt csl';
                ) callstacks
              ;
              sub_total + 1
            ) range_map 0
          in
          (cls,sub_total) :: sub_totals, sub_total + total
        ) cls_map ([],0)
      in
      (* Print alarms summary *)
      print out "@[<v 2>Summary of detected alarms:@,%a@,Total: %d@]@.@."
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           (fun fmt (cls,nb) -> fprintf fmt "%a: %d" pp_alarm_class cls nb)
        ) sub_totals
        total
  in
  let () =
    match Soundness.get_warnings () with
    | [] -> ()
    | warnings ->
      print out "%d warning%a detected:@,  @[<v>%a@]@.@." (List.length warnings) Debug.plurial_list warnings
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           Core.Soundness.pp_warning
        ) warnings
  in
  print out "Time: %.3fs@." time;
  ()

let panic ?btrace exn files time out =
  print out "%a@." (Debug.color_str "red") "Analysis aborted";
  let () =
    match exn with
    | Exceptions.Panic (msg, "") -> print out "panic: %s@." msg
    | Exceptions.Panic (msg, loc) -> print out "panic raised in %s: %s@." loc msg

    | Exceptions.PanicAt (range, msg, "") -> print out "panic in %a: %s@." Location.pp_range range msg
    | Exceptions.PanicAt (range, msg, loc) -> print out "%a: panic raised in %s: %s@." Location.pp_range range loc msg

    | Exceptions.SyntaxError (range, msg) -> print out "%a: syntax error: %s@." Location.pp_range range msg
    | Exceptions.UnnamedSyntaxError range -> print out "%a: syntax error@." Location.pp_range range

    | Exceptions.SyntaxErrorList l ->
      print out "Syntax errors:@\n  @[%a@]@."
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
           (fun fmt (range, msg) -> fprintf fmt "%a: %s" Location.pp_range range msg
           )
        ) l

    | Exceptions.UnnamedSyntaxErrorList l ->
      print out "Syntax errors:@\n  @[%a@]@."
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") Location.pp_range)
        l

    | _ -> print out "Uncaught exception: %s@." (Printexc.to_string exn)
  in
  let () =
    match btrace with
    | Some x when String.length x > 0 -> print out "Backtrace:@\n%s" x
    | _ -> ()
  in
  ()

let group_args_by_category args =
  let sorted = List.sort (fun arg1 arg2 ->
      compare arg1.category arg2.category
    ) args
  in
  let grouped, _ = List.fold_right (fun arg (acc,cat) ->
      if compare cat arg.category <> 0
      then
        (arg.category,[arg]) :: acc, arg.category
      else
        let (_, l) = List.hd acc in
        (cat, arg :: l) :: (List.tl acc), cat
    ) sorted ([],"")
  in
  grouped

let help (args:ArgExt.arg list) out =
  let print_default fmt d =
    if d = "" then ()
    else fprintf fmt " (default: %s)" d
  in
  let groups = group_args_by_category args in
  print out "Options:@.";
  List.iter (fun (cat, args) ->
      print out "  %s@." (String.uppercase_ascii cat);
      List.iter (fun arg ->
          match arg.spec with
          | ArgExt.Symbol(l,_) ->
            print out "    %s={%a} %s%a@."
              arg.key
              (pp_print_list
                 ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
                 pp_print_string
              ) l
              arg.doc
              print_default arg.default
          | _ ->
            print out "    %s %s%a@." arg.key arg.doc print_default arg.default
        ) (List.sort Stdlib.compare args)
    ) groups

let list_domains (domains:string list) out =
  print out "Domains:@.";
  List.iter (fun d -> print out "  %s@." d) domains

let list_alarms alarms out =
  print out "Alarm classes:@.";
  List.iter (fun a -> print out "  %a@." Core.Alarm.pp_alarm_class a) alarms

let print range printer flow out =
  if Debug.can_print "print" then
    print out "%a@\n  @[%a@]@."
      Location.pp_range range
      printer flow
  else
    ()
