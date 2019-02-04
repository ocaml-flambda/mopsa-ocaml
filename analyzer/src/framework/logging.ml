(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Log of analysis event *)

open Ast
open Location
open Zone
open Format

(** Command-line option to activate logs *)
let opt_log = ref false

(** Command-line option for printing logs without abstract states *)
let opt_short_log = ref false

(** Number of tabs of the current level *)
let tabs = ref 0

let color level s =
  let code = (level mod 16) * 16 + 10 in
  if !Debug.print_color then
    Printf.sprintf "\027[1;38;5;%dm%s\027[0m" code s
  else
    s


(** Symbol of a new entry *)
let entry_symbol level = color level "+"

(** Symbol of a closing an entry *)
let close_symbol level = color level "●"

(** Symbol of an indentation *)
let indent_symbol level = color level "|"  

(** Indent a message by adding tabs at the beginning of each line *)
let indent ?(close=false) fmt =
  if !opt_log || !opt_short_log then
    (* Get the formatted message as a string *)
    Format.kasprintf (fun str ->
        (* Split the message into lines *)
        let lines = String.split_on_char '\n' str in

        match lines with
        | [] -> ()
        | first :: others ->

          (* The first line is prefixed with the entry symbol *)
          let first' =
            if not close then (entry_symbol !tabs) ^ " " ^ first
            else (close_symbol !tabs) ^ " " ^ first
          in

          (* The other lines are prefixed with the indent symbol *)
          let others' =
            if not close then
              List.map (fun line -> (indent_symbol !tabs) ^ " " ^ line) others
            else
              List.map (fun line -> "  " ^ line) others
          in

          (* Add the margin *)
          let margin = List.init !tabs (fun i -> (indent_symbol i) ^ " ") |>
                       String.concat ""
          in
          let lines' = List.map (fun line ->
              margin ^ line
            ) (first' :: others')
          in

          printf "%a@."
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") pp_print_string)
            lines'
      ) fmt
  else
    ifprintf std_formatter fmt


let phase name =
  indent "%s" name

let parse file =
  indent "parsing %s" file

let reach loc =
  indent "reaching %a" pp_range loc

let exec stmt zone man flow =
  if !opt_short_log then
    indent "@[<v 3>𝕊⟦ %a@] ⟧ in zone %a"
      pp_stmt stmt
      pp_zone zone
      ~close:false
  else
    indent "@[<v 3>𝕊⟦ %a@] ⟧@ in %a@, and zone %a"
      pp_stmt stmt
      (Flow.print man) flow
      pp_zone zone
      ~close:false
  ;
  incr tabs

let exec_done stmt zone time man flow =
  decr tabs;
  if !opt_short_log then
    indent "@[<v 2>𝕊⟦ %a@] ⟧ done in zone %a"
      pp_stmt stmt
      pp_zone zone
      ~close:true
  else
    indent "@[<v 2>𝕊⟦ %a@] ⟧@ = %a@ in zone %a [%.4fs]"
      pp_stmt stmt
      (Flow.print man) flow
      pp_zone zone
      time
      ~close:true 

let eval exp zone man flow =
  if !opt_short_log then
    indent "@[<v 2>𝔼⟦ %a@] ⟧ in zone %a"
      pp_expr exp
      pp_zone2 zone
      ~close:false
  else
    indent "@[<v 2>𝔼⟦ %a@] ⟧@ in zone %a and pre-state:@,%a"
      pp_expr exp
      pp_zone2 zone
      (Flow.print man) flow
      ~close:false
  ;
  incr tabs

let eval_done exp zone time evl =
  decr tabs;
  indent "@[<v 2>𝔼⟦ %a@] ⟧ = %a in zone %a [%.4fs]"
    pp_expr exp
    (Eval.print ~pp:pp_expr) evl
    pp_zone2 zone
    time
    ~close:true
