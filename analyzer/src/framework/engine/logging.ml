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

(** Logs of analysis events *)

open Location
open Ast.Expr
open Ast.Stmt
open Core
open Core.Zone
open Core.Manager
open Format

(** Command-line option to activate logs *)
let opt_log = ref false

(** Command-line option for printing logs without abstract states *)
let opt_short_log = ref false

(** Current log level *)
let cur_level = ref 0

let color level s =
  let code = (level mod 16) * 16 + 10 in
  if !Debug.print_color then
    Printf.sprintf "\027[1;38;5;%dm%s\027[0m" code s
  else
    s

type symbol =
  | BEGIN
  | END
  | MSG

(** Symbol of a new entry *)
let symbol_to_string symbol level =
  match symbol with
  | BEGIN -> color level "+"
  | END -> color level "o"
  | MSG -> color level "*"

let is_end_symbol = function
  | END -> true
  | _ -> false

(** Tabulation *)
let tab level = color level "|"

(** Indent a message by adding tabs at the beginning of each line *)
let indent ~symbol fmt =
  if !opt_log || !opt_short_log then
    (* Get the formatted message as a string *)
    Format.kasprintf (fun str ->
        (* Split the message into lines *)
        let lines = String.split_on_char '\n' str in

        match lines with
        | [] -> ()
        | first :: others ->

          (* The first line is prefixed with the entry symbol *)
          let first' = (symbol_to_string symbol !cur_level) ^ " " ^ first in

          (* The other lines are prefixed with the indent symbol *)
          let others' =
            if not (is_end_symbol symbol) then
              List.map (fun line -> (tab !cur_level) ^ " " ^ line) others
            else
              List.map (fun line -> "  " ^ line) others
          in

          (* Add the margin *)
          let margin = List.init !cur_level (fun i -> (tab i) ^ " ") |>
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
  indent "%s" name ~symbol:MSG

let parse file =
  indent "parsing %s" file ~symbol:MSG

let reach loc =
  indent "reaching %a" pp_range loc ~symbol:MSG

let pp_S fmt stmt =
  fprintf fmt "@[<v 3>S [| %a@] |]" pp_stmt stmt

let pp_E fmt exp =
  fprintf fmt "@[<v 3>E [| %a@] |]" pp_expr exp

let exec stmt zone man flow =
  if !opt_short_log then
    indent "%a in zone %a"
      pp_S stmt
      pp_zone zone
      ~symbol:BEGIN
  else
    indent "%a @,in %a @,and zone %a"
      pp_S stmt
      (Flow.print man.lattice) flow
      pp_zone zone
      ~symbol:BEGIN
  ;
  incr cur_level

let exec_done stmt zone time man flow =
  decr cur_level;
  if !opt_short_log then
    indent "%a done in zone %a [%.4fs]"
      pp_S stmt
      pp_zone zone
      time
      ~symbol:END
  else
    indent "%a done in zone %a [%.4fs]@ -->  %a"
      pp_S stmt
      pp_zone zone
      time
      (Flow.print man.lattice) flow
      ~symbol:END

let eval exp zone man flow =
  if !opt_short_log then
    indent "%a in zone %a"
      pp_E exp
      pp_zone2 zone
      ~symbol:BEGIN
  else
    indent "%a @,in %a @,and zone %a"
      pp_E exp
      (Flow.print man.lattice) flow
      pp_zone2 zone
      ~symbol:BEGIN
  ;
  incr cur_level


let eval_done exp zone time evl =
  decr cur_level;
  if !opt_short_log then
    indent "%a done in zone %a [%.4fs]"
      pp_E exp
      pp_zone2 zone
      time
      ~symbol:END
  else
    indent "%a done in zone %a [%.4fs]@ -->  %a"
      pp_E exp
      pp_zone2 zone
      time
      (Eval.print ~pp:pp_expr) evl
      ~symbol:END

let debug fmt =
  indent ~symbol:MSG fmt
