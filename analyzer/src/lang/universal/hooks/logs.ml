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

(** Hook for displaying analysis logs as a tree *)

open Location
open Ast.Expr
open Ast.Stmt
open Format
open Core.All
open Sig.Domain.Manager


(** Command-line option for printing logs without abstract states *)
let opt_short_logs = ref false

let () =
  Config.Options.register_builtin_option {
    key = "-short-logs";
    category = "Debugging";
    doc = " show analysis logs without abstract states";
    spec = ArgExt.Set opt_short_logs;
    default = "false";
  }


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "logs"
  let exec_zones = [Z_any]
  let eval_zones = [Z_any,Z_any]


  (** {2 Initialization} *)
  (** ****************** *)

  (* We use a stack for keeping the duration of exec and eval *)
  let stack = Stack.create ()

  let init ctx = ()


  (** {2 Indentation} *)
  (** *************** *)

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
    (* Get the formatted message as a string *)
    Format.kasprintf (fun str ->
        (* Split the message into lines *)
        let lines = String.split_on_char '\n' str in
        let cur_level = Stack.length stack in

        match lines with
        | [] -> ()
        | first :: others ->

          (* The first line is prefixed with the entry symbol *)
          let first' = (symbol_to_string symbol cur_level) ^ " " ^ first in

          (* The other lines are prefixed with the indent symbol *)
          let others' =
            if not (is_end_symbol symbol) then
              List.map (fun line -> (tab cur_level) ^ " " ^ line) others
            else
              List.map (fun line -> "  " ^ line) others
          in

          (* Add the margin *)
          let margin = List.init cur_level (fun i -> (tab i) ^ " ") |>
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


  let reach loc =
    indent "reaching %a" pp_range loc ~symbol:MSG

  let pp_S fmt stmt =
    fprintf fmt "@[<v 3>S [| %a@] |]" pp_stmt stmt

  let pp_E fmt exp =
    fprintf fmt "@[<v 3>E [| %a@] |]" pp_expr exp


  (** {2 Events handlers} *)
  (** ******************* *)

  let on_before_exec zone stmt man flow =
    reach stmt.srange;
    if !opt_short_logs then
      indent "%a in zone %a"
        pp_S stmt
        pp_zone zone
        ~symbol:BEGIN
    else
      indent "%a @,in %a @,and zone %a"
        pp_S stmt
        (Flow.print man.lattice.print) flow
        pp_zone zone
        ~symbol:BEGIN
    ;
    Stack.push (Sys.time ()) stack


  let on_after_exec zone stmt man post =
    let time = Sys.time () -. Stack.pop stack in
    if !opt_short_logs then
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
        (Post.print man.lattice.print) post
        ~symbol:END


  let on_before_eval zone exp man flow =
    if !opt_short_logs then
      indent "%a in zone %a"
        pp_E exp
        pp_zone2 zone
        ~symbol:BEGIN
    else
      indent "%a @,in %a @,and zone %a"
        pp_E exp
        (Flow.print man.lattice.print) flow
        pp_zone2 zone
        ~symbol:BEGIN
    ;
    Stack.push (Sys.time ()) stack


  let on_after_eval zone exp man evl =
    let time = Sys.time () -. Stack.pop stack in
    if !opt_short_logs then
      indent "%a = %a done in zone %a [%.4fs]"
        pp_E exp
        Eval.print evl
        pp_zone2 zone
        time
        ~symbol:END
    else
      indent "%a done in zone %a [%.4fs]@ -->  %a"
        pp_E exp
        pp_zone2 zone
        time
        Eval.print evl
        ~symbol:END

  let on_finish man flow = ()

end

let () =
  Core.Hook.register_stateless_hook (module Hook)
