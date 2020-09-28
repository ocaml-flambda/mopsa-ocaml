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

open Mopsa
open Format
open Core.All


(** Logs options *)
module type OPTIONS =
sig
  val name  : string
  val short : bool
end

module Hook(Options:OPTIONS) =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = Options.name


  (** {2 Initialization} *)
  (** ****************** *)

  (* We use a stack for keeping the duration of exec and eval *)
  let stack = Stack.create ()

  let init ctx = Stack.clear stack


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
        let cur_level = max (Stack.length stack) 0 in

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
    fprintf fmt "@[<v 3>E [| %a : %a@] |]" pp_expr exp pp_typ exp.etyp

  let get_timing () =
    try Sys.time () -. Stack.pop stack
    with Stack.Empty -> Float.nan


  (** {2 Events handlers} *)
  (** ******************* *)

  let on_before_exec route stmt man flow =
    reach stmt.srange;
    if Options.short then
      indent "%a in semantic %a"
        pp_S stmt
        pp_route route
        ~symbol:BEGIN
    else
      indent "%a @,in %a @,and semantic %a"
        pp_S stmt
        (format (Flow.print man.lattice.print)) flow
        pp_route route
        ~symbol:BEGIN
    ;
    Stack.push (Sys.time ()) stack


  let on_after_exec route stmt man flow post =
    let time = get_timing () in
    let nb = Cases.cardinal post in
    if Options.short then
      indent "%a done in semantic %a [%.4fs, %d case%a]"
        pp_S stmt
        pp_route route
        time
        nb Debug.plurial_int nb
        ~symbol:END
    else
      indent "%a done in semantic %a [%.4fs, %d case%a]@ -->  %a"
        pp_S stmt
        pp_route route
        time
        nb Debug.plurial_int nb
        (Cases.print
           (fun fmt _ flow ->
              format (Flow.print man.lattice.print) fmt flow
           )
        ) post
        ~symbol:END


  let on_before_eval route exp man flow =
    if Options.short then
      indent "%a in semantic %a"
        pp_E exp
        pp_route route
        ~symbol:BEGIN
    else
      indent "%a @,in %a @,and semantic %a"
        pp_E exp
        (format (Flow.print man.lattice.print)) flow
        pp_route route
        ~symbol:BEGIN
    ;
      Stack.push (Sys.time ()) stack

  let on_after_eval route exp man flow evl =
    let time = get_timing () in
    let pp_evl_with_type fmt evl =
      Cases.print_result (
        fun fmt e flow ->
          Format.fprintf fmt "%a : %a" pp_expr e pp_typ e.etyp
      ) fmt evl
    in
    let nb = Cases.cardinal evl in
    if Options.short then
      indent "%a = %a done in semantic %a [%.4fs, %d case%a]"
        pp_E exp
        pp_evl_with_type evl
        pp_route route
        time
        nb Debug.plurial_int nb
        ~symbol:END
    else
      indent "%a done in semantic %a [%.4fs, %d case%a]@ -->  %a"
        pp_E exp
        pp_route route
        time
        nb Debug.plurial_int nb
        pp_evl_with_type evl
        ~symbol:END

  let on_finish man flow =
    Stack.clear stack

end

let () =
  Core.Hook.register_stateless_hook (module Hook(struct let name = "logs" let short = false end));
  Core.Hook.register_stateless_hook (module Hook(struct let name = "short-logs" let short = true end))
