(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2023 The MOPSA Project.                               *)
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

open Core.All
open Mopsa_utils
open Action
open Format
open Location

type trace_element_kind = BEGIN | END

type trace_element = {
  id: int;
  action: action;
  kind: trace_element_kind;
  time: float;
}

type trace = {
  elements: trace_element list;
  last_id: int;
}

let empty_trace = {
  elements = [];
  last_id = -1;
}

let begin_trace_element action trace =
  let id = trace.last_id + 1 in
  { elements = {id; kind = BEGIN; action; time = Sys.time ()} :: trace.elements;
    last_id = id }

let end_trace_element id action trace =
  { elements = {id; kind = END; action; time = Sys.time ()} :: trace.elements;
    last_id = trace.last_id }

let get_last_trace_element_id trace = trace.last_id

let colored level s =
  let code = (level mod 16) * 16 + 10 in
  if !Debug.print_color then
    Printf.sprintf "\027[1;38;5;%dm%s\027[0m" code s
  else
    s

let pp_trace fmt trace =
  let rec iter indent fmt = function
    | [] -> ()
    | hd::tl ->
      let margin = List.init indent (fun i -> colored i "│ ") |>
                   String.concat ""
      in
      fprintf fmt "%s%a %a [#%d, %.4f, %a]"
        margin
        (fun fmt -> function
           | BEGIN -> pp_print_string fmt (colored indent "┌")
           | END   -> pp_print_string fmt (colored indent "└")
        ) hd.kind
        (pp_action ~truncate:true ~indent:0) hd.action
        hd.id
        hd.time
        pp_relative_range (action_range hd.action);
      match tl with
      | [] -> ()
      | next::_ ->
        let indent' =
          match hd.kind, next.kind with
          | BEGIN, BEGIN -> indent + 1
          | BEGIN, END -> indent
          | END, BEGIN -> indent
          | END, END -> indent - 1
        in
        fprintf fmt "@,%a"
          (iter indent') tl
  in
  let relements = List.rev trace.elements in
  fprintf fmt "@[<v>%a@]" (iter 0) relements
