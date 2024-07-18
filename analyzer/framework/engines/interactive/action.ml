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
open Location
open Format

type action =
  | Exec of stmt * route
  | Eval of expr * route * semantic

let action_range = function
  | Exec(stmt,_)   -> stmt.srange
  | Eval(expr,_,_) -> expr.erange

let action_vars = function
  | Exec(stmt,_)   -> stmt_vars stmt
  | Eval(expr,_,_) -> expr_vars expr

let action_line_opt action =
  let range = action_range action in
  if not (Location.is_orig_range range) then
    None
  else
    let file = Location.get_range_file range in
    let line = Location.get_range_line range in
    Some(file, line)

(** Get the variables appearing in the line of an action *)
let action_line_vars action =
  match action_line_opt action with
  | None -> []
  | Some(_, line) ->
    let visit_expr acc e =
      if not (Location.is_orig_range e.erange) then VisitParts acc
      else
        let line' = Location.get_range_line e.erange in
        if line = line' then Keep (acc@expr_vars e) else VisitParts acc
    in
    let visit_stmt acc s = VisitParts acc in
    match action with
    | Exec (stmt, _) ->
      fold_stmt visit_expr visit_stmt [] stmt

    | Eval (expr, _, _) ->
      fold_expr visit_expr visit_stmt [] expr


(* Get the number of digits of an integer *)
let nb_digits n =
  int_of_float (log10 (float_of_int n)) + 1

(* Right align an integer *)
let pp_right_align_int width fmt i =
  let digits = nb_digits i in
  fprintf fmt "%s%d"
    (String.init (width - digits) (fun _ -> ' '))
    i

let pp_right_align width pp fmt x =
  let s = asprintf "%a" pp x in
  let len = String.length s in
  fprintf fmt "%s%s"
    (String.init (width - len) (fun _ -> ' '))
    s

(* Format has issues when identing in presence of unicode characters. So we
     do it manually. *)
let fix_string_indentation indent s =
  let lines = String.split_on_char '\n' s in
  match lines with
  | [] -> ""
  | [_] -> s
  | hd::tl ->
    let lines' = hd :: List.map (fun l -> (String.make indent ' ') ^ "    " ^ l) tl in
    String.concat "\n" lines'

let truncate_string s =
  let lines = String.split_on_char '\n' s in
  match lines with
  | [] | [_] -> s
  | hd::tl -> hd ^ " ..."

let pp_exec ~truncate ~indent fmt stmt =
  let s = asprintf "@[<v>%a@]" pp_stmt stmt in
  fprintf fmt "%a %a %s %a"
    Debug.(color 45 pp_print_string) "𝕊"
    Debug.(color 45 pp_print_string) "⟦"
    (if truncate then truncate_string s else fix_string_indentation indent s)
    Debug.(color 45 pp_print_string) "⟧"

let pp_eval ~truncate ~indent fmt exp =
  let s = asprintf "@[<v>%a@]" pp_expr exp in
  fprintf fmt "%a %a %s : %a %a"
    Debug.(color 209 pp_print_string) "𝔼"
    Debug.(color 209 pp_print_string) "⟦"
    (if truncate then truncate_string s else fix_string_indentation indent s)
    pp_typ exp.etyp
    Debug.(color 209 pp_print_string) "⟧"

let pp_action ?(truncate=false) ?(indent=0) fmt action =
  match action with
  | Exec(stmt,_) -> pp_exec ~truncate ~indent fmt stmt
  | Eval(exp,_,_) -> pp_eval ~truncate ~indent fmt exp

(** Print source code of an action *)
let pp_action_source_code fmt action =
  (* Entry point *)
  let rec doit () =
    let range = action_range action in
    if not (is_orig_range (untag_range range)) then () else
      let start = get_range_start range in
      let file = start.pos_file in
      let line = start.pos_line in
      if not (Sys.file_exists file) then ()
      else
        let ch = open_in file in
        let before,at,after = read_lines_around ch line in
        let max_line = line + List.length after in
        let max_digits = nb_digits max_line in
        List.iter (pp_surrounding_line max_digits std_formatter) before;
        pp_target_line max_digits std_formatter at;
        List.iter (pp_surrounding_line max_digits std_formatter) after;
        close_in ch
  (* Read lines before and after a target line *)
  and read_lines_around ch line =
    let rec iter before at after i =
      try
        let l = input_line ch in
        if i < line - 5 then iter before at after (i+1) else
        if i > line + 5 then (before,at,after)
        else
        if i < line then iter ((i,l)::before) at after (i+1) else
        if i = line then iter before (i,l) after (i+1)
        else iter before at ((i,l)::after) (i+1)
      with End_of_file -> (before,at,after)
    in
    let before,at,after = iter [] (0,"") [] 1 in
    List.rev before, at, List.rev after
  (* Print a surrounding line *)
  and pp_surrounding_line max_line fmt (i,l) =
    fprintf fmt "   %a  %s@."
      (pp_right_align_int max_line) i
      l
  (* Print the target line *)
  and pp_target_line max_line fmt (i,l) =
    fprintf fmt " %a %a  %a@."
      Debug.(color 118 pp_print_string) "►"
      Debug.(color 118 (pp_right_align_int max_line)) i
      Debug.(color 118 pp_print_string) l
  in
  doit ()

