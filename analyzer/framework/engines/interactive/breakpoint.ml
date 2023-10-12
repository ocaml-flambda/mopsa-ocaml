(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2021 The MOPSA Project.                               *)
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

(** Breakpoints for the interactive mode *)

open Mopsa_utils
open Core.All
open Format
open Location


(** Breakpoint *)
type breakpoint =
  | B_function of string (** function *)
  (** Break at the beginning of a function  *)

  | B_line of string (** file *) * int (** line *)
  (** Break at line *)

  | B_named of string
  (** Named breakpoint *) 


(** Compare two breakpoints *)
let compare_breakpoint b1 b2 : int =
  match b1, b2 with
  | B_function(f1), B_function(f2) ->
    compare f1 f2

  | B_line(file1,line1), B_line(file2,line2) ->
    Compare.pair compare compare (file1,line1) (file2,line2)

  | B_named b1, B_named b2 ->
    String.compare b1 b2

  | _ -> compare b1 b2


(** Print a breakpoint *)
let pp_breakpoint fmt = function
  | B_function f -> Format.fprintf fmt "%s" f
  | B_line(file,line) -> Format.fprintf fmt "%s:%d" file line
  | B_named b -> Format.fprintf fmt "@%s" b


(** Set of breakpoints *)
module BreakpointSet = SetExt.Make(struct
    type t = breakpoint let
    compare = compare_breakpoint
  end)


(** Print a set of breakpoints *)
let pp_breakpoint_set fmt bs =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") pp_breakpoint)
    (BreakpointSet.elements bs)



(** Exception raised when parsing an invalid breakpoint string *)
exception Invalid_breakpoint_syntax


(** Parse a breakpoint string *)
let parse_breakpoint (s:string) : breakpoint =
  if Str.string_match (Str.regexp "@\\(.+\\)$") s 0
  then
    let name = Str.matched_group 1 s in
    B_named name else

  if Str.string_match (Str.regexp "\\(.+\\):\\([0-9]+\\)$") s 0
  then
    let file = Str.matched_group 1 s in
    let line = int_of_string (Str.matched_group 2 s) in
    B_line(file, line) else

  if Str.string_match (Str.regexp "\\([^0-9].*\\)$") s 0
  then
    let func = Str.matched_group 1 s in
    B_function(func)

  else raise Invalid_breakpoint_syntax

(** Set of active breakpoints *)
let breakpoints = ref BreakpointSet.empty
