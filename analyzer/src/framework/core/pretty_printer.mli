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

(** Printer - pretty-printing of expressions values *)

open Ast.Var
open Ast.Expr
open Yojson.Basic


module StringMap = MapExt.StringMap

type section =
  | Map    of section StringMap.t
  | List   of section list
  | String of string

type domain = string

type printer

val empty_printer : printer

val get_prev_exprs : printer -> expr list

val add_expr : printer -> expr -> unit

val mem_prev_expr : printer -> expr -> bool

val pp_string : printer -> domain -> string -> unit

val pp_map : printer -> domain -> (string*section) list -> unit

val pp_map_binding : printer -> domain -> string -> section -> unit

val pp_list : printer -> domain -> section list -> unit

val pp_list_element : printer -> domain -> section -> unit

val flush : Format.formatter -> printer -> unit

val printer_to_json : printer -> Yojson.t
