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


type pprint_section =
  | Map    of pprint_section MapExt.StringMap.t
  | List   of pprint_section list
  | String of string

type domain = string

type pprinter

val empty_pprinter : pprinter

val get_pprinter_exprs : pprinter -> expr list

val add_pprinter_expr : pprinter -> expr -> unit

val mem_pprinter_expr : pprinter -> expr -> bool

val pprint_string : pprinter -> domain -> string -> unit

val pprint_map : pprinter -> domain -> (string*pprint_section) list -> unit

val pprint_map_binding : pprinter -> domain -> string -> pprint_section -> unit

val pprint_list : pprinter -> domain -> pprint_section list -> unit

val pprint_list_element : pprinter -> domain -> pprint_section -> unit

val flush_pprinter : Format.formatter -> pprinter -> unit

val pprinter_to_json : pprinter -> Yojson.t
