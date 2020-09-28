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

(** Print - pretty-printing of expressions values *)

open Ast.Var
open Ast.Expr
open Yojson.Basic

type printer

val empty_printer : unit -> printer

val get_printed_exprs : printer -> expr list

val add_printed_expr : printer -> expr -> unit

val mem_printed_expr : printer -> expr -> bool

type print_selector =
  | Key    of string
  | Index  of int

val fkey : ('a, Format.formatter, unit, print_selector) format4 -> 'a

val pkey : (printer -> 'a -> unit) -> 'a -> print_selector

type print_path = print_selector list

type print_symbols = {
  sym_begin : string;
  sym_sep   : string;
  sym_end   : string;
}

type print_object =
  | Empty
  | Bool   of bool
  | Int    of Z.t
  | Float  of float
  | String of string
  | Map    of print_object MapExt.StringMap.t * print_symbols
  | List   of print_object list * print_symbols

val pp_obj : ?path:print_path -> (printer -> print_object -> unit)

val pp_string : ?path:print_path -> (printer -> string -> unit)

val pp_bool : ?path:print_path -> (printer -> bool -> unit)

val pp_int : ?path:print_path -> (printer -> int -> unit)

val pp_z : ?path:print_path -> (printer -> Z.t -> unit)

val pp_float : ?path:print_path -> (printer -> float -> unit)

val pp_list : ?path:print_path -> ?sym_begin:string -> ?sym_sep:string -> ?sym_end:string -> (printer -> 'a -> unit) -> (printer -> 'a list -> unit)

val pp_obj_list : ?path:print_path -> ?sym_begin:string -> ?sym_sep:string -> ?sym_end:string -> (printer -> print_object list -> unit)

val pp_append_list : ?path:print_path -> (printer -> 'a -> unit) -> (printer -> 'a -> unit)

val pp_append_obj_list : ?path:print_path -> (printer -> print_object -> unit)

val pp_map : ?path:print_path -> ?sym_begin:string -> ?sym_sep:string -> ?sym_end:string -> (printer -> 'k -> unit) -> (printer -> 'v -> unit) -> (printer -> ('k * 'v) list -> unit)

val pp_obj_map : ?path:print_path -> ?sym_begin:string -> ?sym_sep:string -> ?sym_end:string -> (printer -> (print_object * print_object) list -> unit)

val pp_set : ?path:print_path -> ?sym_sep:string -> (printer -> 'a -> unit) -> (printer -> 'a list -> unit)

val pp_obj_set : ?path:print_path -> ?sym_sep:string -> (printer -> print_object list -> unit)

val pp_tuple : ?path:print_path -> ?sym_sep:string -> (printer -> 'a -> unit) -> (printer -> 'a list -> unit)

val pp_obj_tuple : ?path:print_path -> ?sym_sep:string -> (printer -> print_object list -> unit)

val pp_sequence : ?path:print_path -> ?sym_sep:string -> (printer -> 'a -> unit) -> (printer -> 'a list -> unit)

val pp_obj_sequence : ?path:print_path -> ?sym_sep:string -> (printer -> print_object list -> unit)

val pp_boxed : ?path:print_path -> (printer -> 'a -> unit) -> (printer -> 'a -> unit)

val pp_boxed_format : ?path:print_path -> (printer -> ('a, Format.formatter, unit, unit) format4 -> 'a)

val boxed : (printer -> 'a -> unit) -> 'a -> print_object

val boxed_format : ('a, Format.formatter, unit, print_object) format4 -> 'a

val find_print_object : printer -> print_path -> print_object

val format : (printer -> 'a -> unit) -> (Format.formatter -> 'a -> unit)

val unformat : ?path:print_path -> (Format.formatter -> 'a -> unit) -> (printer -> 'a -> unit)

val fprint : Format.formatter -> printer -> unit

val printer_to_json : printer -> Yojson.Basic.t
