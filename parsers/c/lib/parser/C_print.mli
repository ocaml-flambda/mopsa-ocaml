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

(**
  C_print - Printing, converts C AST to valid C code
 *)

open C_AST

val integer_suffix : integer_type -> string
val float_suffix : float_type -> string
(** Suffix for litterals. *)

val string_of_signedness : signedness -> string
val string_of_integer_type : integer_type -> string
val string_of_float_type : float_type -> string
val string_of_record_kind : record_kind -> string
val string_of_qualifier : qualifier -> string
val string_of_binary_arithmetic : binary_arithmetic -> string
val string_of_binary_logical : binary_logical -> string
val string_of_binary_operator : binary_operator -> string
val string_of_unary_operator : unary_operator -> string
val string_of_inc_direction : inc_direction -> string
val string_of_var_decl : variable -> string
val string_of_var_advance_decl : variable -> string
val string_of_func_decl : func -> string
val string_of_func_proto : func -> string
val string_of_expr : expr -> string
val string_of_type : typ -> string
val string_of_type_qual : type_qual -> string
val string_of_string_literal : string -> string
val string_of_enum_decl : enum_type -> string
val string_of_record_decl : record_type -> string
val string_of_typedef : typedef -> string
val string_of_statement : statement -> string
(** Convert SAST types to string. *)

val print_project : ?verbose:bool -> out_channel -> project -> unit
(** Print a whole project as a valid C source. *)
