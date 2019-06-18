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
  Clang_parser - Extracting Clang AST to OCaml
 *)

open Clang_AST
open Clang_utils


(** {1 Version} *)


let version = "1.0"



(** {1 Target information} *)

external get_default_target_options: unit -> target_options = "mlclang_get_default_target_options"
(** Returns the default target, which corresponds to the host. *)

external get_target_info: target_options -> target_info = "mlclang_get_target_info"
(** Gets the target informations (type width and alignment, ...) for the given target. *)


(** {1 Parsing} *)
                                                            
type parse_result = {
  parse_decl: decl; (** AST. *)
  parse_diag : diagnostic list; (** Warnings and errors. *)
  parse_comments: comment list; (** C/C++ comments. *)
  parse_macros: macro list; (** Macros. *)
  parse_files: string list; (** Files read during parsing. *)
}
      
external parse: command:string -> target:target_options -> filename:string -> args:string array -> parse_result = "mlclang_parse"
(** Parse the source file with the specified command (e.g., "clang" or "clang++") for the specified target, given the the specified compile-time options. *)
