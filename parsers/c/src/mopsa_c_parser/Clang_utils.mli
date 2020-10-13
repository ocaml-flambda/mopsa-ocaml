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
  Clang_utils - Utilities for the Clang AST
 *)

open Clang_AST


(** {1 Locations} *)

val empty_loc : loc
val empty_range : range
val loc_is_empty : loc -> bool
val range_is_empty : range -> bool


(** {1 Types} *)

val target_unsigned_int :
  target_int_type -> target_int_type

val int_type_align : target_info -> target_int_type -> int

val real_type_align :
  target_info -> target_real_type -> int

val int_type_width : target_info -> target_int_type -> int

val real_type_width :
  target_info -> target_real_type -> int

                         
(** {1 Debugging utilities} *)

external dump_block: recursive:bool -> 'a -> unit = "mlclang_dump_block"
