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

let empty_loc = { loc_line = -1; loc_column = -1; loc_file = "<invalid>"; }
let empty_range = { range_begin = empty_loc; range_end = empty_loc; }

let loc_is_empty l =
  l.loc_line < 0 || l.loc_column < 0 || l.loc_file = "<invalid>"

let range_is_empty r =
  loc_is_empty r.range_begin && loc_is_empty r.range_end

(** {1 Types} *)

let target_unsigned_int = function
  | Target_NoInt -> Target_NoInt
  | Target_SignedChar | Target_UnsignedChar -> Target_UnsignedChar
  | Target_SignedShort | Target_UnsignedShort -> Target_UnsignedShort
  | Target_SignedInt | Target_UnsignedInt -> Target_UnsignedInt
  | Target_SignedLong | Target_UnsignedLong -> Target_UnsignedLong
  | Target_SignedLongLong | Target_UnsignedLongLong -> Target_UnsignedLongLong
(** Unsigned version of an int. *)

let int_type_align (t:target_info) = function
  | Target_NoInt -> 0
  | Target_SignedChar | Target_UnsignedChar -> t.target_char_align
  | Target_SignedShort | Target_UnsignedShort -> t.target_short_align
  | Target_SignedInt | Target_UnsignedInt -> t.target_int_align
  | Target_SignedLong | Target_UnsignedLong -> t.target_long_align
  | Target_SignedLongLong | Target_UnsignedLongLong -> t.target_long_long_align
(** Alignment of an int type for the target. *)

let real_type_align (t:target_info) = function
  | Target_NoFloat -> 0
  | Target_Float -> t.target_float_align
  | Target_Double -> t.target_double_align
  | Target_LongDouble -> t.target_long_double_align
  | Target_Float128 -> t.target_float128_align
(** Alignment of a real (float) type for the target. *)

let int_type_width (t:target_info) = function
  | Target_NoInt -> 0
  | Target_SignedChar | Target_UnsignedChar -> t.target_char_width
  | Target_SignedShort | Target_UnsignedShort -> t.target_short_width
  | Target_SignedInt | Target_UnsignedInt -> t.target_int_width
  | Target_SignedLong | Target_UnsignedLong -> t.target_long_width
  | Target_SignedLongLong | Target_UnsignedLongLong -> t.target_long_long_width
(** Bit-width of an int type for the target. *)

let real_type_width (t:target_info) = function
  | Target_NoFloat -> 0
  | Target_Float -> t.target_float_width
  | Target_Double -> t.target_double_width
  | Target_LongDouble -> t.target_long_double_width
  | Target_Float128 -> t.target_float128_width
(** Bit-width of a real (float) type for the target. *)



(** {1 Debugging utilities} *)

external dump_block: recursive:bool -> 'a -> unit = "mlclang_dump_block"
