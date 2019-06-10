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


(** Common utility functions for builtins *)



(** List of builtin functions *)
let is_builtin_function = function
  | "__builtin_constant_p"
  | "__builtin_va_start"
  | "__builtin_va_end"
  | "__builtin_va_copy"
  | "printf"
  | "__printf_chk"
  | "__fprintf_chk"
  | "__vfprintf_chk"
  | "__vprintf_chk"
  | "_mopsa_range_char"
  | "_mopsa_range_unsigned_char"
  | "_mopsa_range_short"
  | "_mopsa_range_unsigned_short"
  | "_mopsa_range_int"
  | "_mopsa_range_unsigned_int"
  | "_mopsa_range_long"
  | "_mopsa_range_unsigned_long"
  | "_mopsa_range_long_long"
  | "_mopsa_range_unsigned_long_long"
  | "_mopsa_set_debug_channels"
  | "_mopsa_range"
  | "_mopsa_rand"
  | "_mopsa_rand_int"
  | "_mopsa_rand_unsigned_long"
  | "_mopsa_panic"
  | "_mopsa_print"
  | "_mopsa_assert_exists"
  | "_mopsa_assert"
  | "_mopsa_assert_safe"
  | "_mopsa_assert_unsafe"
  | "_mopsa_assert_error"
  | "_mopsa_assert_error_exists"
  | "_mopsa_assert_error_at_line"
  | "_mopsa_fd_to_int"
  | "_mopsa_int_to_fd"
  | "_mopsa_cf_part"
  | "_mopsa_cf_merge"
    -> true

  | _ -> false
