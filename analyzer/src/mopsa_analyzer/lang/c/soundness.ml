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

(** Soundness assumptions *)

open Mopsa
open Common.Base
open Format


type assumption_kind +=
  | A_ignore_unsupported_format_string
  | A_ignore_undefined_function of string
  | A_ignore_undetermined_function_pointer of expr
  | A_ignore_modification_undetermined_pointer of expr
  | A_ignore_undetermined_exit_functions

let () =
  register_assumption {
    print = (fun next fmt -> function
        | A_ignore_unsupported_format_string ->
          fprintf fmt "ignoring unsupported format"

        | A_ignore_undefined_function f ->
          fprintf fmt "ignoring side-effects of calling undefined function '%a'"
            (Debug.bold pp_print_string) f

        | A_ignore_undetermined_function_pointer ptr ->
          fprintf fmt "ignoring side-effects of calling undetermined function pointer '%a'"
            (Debug.bold pp_expr) ptr

        | A_ignore_modification_undetermined_pointer ptr ->
          fprintf fmt "ignoring modification of blocks pointed by undetermined pointer '%a'"
            (Debug.bold pp_expr) ptr

        | A_ignore_undetermined_exit_functions ->
          fprintf fmt "ignoring side-effect of undetermined exit functions"

        | a -> next fmt a
      );
    compare = (fun next a1 a2 ->
        match a1,a2 with
        | A_ignore_unsupported_format_string, A_ignore_unsupported_format_string -> 0

        | A_ignore_undefined_function f1, A_ignore_undefined_function f2 ->
          String.compare f1 f2

        | A_ignore_undetermined_function_pointer ptr1, A_ignore_undetermined_function_pointer ptr2 ->
          compare_expr ptr1 ptr2

        | A_ignore_modification_undetermined_pointer ptr1, A_ignore_modification_undetermined_pointer ptr2 ->
          compare_expr ptr1 ptr2

        | A_ignore_undetermined_exit_functions, A_ignore_undetermined_exit_functions -> 0

        | _ -> next a1 a2
      );
  }
