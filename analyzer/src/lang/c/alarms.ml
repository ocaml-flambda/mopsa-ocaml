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

(** Alarms for C runtime errors *)

open Mopsa

type alarm_kind +=
  | AOutOfBound
  | ANullDeref
  | AInvalidDeref
  | ADivideByZero
  | AIntegerOverflow
  | AIllegalPointerDiff
  | AIllegalPointerOrder
  | AVaArgNoNext
  | AReadOnlyModification

let () =
  register_alarm
    {
      compare = (fun default a b -> default a b);
      pp_token = (fun default fmt a ->
          match a.alarm_kind with
          | AOutOfBound -> Format.fprintf fmt "out-bound"
          | ANullDeref -> Format.fprintf fmt "null-deref"
          | AInvalidDeref -> Format.fprintf fmt "invalid-deref"
          | ADivideByZero -> Format.fprintf fmt "div-zero"
          | AIntegerOverflow -> Format.fprintf fmt "int-overflow"
          | AIllegalPointerDiff -> Format.fprintf fmt "ptr-diff"
          | AIllegalPointerOrder -> Format.fprintf fmt "ptr-order"
          | AVaArgNoNext -> Format.fprintf fmt "va-arg"
          | AReadOnlyModification -> Format.fprintf fmt "readonly"
          | _ -> default fmt a
        );
      pp_title = (fun default fmt a ->
          match a.alarm_kind with
          | AOutOfBound -> Format.fprintf fmt "Out of bound access"
          | ANullDeref -> Format.fprintf fmt "Null pointer dereference"
          | AInvalidDeref -> Format.fprintf fmt "Invalid pointer dereference"
          | ADivideByZero -> Format.fprintf fmt "Division by zero"
          | AIntegerOverflow -> Format.fprintf fmt "Integer overflow"
          | AIllegalPointerDiff -> Format.fprintf fmt "Illegal pointer difference"
          | AIllegalPointerOrder -> Format.fprintf fmt "Illegal pointer comparison"
          | AVaArgNoNext -> Format.fprintf fmt "No next argument for va_arg"
          | AReadOnlyModification -> Format.fprintf fmt "Modification of a readonly memory"
          | _ -> default fmt a
        );
      pp_report = (fun default fmt a ->
          match a.alarm_kind with
          | AOutOfBound -> Format.fprintf fmt ""
          | ANullDeref -> Format.fprintf fmt ""
          | AInvalidDeref -> Format.fprintf fmt ""
          | ADivideByZero -> Format.fprintf fmt ""
          | AIntegerOverflow -> Format.fprintf fmt ""
          | AIllegalPointerDiff -> Format.fprintf fmt ""
          | AIllegalPointerOrder -> Format.fprintf fmt ""
          | AVaArgNoNext -> Format.fprintf fmt ""
          | AReadOnlyModification -> Format.fprintf fmt ""
          | _ -> default fmt a
        )
    };
