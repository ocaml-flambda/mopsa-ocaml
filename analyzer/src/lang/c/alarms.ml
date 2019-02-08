(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
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
  | AVaArgNoNext

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
          | AVaArgNoNext -> Format.fprintf fmt "va-arg"
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
          | AVaArgNoNext -> Format.fprintf fmt "No next argument for va_arg "
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
          | AVaArgNoNext -> Format.fprintf fmt ""
          | _ -> default fmt a
        )
    };
