(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open Mopsa

type alarm_kind +=
   | APyException of expr

let () =
  register_alarm
    {
      compare = (fun default a a' -> match a.alarm_kind, a'.alarm_kind with
                                     | APyException e, APyException e' -> compare_expr e e'
                                     | _ -> default a a');
      pp_token = (fun default fmt a ->
        match a.alarm_kind with
        | APyException e -> Format.fprintf fmt "PyExc(%a)" pp_expr e
        | _ -> default fmt a);
      pp_title = (fun default fmt a ->
        match a.alarm_kind with
        | APyException e -> Format.fprintf fmt "Python Exception: %a" pp_expr e
        | _ -> default fmt a);
      pp_report = (fun default fmt a ->
        match a.alarm_kind with
        | APyException _ -> Format.fprintf fmt "FIXME"
        | _ -> default fmt a);
    }
