(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Stub alarms *)

open Mopsa
open Ast

type alarm_kind +=
  | A_stub_invalid_require

let () =
  register_alarm
    {
      compare = (fun default a b ->default a b);
      pp_token = (fun default fmt a ->
          match a.alarm_kind with
          | A_stub_invalid_require -> Format.fprintf fmt "stub-req"
          | _ -> default fmt a
        );
      pp_title = (fun default fmt a ->
          match a.alarm_kind with
          | A_stub_invalid_require -> Format.fprintf fmt "Invalid stub requirement"
          | _ -> default fmt a
        );
      pp_report = (fun default fmt a ->
          match a.alarm_kind with
          | A_stub_invalid_require -> Format.fprintf fmt "TODO"
          | _ -> default fmt a
        )
    };
