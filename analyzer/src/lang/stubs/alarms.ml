(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Stub alarms *)

open Framework.Essentials
open Ast

type alarm_kind +=
  | A_stub_invalid_require of requires with_range

let () =
  register_alarm
    {
      compare = (fun default a b ->
          match a.alarm_kind, b.alarm_kind with
          | A_stub_invalid_require req1, A_stub_invalid_require req2 ->
            compare_range req1.range req2.range
          | _ -> default a b
        );
      pp_token = (fun default fmt a ->
          match a.alarm_kind with
          | A_stub_invalid_require(req) -> Format.fprintf fmt "stub:req:%a" pp_range req.range
          | _ -> default fmt a
        );
      pp_title = (fun default fmt a ->
          match a.alarm_kind with
          | A_stub_invalid_require req -> Format.fprintf fmt "Invalid stub requirement at %a" pp_range req.range
          | _ -> default fmt a
        );
      pp_report = (fun default fmt a ->
          match a.alarm_kind with
          | A_stub_invalid_require req -> Format.fprintf fmt "TODO"
          | _ -> default fmt a
        )
    };
