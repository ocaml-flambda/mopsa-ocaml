(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Alarm for uncaught Python exceptions. *)

open Framework.Alarm

type alarm_kind +=
  | UncaughtException of string

let () =
  register_alarm_compare (fun next a1 a2 ->
      match a1.alarm_kind, a2.alarm_kind with
      | UncaughtException exn1, UncaughtException exn2 -> compare exn1 exn2
      | _ -> next a1 a2
    );

  register_pp_alarm (fun default fmt alarm ->
      match alarm.alarm_kind with
      | UncaughtException name ->
        Format.fprintf fmt "Uncaught exception %s" name
      | _ -> default fmt alarm
    );
