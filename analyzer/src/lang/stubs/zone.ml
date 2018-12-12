(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones for the Stubs language. *)

open Mopsa
open Ast


type zone +=
  | Z_stubs

let () =
  register_zone {
    zone = Z_stubs;
    subset = None;
    name = "Stubs";
    eval = (fun exp ->
        match ekind exp with
        | E_stub_call _           -> Process
        | E_stub_return           -> Keep
        | E_stub_builtin_call _   -> Process
        | _                       -> Process
      );
    }
