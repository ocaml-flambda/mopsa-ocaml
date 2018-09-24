(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones for the C language. *)

open Framework.Zone

type zone +=
  | Z_py

let () =
  register_zone {
    zone = Z_py;
    name = "Z_py";
    subset = None;
    eval = (fun exp -> Process);
  }
