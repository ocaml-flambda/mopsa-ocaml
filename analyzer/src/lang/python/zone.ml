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
   | Z_py_value
   | Z_py_addr
   | Z_py_types

let () =
  register_zone {
    zone = Z_py;
    name = "Z_py";
    subset = None;
    eval = (fun exp -> Process);
    };
  register_zone {
    zone = Z_py_addr;
    name = "Z_py_addr";
    subset = Some Z_py;
    eval = (fun exp -> Process);
    };
  register_zone {
    zone = Z_py_types;
    name = "Z_py_types";
    subset = Some Z_py_types;
    eval = (fun exp -> Process);
    };
  register_zone {
    zone = Z_py_value;
    name = "Z_py_value";
    subset = Some Z_py;
    eval = (fun exp -> Process);
  }
