(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Rounding mode. *)

open Mopsa

let name = "universal.numeric.rounding"

(****************************************************************************)
(**                      {2 Command line options}                           *)
(****************************************************************************)

let opt_float_rounding = ref Apron.Texpr1.Near

let () =
  register_standalone_option name {
    key = "-float-rounding-mode";
    spec = Arg.Symbol (
        ["near"; "zero"; "up"; "down"; "rnd"],
        (function
          | "near" -> opt_float_rounding := Apron.Texpr1.Near
          | "zero" -> opt_float_rounding := Apron.Texpr1.Zero
          | "up"   -> opt_float_rounding := Apron.Texpr1.Up
          | "down" -> opt_float_rounding := Apron.Texpr1.Down
          | "rnd"  -> opt_float_rounding := Apron.Texpr1.Rnd
          | x -> Exceptions.panic "unknown rounding mode %s" x
        )
      );
    doc = "rounding mode of floating-point computations. (default: near)";
  }
