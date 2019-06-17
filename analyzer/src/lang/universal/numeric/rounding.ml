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
    category = "Numeric";
    spec = ArgExt.Symbol (
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
    doc = "rounding mode of floating-point computations.";
    default = "near";
  }
