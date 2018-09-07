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
  | Z_c
  | Z_c_scalar

let () =
  register_zone {
      subset = (fun next z1 z2 ->
        match z1, z2 with
        | Z_c_scalar, Z_c -> true
        | _ -> next z1 z2
        );
      print = (fun next fmt z ->
          match z with
          | Z_c -> Format.fprintf fmt "c"
          | Z_c_scalar -> Format.fprintf fmt "c/scalar"
          | _ -> next fmt z
        );
    }
