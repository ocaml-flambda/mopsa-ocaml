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
  | Z_c_num
  | Z_c_lval

let () =
  register_zone {
      subset = (fun next z1 z2 ->
        match z1, z2 with
        | Z_c_scalar, Z_c -> true
        | Z_c_num, Z_c_scalar -> true
        | _ -> next z1 z2
        );
      print = (fun next fmt z ->
          match z with
          | Z_c -> Format.fprintf fmt "c"
          | Z_c_scalar -> Format.fprintf fmt "c/scalar"
          | Z_c_num -> Format.fprintf fmt "c/scalar/num"
          | Z_c_lval -> Format.fprintf fmt "c/lval"
          | _ -> next fmt z
        );
    }
