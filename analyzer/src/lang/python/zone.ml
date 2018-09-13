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
      subset = (fun next z1 z2 ->
        match z1, z2 with
        | _ -> next z1 z2
        );
      print = (fun next fmt z ->
          match z with
          | Z_py -> Format.fprintf fmt "py"
          | _ -> next fmt z
        );
    }
