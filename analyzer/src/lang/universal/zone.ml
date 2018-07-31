(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones for the Universal language. *)

type Framework.Zone.t +=
  | Z_universal
  | Z_universal_int

let () =
  Framework.Zone.(register {
      subset = (fun next z1 z2 ->
          match z1, z2 with
          | Z_universal_int, Z_universal -> true
          | _ -> next z1 z2
        );
      print = (fun next fmt z ->
          match z with
          | Z_universal -> Format.fprintf fmt "universal"
          | Z_universal_int -> Format.fprintf fmt "universal/int"
          | _ -> next fmt z
        );
    })
