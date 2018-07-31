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

let () =
  Framework.Zone.(register {
      subset = (fun next z1 z2 -> next z1 z2);
      print = (fun next fmt z ->
          match z with
          | Z_universal -> Format.fprintf fmt "universal"
          | _ -> next fmt z
        );
    })
