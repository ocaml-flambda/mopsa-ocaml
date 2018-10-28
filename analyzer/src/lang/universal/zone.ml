(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones for the Universal language. *)

open Framework.Zone

type zone +=
  | Z_universal
  | Z_universal_num
  | Z_universal_string
  | Z_universal_tree

let () =
  register_zone {
      subset = (fun next z1 z2 ->
          match z1, z2 with
            | Z_universal_num, Z_universal -> true
            | Z_universal_string, Z_universal -> true
            | Z_universal_tree, Z_universal -> true
            | _ -> next z1 z2
        );
      print = (fun next fmt z ->
          match z with
          | Z_universal -> Format.fprintf fmt "universal"
          | Z_universal_num -> Format.fprintf fmt "universal/num"
          | Z_universal_string -> Format.fprintf fmt "universal/string"
          | Z_universal_tree -> Format.fprintf fmt "universal/tree"
          | _ -> next fmt z
        );
    }
