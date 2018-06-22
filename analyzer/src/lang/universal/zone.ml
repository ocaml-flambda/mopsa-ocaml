(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones for the Universal language. *)

type Framework.Zone.t +=
  | Z_num
  | Z_num_int
  | Z_num_float
  | Z_heap

let () =
  Framework.Zone.register_partial_order (fun next z1 z2 ->
      match z1, z2 with
      | (Z_num_int | Z_num_float), Z_num -> true
      | _ -> next z1 z2
    );
  Framework.Zone.register_pp (fun next fmt z ->
      match z with
      | Z_heap -> Format.fprintf fmt "heap"
      | Z_num -> Format.fprintf fmt "num"
      | Z_num_int -> Format.fprintf fmt "int"
      | Z_num_float -> Format.fprintf fmt "real"
      | _ -> next fmt z
    )
