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
  | Z_u
  | Z_u_num

let () =
  register_zone {
    zone = Z_u;
    subset = None;
    name = "u";
    exec_template = (fun stmt -> Process);
    eval_template = (fun exp -> Process);
  };

  register_zone {
    zone = Z_u_num;
    subset = Some Z_u;
    name = "u/num";
    exec_template = (fun stmt -> Process);
    eval_template = (fun exp -> Process);
  };

  ()
