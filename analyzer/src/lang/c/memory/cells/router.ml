(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Evaluation router for cells *)

open Framework.Domains.Router

module Router =
struct

  let name = "c.memory.cells.router"

  let table = [
    {
      src = Zone.Z_c;
      dst = Cell.Z_c_cell;
      path = [Zone.Z_c_scalar]
    }
  ]
end

let () =
  register_router (module Router)
