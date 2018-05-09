(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction rule between cell expansion and smashing memory models. *)

open Framework.Domains.Reduce.Reduction

let name = "c.memory.cell.expand_smashing_reduction"
let debug fmt = Debug.debug ~channel:name fmt

module Rule =
struct

  let refine_eval man exp flow (e1, flow1) (e2, flow2) = BOTH

end

let setup () =
  register_reduction name (module Rule)
