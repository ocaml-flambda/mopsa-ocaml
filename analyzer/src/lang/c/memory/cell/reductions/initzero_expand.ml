(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction rule between cell expansion and smashing memory models. *)

open Framework.Ast
open Framework.Manager
open Framework.Domains.Reduce.Reduction
open Universal.Numeric.Query
open Universal.Numeric.Values

let name = "c.memory.cell.reductions.initzero_expand"
let debug fmt = Debug.debug ~channel:name fmt

module Rule =
struct

  let refine_general man ctx (e1, flow1) (e2, flow2) =
    match man.ask ctx (QIntInterval e1) flow1, man.ask ctx (QIntInterval e2) flow2 with
    | Some itv1, Some itv2 -> if Int.leq itv1 itv2 then HEAD else if Int.leq itv2 itv1 then TAIL else BOTH
    | _ -> BOTH

  let refine_eval man ctx exp flow (e1, flow1) (e2, flow2) =
    refine_general man ctx (e1, flow1) (e2, flow2)
end

let setup () =
  register_reduction name (module Rule)
