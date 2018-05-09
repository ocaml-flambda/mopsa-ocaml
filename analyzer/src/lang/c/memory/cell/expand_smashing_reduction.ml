(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction rule between cell expansion and smashing memory models. *)

open Framework.Ast
open Framework.Domains.Reduce.Reduction

let name = "c.memory.cell.expand_smashing_reduction"
let debug fmt = Debug.debug ~channel:name fmt

module Rule =
struct

  let refine_var_case man exp flow (v1, flow1) (v2, flow2) =
    match vkind v1, vkind v2 with
    | Expand.(V_expand_cell (OffsetCell _)), Smashing.(V_smash_cell _) -> HEAD
    | Smashing.(V_smash_cell _), Expand.(V_expand_cell (AnyCell _)) -> HEAD

    | Smashing.(V_smash_cell _), Expand.(V_expand_cell (OffsetCell _)) -> TAIL
    | Expand.(V_expand_cell (AnyCell _)), Smashing.(V_smash_cell _) -> TAIL

    | _ -> BOTH

  let refine_eval man exp flow (e1, flow1) (e2, flow2) =
    match ekind e1, ekind e2 with
    | E_var v1, E_var v2 -> refine_var_case man exp flow (v1, flow1) (v2, flow2)
    | _ -> BOTH

end

let setup () =
  register_reduction name (module Rule)
