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

let name = "c.memory.cell.reductions.expand_smashing"
let debug fmt = Debug.debug ~channel:name fmt

module Rule =
struct

  let refine_offset_smash_cell man ctx ov oflow sv sflow oslct sslct =
    match man.ask ctx (QIntInterval ov) oflow, man.ask ctx (QIntInterval sv) sflow with
    | Some itv1, Some itv2 -> if Int.leq itv1 itv2 then oslct else sslct
    | _ -> assert false

  let refine_var_case man ctx exp flow e1 v1 flow1 e2 v2 flow2 =
    match vkind v1, vkind v2 with
    | Smashing.(V_smash_cell _), Expand.(V_expand_cell (AnyCell _)) -> HEAD
    | Expand.(V_expand_cell (AnyCell _)), Smashing.(V_smash_cell _) -> TAIL

    | Smashing.(V_smash_cell sc), Expand.(V_expand_cell (OffsetCell oc)) ->
      refine_offset_smash_cell man ctx e2 flow2 e1 flow1 TAIL HEAD

    | Expand.(V_expand_cell (OffsetCell oc)), Smashing.(V_smash_cell sc) ->
      refine_offset_smash_cell man ctx e1 flow1 e2 flow2 HEAD TAIL

    | _ -> BOTH

  let refine_eval man ctx exp flow (e1, flow1) (e2, flow2) =
    match ekind e1, ekind e2 with
    | E_var v1, E_var v2 -> refine_var_case man ctx exp flow e1 v1 flow1 e2 v2 flow2
    | _ -> BOTH

end

let setup () =
  register_reduction name (module Rule)
