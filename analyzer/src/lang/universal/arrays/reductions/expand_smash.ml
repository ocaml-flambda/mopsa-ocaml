(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for refining an array access concurrently rewritten by
   expansion and smashing *)

open Mopsa
open Framework.Domains.Reduced_product.Reductions
open Framework.Domains.Reduced_product.Pool
open Ast


let name = "universal.arrays.reductions.expand_smash"
let debug fmt = Debug.debug ~channel:name fmt

module Reduction : Eval_reduction.REDUCTION =
struct

  module E = Expand.Domain
  module S = Smash.Domain

  (* Choose between an expand evaluation [e] and a smash evaluation [s] *)
  let choose expand smash eflow sflow range dman man conj =
    match ekind expand with
    | E_var _ ->
      (* In case we computed an expanded cell, we have to make sure that its value
         is as precise as the smash abstraction *)
      let flow' = man.exec (mk_assign expand smash range) sflow |>
                  (* Keep the set of expanded cell from eflow, so just put it to ⊤ here *)
                  Flow.map_token T_cur (dman.set_env E.id E.top) man |>
                  (* Meet will compute the most precise value *)
                  Flow.meet man eflow
      in
      (* Remove smash temporary if any *)
      let flow'' =
        match ekind smash with
        | E_var v when String.sub v.vname 0 4 = "$tmp" ->
          man.exec (mk_remove_var v range) flow'

        | _ -> flow'
      in
      dman.set_eval E.id expand flow'' conj |>
      dman.remove_eval S.id

    | E_constant (C_top _) ->
      (* Expansion domain can not realize the cell, so keep only the smash*)
      dman.remove_eval E.id conj

    | _ -> conj

  (* Reduction operator *)
  let reduce exp dman nrman man conj =
    match dman.get_eval E.id conj, dman.get_eval S.id conj with
    | Some(Some e, eflow), Some(Some s, sflow) -> choose e s eflow sflow exp.erange dman man conj
    | _ -> conj

end

let () =
  Eval_reduction.register_reduction name (module Reduction)
