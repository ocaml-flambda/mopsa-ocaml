(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for refining an array access concurrently rewritten by
   expansion and smashing *)

open Framework.Essentials
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
  let choose e s eflow sflow range dman man conj =
    match ekind e with
    | E_var _ ->
      (* In case we computed an expanded cell, we have to make sure that its value
         is as precise as the smash abstraction *)
      let flow' = man.exec (mk_assign e s range) sflow |>
                  Flow.meet man eflow
      in
      (* Remove smash temporary if any *)
      let flow'' =
        match ekind s with
        | E_var v when String.sub v.vname 0 4 = "$tmp" ->
          man.exec (mk_remove_var v range) flow'

        | _ -> flow'
      in
      dman.set_eval E.id e flow'' conj |>
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
