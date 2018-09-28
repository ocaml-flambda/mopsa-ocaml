(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for broadcasting C_interval channel to other domains *)

open Framework.Essentials
open Framework.Domains.Reduced_product.Reductions
open Framework.Domains.Reduced_product.Pool
open Ast


let name = "universal.numeric.reductions.intervals_broadcast"
let debug fmt = Debug.debug ~channel:name fmt

module I = Values.Intervals.Value
module O = Relational.Oct
module P = Relational.Poly

module Reduction : Post_reduction.REDUCTION =
struct

  (* This reduction operator is activated when C_interval is published *)
  let trigger = Some Channels.C_interval

  (* Refine the interval of variable [v] in all domains *)
  let broadcast_interval v itv dman cur =
    let doit : type t. t domain -> 'a -> 'a = fun id acc ->
      match O.identify id, P.identify id with
      | Some Eq, None ->
        (* Octagons *)
        let o = dman.get_env O.id acc in
        let itv' = O.get_interval v o in
        if I.subset itv' itv then acc
        else
          let o' = O.set_interval v itv o in
          dman.set_env O.id o' acc

      | None, Some Eq ->
        (* Polyhedra *)
        let p = dman.get_env P.id acc in
        let itv' = P.get_interval v p in
        if I.subset itv' itv then acc
        else
          let p' = P.set_interval v itv p in
          dman.set_env P.id p' acc

      | _ -> acc
    in

    dman.fold {doit} cur

  (* Reduction operator *)
  let reduce stmt (dman: ('a, 'd) domain_man) (nrman: ('a, 'v) nonrel_man) man flow : 'a flow =
    match skind stmt with
    | S_assume(e) ->
      let vars = Framework.Visitor.expr_vars e in
      List.fold_left (fun acc v ->
          let cur = Flow.get T_cur man acc in
          let i1 = nrman.get_var_value I.id v cur in
          let cur' = broadcast_interval v i1 dman cur in
          Flow.set T_cur cur' man acc
        ) flow vars

    | _ -> flow

end

let () =
  Post_reduction.register_reduction name (module Reduction)
