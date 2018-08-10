(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for integer intervals and congruences. *)

open Framework.Essentials
open Framework.Domains.Reduced_product.Reductions
open Framework.Domains.Reduced_product.Pool
open Ast


(***************************************************************************)
(*                             First reduction                             *)
(***************************************************************************)

let name = "universal.numeric.reductions.intervals_congruences"
let debug fmt = Debug.debug ~channel:name fmt

module I = Values.Intervals.Value
module C = Values.Congruences.Value
module O = Relational.Oct   

type Post.channel += C_interval_congruence

module Reduction1 : Value_reduction.REDUCTION =
struct

  let reduce (man: 'a value_man) (v: 'a) : 'a with_channel =
    debug "reduce %a and %a"
      I.print (man.get I.id v)
      C.print (man.get C.id v)
    ;
    try
      let i = man.get I.id v |> Bot.bot_to_exn in
      let c = man.get C.id v |> Bot.bot_to_exn in

      let reduction = CongUtils.IntCong.meet_inter c i in
      let c', i' = Bot.bot_to_exn reduction in

      debug "reduce %a and %a => result: %a and %a"
        I.print (Bot.Nb i)
        C.print (Bot.Nb c)
        I.print (Bot.Nb i')
        C.print (Bot.Nb c')
      ;

      let channels =
        if ItvUtils.IntItv.equal i' i then []
        else [C_interval_congruence]
      in
      man.set I.id (Bot.Nb i') v |>
      man.set C.id (Bot.Nb c') |>
      Value_reduction.return ~channels
    with Bot.Found_BOT ->
      debug "reduce %a and %a => result: ⊥"
        I.print (man.get I.id v)
        C.print (man.get C.id v)
      ;
      man.set I.id I.bottom v |>
      man.set C.id C.bottom |>
      Value_reduction.return
end


let () =
  Value_reduction.register_reduction name (module Reduction1)


(***************************************************************************)
(*                            Second reduction                             *)
(***************************************************************************)

let name = "universal.numeric.reductions.intervals_octagons"
let debug fmt = Debug.debug ~channel:name fmt

module Reduction2 : State_reduction.REDUCTION =
struct
  let trigger = Some C_interval_congruence

  let reduce stmt (dman: ('a, 'd) domain_man) (nman: ('a, 'v) nonrel_man) man flow : 'a flow =
    match skind stmt with
    | S_assume(e) ->
      let vars = Framework.Visitor.expr_vars e in
      List.fold_left (fun acc v ->
          let cur = Flow.get T_cur man acc in
          let i1 = nman.get I.id v cur in
          let o = dman.get_state O.id cur in
          let i2 = O.interval v o in
          if I.subset i2 i1 then acc
          else
            let o' = O.refine_interval v i1 o in
            let cur' = dman.set_state O.id o' cur in
            Flow.set T_cur cur' man acc
        ) flow vars

    | _ -> flow

end

let () =
  State_reduction.register_reduction name (module Reduction2)
