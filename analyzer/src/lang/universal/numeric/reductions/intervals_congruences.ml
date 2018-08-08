(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for integer intervals and congruences. *)

open Framework.Essentials
open Framework.Domains.Reduced_product.Reductions.Value_reduction
open Framework.Domains.Reduced_product.Pool
open Ast

let name = "universal.numeric.reductions.intervals_congruences"
let debug fmt = Debug.debug ~channel:name fmt

module Reduction : REDUCTION =
struct

  module I = Values.Intervals.Value
  module C = Values.Congruences.Value   

  let reduce (man: 'a value_man) (v: 'a) : 'a =
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

      man.set I.id (Bot.Nb i') v |>
      man.set C.id(Bot.Nb c')
    with Bot.Found_BOT ->
      debug "reduce %a and %a => result: ⊥"
        I.print (man.get I.id v)
        C.print (man.get C.id v)
      ;
      man.set I.id I.bottom v |>
      man.set C.id C.bottom
end


let () =
  register_reduction name (module Reduction)
