(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for integer intervals and congruences. *)

open Framework.Essentials
open Framework.Domains.Nonrel.Value_reduced_product
open Ast

let name = "universal.numeric.reductions.integer_interval_congruence"
let debug fmt = Debug.debug ~channel:name fmt

module Reduction : REDUCTION =
struct

  let itv = Values.Integer_interval.Value.id
  let cgr = Values.Integer_congruence.Value.id

  let reduce (man: 'a pool_man) (v: 'a) : 'a =
    debug "reduce %a and %a"
      Values.Integer_interval.Value.print (man.get itv v)
      Values.Integer_congruence.Value.print (man.get cgr v)
    ;
    try
      let i = man.get itv v |> Bot.bot_to_exn in
      let c = man.get cgr v |> Bot.bot_to_exn in

      let reduction = Congruences.IntCong.meet_inter c i in
      let c', i' = Bot.bot_to_exn reduction in
      debug "reduce %a and %a => result: %a and %a"
        Values.Integer_interval.Value.print (Bot.Nb i)
        Values.Integer_congruence.Value.print (Bot.Nb c)
        Values.Integer_interval.Value.print (Bot.Nb i')
        Values.Integer_congruence.Value.print (Bot.Nb c')
      ;

      man.set itv (Bot.Nb i') v |>
      man.set cgr (Bot.Nb c')
    with Bot.Found_BOT ->
      debug "reduce %a and %a => result: ⊥"
        Values.Integer_interval.Value.print (man.get itv v)
        Values.Integer_congruence.Value.print (man.get cgr v)
      ;
      man.bottom
end


let () =
  register_reduction name (module Reduction)
