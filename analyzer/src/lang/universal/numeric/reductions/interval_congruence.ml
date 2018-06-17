(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduced product of integer intervals and congruences. *)

open Framework.Essentials
open Framework.Domains.Reductions.Value_reduction
open Ast

let name = "universal.numeric.reductions.integer_congruence"
let debug fmt = Debug.debug ~channel:name fmt

module I = Values.Int.Value
module C = Values.Congruence.Value

type _ key +=
  | Int : I.t key
  | Cgr : C.t key

module Reduction : REDUCTION =
struct

  let reduce (man: 'a Pool.manager) (v: 'a) : 'a =
    try
      let itv = man.get Int v |> Bot.bot_to_exn in
      let cgr = man.get Cgr v |> Bot.bot_to_exn in
      let reduction = Congruences.IntCong.meet_inter cgr itv in
      let cgr, itv = Bot.bot_to_exn reduction in
      man.set Cgr (Bot.Nb cgr) v |>
      man.set Int (Bot.Nb itv)
    with Bot.Found_BOT ->
      man.bottom

end


let () =
  register_reduction name {
    pool = Pool.[Value (Int, (module I)); Value (Cgr, (module C))];
    eq = (
      let f : type a b. a key -> b key -> (a, b) eq option = fun k1 k2 ->
        match k1, k2 with
        | Int, Int -> Some Eq
        | Cgr, Cgr -> Some Eq
        | _ -> None
      in
      f
    )
  } (module Reduction)
