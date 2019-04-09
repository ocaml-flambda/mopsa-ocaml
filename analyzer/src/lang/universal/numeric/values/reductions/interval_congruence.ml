(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for intervals and congruences. *)

open Mopsa
open Core.Sig.Reduction.Value


(***************************************************************************)
(*                             First reduction                             *)
(***************************************************************************)

let name = "universal.numeric.values.reductions.interval_congruence"
let debug fmt = Debug.debug ~channel:name fmt

module I = Intervals.Integer.Value
module C = Congruences.Value

module Reduction =
struct

  (* Reduce a congruence and an interval *)
  let meet_cgr_itv c i =
    match c, i with
    | Bot.BOT, _ | _, Bot.BOT -> (C.bottom, I.bottom)
    | Bot.Nb a, Bot.Nb b ->
      match CongUtils.IntCong.meet_inter a b with
      | Bot.BOT -> (C.bottom, I.bottom)
      | Bot.Nb (a', b') ->
        let c' = Bot.Nb a' and i' = Bot.Nb b' in
        debug "reduce %a and %a => result: %a and %a"
          I.print i
          C.print c
          I.print i'
          C.print c'
        ;
        (c', i')

  (* Reduction operator *)
  let reduce (man: 'a vrman) (v: 'a) : 'a =
    let c = man.vrget C.id v in
    let i = man.vrget I.id v in

    let c', i' = meet_cgr_itv c i in

    man.vrset I.id i' v |>
    man.vrset C.id c'
end


let () =
  register_reduction name (module Reduction)
