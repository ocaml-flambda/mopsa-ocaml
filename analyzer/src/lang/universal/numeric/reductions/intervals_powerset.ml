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

(** Reduction operator for intervals and powersets. *)

open Mopsa
open Core.Sig.Value.Reduction



module Reduction =
struct

  let name = "universal.numeric.reductions.intervals_powerset"
  let debug fmt = Debug.debug ~channel:name fmt

  module I = Values.Intervals.Integer.Value
  module P = Values.Powerset.Value


  (* Reduce an interval and a powerset *)
  let meet_itv_pwr i p =
    if I.is_bottom i || P.is_bottom p then
      I.bottom, P.bottom
    else
      (* Reduce powersets with intervals if [i] is a singleton value *)
      match I.bounds_opt i with
      | Some a, Some b when Z.(a = b) ->
        i, P.singleton a
      | _ ->
        (* Reduce intervals with powerset if the interval of [p] is more precise *)
        match p with
        | Top.TOP -> i,p
        | Top.Nt set ->
          let a = P.Set.min_elt set in
          let b = P.Set.max_elt set in
          let i' = I.meet i (I.of_z a b) in
          i',p

  (* Reduction operator *)
  let reduce (man: 'a vrman) (v: 'a) : 'a =
    let i = man.get I.id v
    and p = man.get P.id v in

    let i',p' = meet_itv_pwr i p in

    man.set I.id i' v |>
    man.set P.id p'
end


let () =
  register_reduction (module Reduction)
