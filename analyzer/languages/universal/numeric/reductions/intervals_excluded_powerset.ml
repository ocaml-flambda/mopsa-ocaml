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

(** Reduction operator for intervals and excluded powerset *)

open Mopsa
open Sig.Reduction.Value


module Reduction =
struct

  let name = "universal.numeric.reductions.intervals_excluded_powerset"
  let debug fmt = Debug.debug ~channel:name fmt

  module I = Values.Intervals.Integer.Value
  module P = Values.Powersets.Excluded.SimplifiedValue
  module B = ItvUtils.IntItv.B


  (* Reduce an interval and a powerset *)

  let of_interval (a:I.t) : P.t =
    match a with
    | BOT -> P.bottom
    | Nb (B.Finite l, B.Finite h) -> P.of_bounds l h
    | _ -> P.top

  let reduce_excluded_set (notint_set:P.Set.t) (i:I.t) : P.t * I.t =
    match i with
    | BOT -> P.bottom, BOT
    | _   -> NotIn notint_set, i

  let reduce_finite_set (in_set:P.Set.t) (i:I.t) : P.t * I.t =
    match i with
    | BOT ->
      P.bottom, BOT

    | Nb (B.Finite l, B.Finite h) ->
      let filtered_set = P.In (P.Set.filter (fun a -> Z.geq a l && Z.leq a h) in_set) in
      filtered_set, P.to_itv filtered_set

    | Nb (B.MINF, B.Finite h) ->
      let filtered_set = P.In (P.Set.filter (fun a -> Z.leq a h) in_set) in
      filtered_set, P.to_itv filtered_set

    | Nb (B.Finite l, B.PINF) ->
      let filtered_set = P.In (P.Set.filter (fun a -> Z.geq a l) in_set) in
      filtered_set, P.to_itv filtered_set

    | _ ->
      In in_set, P.to_itv (In in_set)


  let reduce_pair (x:P.t) (i:I.t) : P.t * I.t =
    if P.is_bottom x || I.is_bottom i then P.bottom, I.bottom else
    match x, i with
    | NotIn s, _ -> reduce_excluded_set s i
    | In s, _    -> reduce_finite_set s i

  let reduce (man: 'a value_reduction_man) (v: 'a) : 'a =
    let i = man.get I.id v in
    let p = man.get P.id v in
    let p',i' = reduce_pair p i in
    man.set I.id i' v |>
    man.set P.id p'
end


let () =
  register_value_reduction (module Reduction)
