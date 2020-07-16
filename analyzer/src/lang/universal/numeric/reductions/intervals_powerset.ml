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
open Framework.Abstraction.Sig.Reduction.Value



module Reduction =
struct

  let name = "universal.numeric.reductions.intervals_powerset"
  let debug fmt = Debug.debug ~channel:name fmt

  module I = Values.Intervals.Integer.Value
  module P = Values.Powerset.Value
  module B = ItvUtils.IntItv.B


  (* Reduce an interval and a powerset *)

  let of_interval (a:I.t) : P.t =
    match a with
    | BOT -> P.bottom
    | Nb (B.Finite l, B.Finite h) -> P.of_bounds l h
    | _ -> P.top

  let to_interval (x:P.t) : I.t =
    match x with
    | TOP -> I.top
    | Nt s ->
      if P.Set.is_empty s then BOT
      else Nb (B.Finite (P.Set.min_elt s), B.Finite (P.Set.max_elt s))

  let reduce_pair (x:P.t) (i:I.t) : P.t * I.t =
    if P.is_top x then of_interval i, i
    else match i with
      | BOT ->
        P.bottom, BOT
      | Nb (B.Finite l, B.Finite h) ->
        let xx = P.Powerset.filter (fun a -> a >= l && a <= h) x in
        xx, to_interval xx
      | Nb (B.MINF, B.Finite h) ->
        let xx = P.Powerset.filter (fun a -> a <= h) x in
        xx, to_interval xx
      | Nb (B.Finite l, B.PINF) ->
        let xx = P.Powerset.filter (fun a -> a >= l) x in
        xx, to_interval xx
      | _ ->
        x, to_interval x


  let reduce (man: 'a value_reduction_man) (v: 'a) : 'a =
    let i = man.get I.id v
    and p = man.get P.id v in

    let p',i' = reduce_pair p i in

    man.set I.id i' v |>
    man.set P.id p'
end


let () =
  register_value_reduction (module Reduction)
