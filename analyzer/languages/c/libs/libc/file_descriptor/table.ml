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

(**
   Abstraction of the table of file descriptors. Each allocated file
   descriptor is mapped to an interval of possible numeric values.
*)


open Mopsa
open Universal.Ast
module Itv = Universal.Numeric.Values.Intervals.Integer.Value


(** Map from addresses to intervals *)
module AddrItvMap = Framework.Lattices.Partial_map.Make(Addr)(Itv)

type table = {
  map: AddrItvMap.t; (* map from addresses to intervals *)
  support: AddrSet.t; (* under-approximation of the support of the map *)
}

let empty : table = {
  map = AddrItvMap.empty;
  support = AddrSet.empty;
}

let bottom : table = {
  map = AddrItvMap.bottom;
  support = AddrSet.empty;
}

let top : table = {
  map = AddrItvMap.top;
  support = AddrSet.empty;
}

let is_bottom (t:table) = false (* FIXME: replace by Table.is_bottom after
                                   fixing bottom value of partial maps *)

let subset a1 a2 =
  AddrItvMap.subset a1.map a2.map &&
  AddrSet.subset a2.support a1.support

let join a1 a2 = {
  map = AddrItvMap.join a1.map a2.map;
  support = AddrSet.inter a1.support a2.support;
}

let meet a1 a2 = {
  map = AddrItvMap.meet a1.map a2.map;
  support = AddrSet.union a1.support a2.support;
}

let widen ctx a1 a2 = {
  map = AddrItvMap.widen ctx a1.map a2.map;
  support = AddrSet.inter a1.support a2.support;
}

let print printer a =
  pp_obj_map printer
    [ (pbox pp_string "others", pbox AddrItvMap.print a.map);
      (pbox pp_string "support", pbox AddrSet.print a.support) ]

let add addr itv a = {
  map = AddrItvMap.add addr itv a.map;
  support = AddrSet.add addr a.support;
}

(** Insert an address in the file table *)
let insert addr window (t:table) flow =
  (* Compute the interval of allocated ids *)
  let allocated = AddrItvMap.fold (fun _ -> Itv.join) t.map Itv.bottom in

  if Itv.is_bottom allocated then
    let itv = Itv.of_int window window in
    add addr itv t, itv
  else
    (* A sound solution is [window, b + 1], where [a, b] = allocated *)
    let l = Z.of_int window in
    let u =
      match Itv.bounds_opt allocated with
      | _, Some b -> Z.succ b
      | _, None   -> Ast.(rangeof s32 flow) |> snd
                  (* When allocated is not upper-bounded *)
    in

    let itv0 = Itv.of_z l u in

    (* We can refine this solution by removing the ids of the minimal support *)
    let minmap = AddrItvMap.filter (fun addr _ -> AddrSet.mem addr t.support) t.map in
    let minitv = AddrItvMap.fold (fun _ itv acc -> itv :: acc) minmap [] in
    (* Sort intervals using the lowest bound *)
    let sorted = List.sort (fun itv1 itv2 ->
        let a1 = Itv.bounds itv2 |> fst in
        let a2 = Itv.bounds itv2 |> fst in
        Z.compare a1 a2
      ) minitv
    in
    let itv = List.fold_left (fun acc itv ->
        Bot.bot_absorb2 Itv.I.filter_neq acc itv |>
        Bot.bot_lift1 fst
      ) itv0 sorted
    in
    add addr itv t, itv


let insert_at addr itv t =
  let itv =
    if AddrItvMap.mem addr t.map
    then Itv.join itv (AddrItvMap.find addr t.map)
    else itv
  in
  {
    map = AddrItvMap.add addr itv t.map;
    support = AddrSet.add addr t.support;
  }


let filter f (t:table) : table =
  let map = AddrItvMap.filter f t.map in
  let support = AddrSet.filter (fun addr -> AddrItvMap.mem addr map) t.support in
  { map; support }

let pool t =
  AddrItvMap.bindings t.map |>
  List.split |>
  fst

let remove addr t = {
  map = AddrItvMap.remove addr t.map;
  support = AddrSet.remove addr t.support;
}

let fold f t x =
  AddrItvMap.fold f t.map x

let for_all f t =
  AddrItvMap.for_all f t.map
