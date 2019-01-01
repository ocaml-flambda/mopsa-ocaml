(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** 
   Abstraction of the table of file descriptors. Each allocated file
   descriptor is mapped to an interval of possible numeric values. 
*)


open Mopsa
open Universal.Ast
module Itv = Universal.Numeric.Values.Intervals.Value

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

let join annot a1 a2 = {
  map = AddrItvMap.join annot a1.map a2.map;
  support = AddrSet.inter a1.support a2.support;
}

let meet annot a1 a2 = {
  map = AddrItvMap.meet annot a1.map a2.map;
  support = AddrSet.union a1.support a2.support;
}

let widen annot a1 a2 = {
  map = AddrItvMap.widen annot a1.map a2.map;
  support = AddrSet.inter a1.support a2.support;
}

let print fmt a =
  Format.fprintf fmt "others: @[%a@]@\nsupport: @[%a@]"
    AddrItvMap.print a.map
    AddrSet.print a.support

let add addr itv a = {
  map = AddrItvMap.add addr itv a.map;
  support = AddrSet.add addr a.support;
}

(** Insert an address in the file table *)
let insert addr window (t:table) =
  (* Compute the interval of allocated ids *)
  let allocated = AddrItvMap.fold (fun _ -> Itv.join ()) t.map Itv.bottom in

  (* A sound solution is [window, b + 1], where [a, b] = allocated *)
  let l = Z.of_int window
  and u = Itv.bounds allocated |> snd
  in

  let sol0 = Itv.of_z l u in

  (* We can refine this solution by removing the ids of the minimal support *)
  let m = AddrItvMap.filter (fun addr _ -> AddrSet.mem addr t.support) t.map in
  let support = AddrItvMap.fold (fun _ itv acc -> itv :: acc) m [] in
  (* Sort intervals using the lowest bound *)
  let sorted = List.sort (fun itv1 itv2 ->
      let a1 = Itv.bounds itv2 |> fst in
      let a2 = Itv.bounds itv2 |> fst in
      Z.compare a1 a2
    ) support
  in
  let sol = List.fold_left (fun itv acc ->
      Itv.binop () O_minus acc itv |>
      Channel.without_channel
    ) sol0 sorted
  in
  add addr sol t, sol

let find addr (t:table) =
  AddrItvMap.find addr t.map

let remove addr t = {
  map = AddrItvMap.remove addr t.map;
  support = AddrSet.remove addr t.support;
}

let fold f t x =
  AddrItvMap.fold f t.map x
