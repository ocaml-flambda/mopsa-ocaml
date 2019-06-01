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

(** Abstraction of control flows *)

open Lattice
open Context
open Token


(****************************************************************************)
(**                             {2 Flows}                                   *)
(****************************************************************************)

type 'a flow = {
  tmap : 'a TokenMap.t;
  ctx : 'a ctx;
}
(** A flow is a flow map augmented with an context *)


let bottom ctx : 'a flow = {
  tmap = TokenMap.bottom;
  ctx;
}

let top ctx : 'a flow = {
  tmap = TokenMap.top;
  ctx;
}

let singleton (ctx:'a Context.ctx) (tk:token) (env:'a) : 'a flow = {
  tmap = TokenMap.singleton tk env;
  ctx
}

let is_bottom (lattice: 'a lattice) (flow: 'a flow) : bool =
  TokenMap.is_bottom lattice flow.tmap

let is_top (lattice: 'a lattice) (flow: 'a flow) : bool =
  TokenMap.is_top lattice flow.tmap

let subset (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : bool =
  TokenMap.subset lattice (Context.get_unit flow2.ctx) flow1.tmap flow2.tmap

let join (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
  { tmap = TokenMap.join lattice (Context.get_unit ctx) flow1.tmap flow2.tmap; ctx }

let join_list lattice ~ctx l =
  match l with
  | [] -> bottom ctx
  | [f] -> f
  | hd :: tl ->
    let ctx  = List.fold_left (fun acc f ->
        Context.get_most_recent acc f.ctx
      ) hd.ctx tl
    in

    {
      tmap = TokenMap.join_list lattice (Context.get_unit ctx) (List.map (function {tmap} -> tmap) l);
      ctx;
    }

let meet (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
  { tmap = TokenMap.meet lattice (Context.get_unit ctx) flow1.tmap flow2.tmap; ctx }

let meet_list lattice ~ctx l =
  match l with
  | [] -> bottom ctx
  | [f] -> f
  | hd :: tl ->
    let ctx  = List.fold_left (fun acc f ->
        Context.get_most_recent acc f.ctx
      ) hd.ctx tl
    in
    {
      tmap = TokenMap.meet_list lattice (Context.get_unit ctx) (List.map (function {tmap} -> tmap) l);
      ctx;
    }

let widen (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
    let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
    { tmap = TokenMap.widen lattice (Context.get_unit ctx) flow1.tmap flow2.tmap; ctx }

let print (lattice: 'a lattice) fmt (flow : 'a flow) : unit =
  TokenMap.print lattice fmt flow.tmap
  ;
  Context.print lattice.print fmt flow.ctx

let print_w_lprint (lprint: Format.formatter -> 'a -> unit) fmt flow =
  TokenMap.print_w_lprint lprint fmt flow.tmap


let get (tk: token) (lattice: 'a lattice) (flow: 'a flow) : 'a =
  TokenMap.get tk lattice flow.tmap

let set (tk: token) (a: 'a) (lattice:'a lattice) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.set tk a lattice flow.tmap }

let copy (tk1:token) (tk2:token) (lattice:'a lattice) (flow1:'a flow) (flow2:'a flow) : 'a flow =
  let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.copy tk1 tk2 lattice flow1.tmap flow2.tmap;
    ctx;
  }

let add (tk: token) (a: 'a) (lattice: 'a lattice) (flow: 'a flow) : 'a flow =
  let a' = get tk lattice flow in
  let aa = lattice.join (Context.get_unit flow.ctx) a a' in
  set tk aa lattice flow

let remove (tk: token) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.remove tk flow.tmap }

let filter (f: token -> 'a -> bool) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.filter f flow.tmap }

let map (f: token -> 'a -> 'a) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.map f flow.tmap }

let fold (f: 'b -> token -> 'a -> 'b) (init: 'b) (flow: 'a flow) : 'b =
  TokenMap.fold f init flow.tmap

let merge (f: token -> 'a option -> 'a option -> 'a option) (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.merge f lattice flow1.tmap flow2.tmap;
    ctx;
  }

let get_ctx flow = flow.ctx

let set_ctx ctx flow = {flow with ctx}

let get_unit_ctx flow = Context.get_unit flow.ctx

let set_unit_ctx ctx flow = { flow with ctx = Context.set_unit ctx flow.ctx }

let map_ctx f flow = set_ctx (f @@ get_ctx flow) flow

let copy_ctx flow1 flow2 =
  let ctx = get_ctx flow1 in
  set_ctx ctx flow2

let get_token_map flow = flow.tmap

let create ctx tmap = {
  tmap;
  ctx;
}

let map_flow (f:token -> 'a -> 'b) (ctx:'b ctx) (flow:'a flow) : 'b flow =
  {
    tmap = TokenMap.map f flow.tmap;
    ctx;
  }

let map_list (f:'b -> 'a flow -> 'a flow) (l: 'b list) (flow: 'a flow) : 'a flow list =
  let flows, ctx = List.fold_left (fun (acc, ctx) x ->
      let flow' = { flow with ctx } in
      let flow'' = f x flow' in
      flow'' :: acc, flow''.ctx
    ) ([], flow.ctx) l
  in
  List.map (set_ctx ctx) flows

let map_list_opt (f:'b -> 'a flow -> 'a flow option) (l:'b list) (flow:'a flow) : 'a flow list =
  let flows, _ = List.fold_left (fun (acc, ctx) x ->
      let flow' = { flow with ctx } in
      match f x flow' with
      | None -> acc, ctx
      | Some flow'' -> flow'' :: acc, flow''.ctx
    ) ([], flow.ctx) l
  in
  flows
