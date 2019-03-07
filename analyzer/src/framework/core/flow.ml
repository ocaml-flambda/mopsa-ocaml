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

open Lattice.Sig
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
  TokenMap.subset lattice flow1.tmap flow2.tmap

let join (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  { tmap = TokenMap.join lattice flow1.tmap flow2.tmap; ctx = flow2.ctx }

let join_list lattice ?(ctx=Context.empty) l =
  match l with
  | [] -> bottom ctx
  | l -> {
      tmap = TokenMap.join_list lattice (List.map (function {tmap} -> tmap) l);
      ctx  = let last = List.nth l (List.length l - 1) in last.ctx
    }

let meet (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  { tmap = TokenMap.meet lattice flow1.tmap flow2.tmap; ctx = flow2.ctx }

let meet_list lattice ?(ctx=Context.empty) l =
  match l with
  | [] -> bottom ctx
  | l -> {
      tmap = TokenMap.meet_list lattice (List.map (function {tmap} -> tmap) l);
      ctx  = let last = List.nth l (List.length l - 1) in last.ctx
    }

let widen (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  { tmap = TokenMap.widen lattice flow2.ctx.ctx_unit flow1.tmap flow2.tmap; ctx = flow2.ctx }

let print (lattice: 'a lattice) fmt (flow : 'a flow) : unit =
  TokenMap.print lattice fmt flow.tmap

let get (tk: token) (lattice: 'a lattice) (flow: 'a flow) : 'a =
  TokenMap.get tk lattice flow.tmap

let set (tk: token) (a: 'a) (lattice:'a lattice) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.set tk a lattice flow.tmap }

let copy (tk1:token) (tk2:token) (lattice:'a lattice) (flow1:'a flow) (flow2:'a flow) : 'a flow =
  {
    tmap = TokenMap.copy tk1 tk2 lattice flow1.tmap flow2.tmap;
    ctx = flow2.ctx;
  }

let add (tk: token) (a: 'a) (lattice: 'a lattice) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.add tk a lattice flow.tmap }

let remove (tk: token) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.remove tk flow.tmap }

let filter (f: token -> 'a -> bool) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.filter f flow.tmap }

let map (f: token -> 'a -> 'a) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.map f flow.tmap }

let fold (f: 'b -> token -> 'a -> 'b) (init: 'b) (flow: 'a flow) : 'b =
  TokenMap.fold f init flow.tmap

let merge (f: token -> 'a option -> 'a option -> 'a option) (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  {
    tmap = TokenMap.merge f lattice flow1.tmap flow2.tmap;
    ctx = flow2.ctx;
  }

let get_ctx flow = flow.ctx

let set_ctx ctx flow = {flow with ctx}

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
