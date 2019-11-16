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
open Alarm


(****************************************************************************)
(**                             {2 Flows}                                   *)
(****************************************************************************)

type 'a flow = {
  tmap : 'a TokenMap.t;
  ctx : 'a ctx;
  alarms: AlarmSet.t;
}
(** A flow is a flow map augmented with an context *)


let bottom ctx alarms : 'a flow = {
  tmap = TokenMap.bottom;
  ctx;
  alarms;
}

let top ctx : 'a flow = {
  tmap = TokenMap.top;
  ctx;
  alarms = AlarmSet.empty;
}

let singleton (ctx:'a Context.ctx) (tk:token) (env:'a) : 'a flow = {
  tmap = TokenMap.singleton tk env;
  ctx;
  alarms = AlarmSet.empty;
}

let is_bottom (lattice: 'a lattice) (flow: 'a flow) : bool =
  TokenMap.is_bottom lattice flow.tmap

let is_top (lattice: 'a lattice) (flow: 'a flow) : bool =
  TokenMap.is_top lattice flow.tmap

let subset (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : bool =
  TokenMap.subset lattice (Context.get_unit flow2.ctx) flow1.tmap flow2.tmap

let join (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.join lattice (Context.get_unit ctx) flow1.tmap flow2.tmap;
    ctx;
    alarms = AlarmSet.union flow1.alarms flow2.alarms;
  }

let join_list lattice ~empty l =
  match l with
  | [] -> empty
  | [f] -> f
  | hd :: tl ->
    let ctx  = List.fold_left (fun acc f ->
        Context.get_most_recent acc f.ctx
      ) hd.ctx tl
    in
    {
      tmap = TokenMap.join_list lattice (Context.get_unit ctx) (List.map (function {tmap} -> tmap) l);
      ctx;
      alarms = List.fold_left (fun acc {alarms} -> AlarmSet.union alarms acc) hd.alarms tl;
    }

let meet (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.meet lattice (Context.get_unit ctx) flow1.tmap flow2.tmap;
    ctx;
    alarms = AlarmSet.inter flow1.alarms flow2.alarms;
  }

let meet_list lattice ~empty l =
  match l with
  | [] -> empty
  | [f] -> f
  | hd :: tl ->
    let ctx  = List.fold_left (fun acc f ->
        Context.get_most_recent acc f.ctx
      ) hd.ctx tl
    in
    {
      tmap = TokenMap.meet_list lattice (Context.get_unit ctx) (List.map (function {tmap} -> tmap) l);
      ctx;
      alarms = List.fold_left (fun acc {alarms} -> AlarmSet.inter alarms acc) hd.alarms tl;
    }

let widen (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
    let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
    {
      tmap = TokenMap.widen lattice (Context.get_unit ctx) flow1.tmap flow2.tmap;
      ctx;
      alarms = AlarmSet.union flow1.alarms flow2.alarms;
    }


let print (pp: Format.formatter -> 'a -> unit) fmt flow =
  TokenMap.print pp fmt flow.tmap


let get (tk: token) (lattice: 'a lattice) (flow: 'a flow) : 'a =
  TokenMap.get tk lattice flow.tmap

let set (tk: token) (a: 'a) (lattice:'a lattice) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.set tk a lattice flow.tmap }

let set_bottom tk flow =
  { flow with tmap = TokenMap.remove tk flow.tmap }

let copy (tk1:token) (tk2:token) (lattice:'a lattice) (flow1:'a flow) (flow2:'a flow) : 'a flow =
  let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.copy tk1 tk2 lattice flow1.tmap flow2.tmap;
    ctx;
    alarms = flow2.alarms;
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

let merge
    (ftk: token -> 'a option -> 'a option -> 'a option)
    (falarm: AlarmSet.t -> AlarmSet.t -> AlarmSet.t)
    (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = Context.get_most_recent flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.merge ftk lattice flow1.tmap flow2.tmap;
    ctx;
    alarms = falarm flow1.alarms flow2.alarms;
  }

let get_ctx flow = flow.ctx

let set_ctx ctx flow =
  if ctx == flow.ctx then flow else {flow with ctx}

let get_unit_ctx flow = Context.get_unit flow.ctx

let set_unit_ctx ctx flow = { flow with ctx = Context.set_unit ctx flow.ctx }

let map_ctx f flow = set_ctx (f @@ get_ctx flow) flow

let copy_ctx flow1 flow2 =
  let ctx = get_ctx flow1 in
  set_ctx ctx flow2

let get_token_map flow = flow.tmap

let add_alarm ?(force=false) alarm lattice flow =
  if not force &&
     get T_cur lattice flow |> lattice.is_bottom
  then flow
  else { flow with alarms = AlarmSet.add alarm flow.alarms }

let raise_alarm ?(force=false) ?(bottom=false) alarm lattice flow =
  let flow = add_alarm ~force alarm lattice flow in
  if not bottom
  then flow
  else set T_cur lattice.bottom lattice flow

let get_alarms flow = flow.alarms

let set_alarms alarms flow = { flow with alarms }

let remove_alarms flow = { flow with alarms = AlarmSet.empty }

let copy_alarms src dst = { dst with alarms = AlarmSet.union src.alarms dst.alarms }

let create ctx alarms tmap = {
  tmap;
  ctx;
  alarms;
}

let get_callstack flow =
  get_ctx flow |>
  Context.find_unit Callstack.ctx_key


let set_callstack cs flow =
  set_ctx (
    get_ctx flow |>
    Context.add_unit Callstack.ctx_key cs
  ) flow

let push_callstack fname range flow =
  set_callstack (
    get_callstack flow |>
    Callstack.push fname range
  ) flow

let pop_callstack flow =
  let hd, cs = get_callstack flow |>
               Callstack.pop
  in
  hd, set_callstack cs flow
