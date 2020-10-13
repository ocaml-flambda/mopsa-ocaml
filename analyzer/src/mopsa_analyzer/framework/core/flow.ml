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

open Mopsa_utils
open Lattice
open Context
open Token
open Alarm
open Callstack


(****************************************************************************)
(**                             {2 Flows}                                   *)
(****************************************************************************)

type 'a flow = {
  tmap : 'a TokenMap.t;
  ctx : 'a ctx;
  report: report;
}
(** A flow is a flow map augmented with an context *)


let bottom ctx report : 'a flow = {
  tmap = TokenMap.bottom;
  ctx;
  report;
}


let top ctx : 'a flow = {
  tmap = TokenMap.top;
  ctx;
  report = empty_report;
}

let singleton (ctx:'a Context.ctx) (tk:token) (env:'a) : 'a flow = {
  tmap = TokenMap.singleton tk env;
  ctx;
  report = empty_report;
}

let is_bottom (lattice: 'a lattice) (flow: 'a flow) : bool =
  TokenMap.is_bottom lattice flow.tmap

let is_top (lattice: 'a lattice) (flow: 'a flow) : bool =
  TokenMap.is_top lattice flow.tmap

let is_empty (flow:'a flow) : bool =
  TokenMap.is_empty flow.tmap && is_empty_report flow.report

let subset (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : bool =
  let ctx = most_recent_ctx flow1.ctx flow2.ctx in
  TokenMap.subset lattice ctx flow1.tmap flow2.tmap

let join (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = most_recent_ctx flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.join lattice ctx flow1.tmap flow2.tmap;
    ctx;
    report = join_report flow1.report flow2.report;
  }

let join_list lattice ~empty l =
  match l with
  | [] -> empty ()
  | [f] -> f
  | hd :: tl ->
    let ctx  = List.fold_left (fun acc f ->
        most_recent_ctx acc f.ctx
      ) hd.ctx tl
    in
    {
      tmap = TokenMap.join_list lattice ctx (List.map (function {tmap} -> tmap) l);
      ctx;
      report = List.fold_left (fun acc {report} -> join_report report acc) hd.report tl;
    }

let meet (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = most_recent_ctx flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.meet lattice ctx flow1.tmap flow2.tmap;
    ctx;
    report = meet_report flow1.report flow2.report;
  }

let meet_list lattice ~empty l =
  match l with
  | [] -> empty
  | [f] -> f
  | hd :: tl ->
    let ctx  = List.fold_left (fun acc f ->
        most_recent_ctx acc f.ctx
      ) hd.ctx tl
    in
    {
      tmap = TokenMap.meet_list lattice ctx (List.map (function {tmap} -> tmap) l);
      ctx;
      report = List.fold_left (fun acc {report} -> meet_report report acc) hd.report tl;
    }

let widen (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
    let ctx = most_recent_ctx flow1.ctx flow2.ctx in
    {
      tmap = TokenMap.widen lattice ctx flow1.tmap flow2.tmap;
      ctx;
      report = join_report flow1.report flow2.report;
    }


let get_ctx flow = flow.ctx

let set_ctx ctx flow =
  if ctx == flow.ctx then flow else {flow with ctx}

let map_ctx f flow = set_ctx (f @@ get_ctx flow) flow

let copy_ctx flow1 flow2 =
  let ctx = get_ctx flow1 in
  set_ctx ctx flow2

let get_report flow = flow.report

let set_report report flow = if report == flow.report then flow else { flow with report }

let copy_report src dst = set_report (join_report src.report dst.report) dst

let remove_report flow = { flow with report = empty_report }

let create ctx report tmap = {
  tmap;
  ctx;
  report;
}

let get_callstack flow =
  get_ctx flow |>
  find_ctx callstack_ctx_key


let set_callstack cs flow =
  set_ctx (
    get_ctx flow |>
    add_ctx Context.callstack_ctx_key cs
  ) flow

let push_callstack fname ?(uniq=fname) range flow =
  set_callstack (
    get_callstack flow |>
    push_callstack fname ~uniq range
  ) flow

let pop_callstack flow =
  let hd, cs = get_callstack flow |>
               pop_callstack
  in
  hd, set_callstack cs flow

let bottom_from flow : 'a flow =
  bottom (get_ctx flow) (get_report flow)


let print (pp: Print.printer -> 'a -> unit) printer flow =
  TokenMap.print pp printer flow.tmap

let get (tk: token) (lattice: 'a lattice) (flow: 'a flow) : 'a =
  TokenMap.get tk lattice flow.tmap

let set (tk: token) (a: 'a) (lattice:'a lattice) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.set tk a lattice flow.tmap }

let set_bottom tk flow =
  { flow with tmap = TokenMap.remove tk flow.tmap }

let copy (tk1:token) (tk2:token) (lattice:'a lattice) (flow1:'a flow) (flow2:'a flow) : 'a flow =
  let ctx = most_recent_ctx flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.copy tk1 tk2 lattice flow1.tmap flow2.tmap;
    ctx;
    report = flow2.report;
  }

let add (tk: token) (a: 'a) (lattice: 'a lattice) (flow: 'a flow) : 'a flow =
  let a' = get tk lattice flow in
  let aa = lattice.join flow.ctx a a' in
  set tk aa lattice flow

let remove (tk: token) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.remove tk flow.tmap }

let rename (tki: token) (tko: token) (lattice: 'a lattice) (flow: 'a flow) : 'a flow =
  let ai = get tki lattice flow in
  remove tki flow |>
  add tko ai lattice

let mem (tk:token) (flow:'a flow) = TokenMap.mem tk flow.tmap

let filter (f: token -> 'a -> bool) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.filter f flow.tmap }

let partition (f: token -> 'a -> bool) (flow: 'a flow) : 'a flow * 'a flow =
  let l, r = TokenMap.partition f flow.tmap in
  { flow with tmap = l }, { flow with tmap = r }


let map (f: token -> 'a -> 'a) (flow: 'a flow) : 'a flow =
  { flow with tmap = TokenMap.map f flow.tmap }

let fold (f: 'b -> token -> 'a -> 'b) (init: 'b) (flow: 'a flow) : 'b =
  TokenMap.fold f init flow.tmap

let map2zo
    (f1: token -> 'a -> 'a)
    (f2: token -> 'a -> 'a)
    (f: token -> 'a -> 'a -> 'a)
    (falarm: report -> report -> report)
    (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let ctx = most_recent_ctx flow1.ctx flow2.ctx in
  {
    tmap = TokenMap.map2zo f1 f2 f flow1.tmap flow2.tmap;
    ctx;
    report = falarm flow1.report flow2.report;
  }

let merge lattice ~merge_report pre (flow1,effect1) (flow2,effect2) =
  let ctx = most_recent_ctx (get_ctx flow1) (get_ctx flow2) in
  map2zo
    (fun _ a1 -> lattice.bottom)
    (fun _ a2 -> lattice.bottom)
    (fun tk a1 a2 ->
       match tk with
       (* Effects concern only cur environments *)
       | T_cur ->
         (* Merge the cur environments *)
         let p = get T_cur lattice pre in
         lattice.merge p (a1,effect1) (a2,effect2)

       (* For the other tokens, compute the meet of the environments *)
       | _ -> lattice.meet ctx a1 a2
    ) merge_report flow1 flow2

let get_token_map flow = flow.tmap

let add_alarm ?(force=false) ?(warning=false) alarm lattice flow =
  let cur = get T_cur lattice flow in
  if not force
  && lattice.is_bottom cur
  then { flow with report = Alarm.add_diagnostic (Alarm.mk_unreachable_diagnostic (check_of_alarm alarm) alarm.alarm_callstack alarm.alarm_range) flow.report }
  else { flow with report = Alarm.add_alarm ~warning alarm flow.report }

let raise_alarm ?(force=false) ?(bottom=false) ?(warning=false) alarm lattice flow =
  let flow = add_alarm ~force ~warning alarm lattice flow in
  if not bottom
  then flow
  else set T_cur lattice.bottom lattice flow

let add_diagnostic diag flow =
  let report = Alarm.add_diagnostic diag flow.report in
  if report == flow.report then flow else { flow with report }

let add_safe_check check range flow =
  let cs = get_callstack flow in
  add_diagnostic (Alarm.mk_safe_diagnostic check cs range) flow

let add_unreachable_check check range flow =
  let cs = get_callstack flow in
  add_diagnostic (Alarm.mk_unreachable_diagnostic check cs range) flow

let add_warning_check check range flow =
  let cs = get_callstack flow in
  add_diagnostic (Alarm.mk_warning_diagnostic check cs range) flow

let add_assumption assumption flow =
  set_report (Alarm.add_assumption assumption flow.report) flow

let add_global_assumption assumption_kind flow =
  set_report (Alarm.add_global_assumption assumption_kind flow.report) flow

let add_local_assumption assumption_kind range flow =
  set_report (Alarm.add_local_assumption assumption_kind range flow.report) flow
