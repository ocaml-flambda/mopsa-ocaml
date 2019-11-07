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

(** Hooks are modules that can observe the execution of the transfer
    functions without modifying their output. They can be used however to enrich the
    analysis by adding information to the context.
*)

open Ast.Stmt
open Ast.Expr
open Lattice
open Flow
open Context
open Post
open Eval
open Zone
open Interface
open Sig.Domain.Manager


module type HOOK =
sig
  val name : string
  val exec_zones : zone list
  val eval_zones: (zone * zone) list
  val init : 'a ctx -> 'a ctx
  val on_before_exec : zone -> stmt -> ('a,'a) man -> 'a flow -> 'a ctx
  val on_after_exec : zone -> stmt -> ('a,'a) man -> 'a post -> 'a ctx
  val on_before_eval : zone*zone -> expr -> ('a,'a) man -> 'a flow -> 'a ctx
  val on_after_eval : zone*zone -> expr -> ('a,'a) man -> 'a eval -> 'a ctx
  val on_finish : ('a,'a) man -> 'a flow -> unit
end


module type STATELESS_HOOK =
sig
  val name : string
  val exec_zones : zone list
  val eval_zones: (zone * zone) list
  val init : 'a ctx -> unit
  val on_before_exec : zone -> stmt -> ('a,'a) man  -> 'a flow -> unit
  val on_after_exec : zone -> stmt -> ('a,'a) man -> 'a post -> unit
  val on_before_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> unit
  val on_after_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a eval -> unit
  val on_finish : ('a,'a) man -> 'a flow -> unit
end

module MakeStatefulHook(Hook:STATELESS_HOOK) : HOOK =
struct
  let name = Hook.name
  let exec_zones = Hook.exec_zones
  let eval_zones = Hook.eval_zones

  let init ctx =
    Hook.init ctx;
    ctx

  let on_before_exec zone stmt man flow =
    Hook.on_before_exec zone stmt man flow;
    Flow.get_ctx flow

  let on_after_exec zone stmt man post =
    Hook.on_after_exec zone stmt man post;
    Post.get_ctx post

  let on_before_eval zone stmt man flow =
    Hook.on_before_eval zone stmt man flow;
    Flow.get_ctx flow

  let on_after_eval zone stmt man eval =
    Hook.on_after_eval zone stmt man eval;
    Eval.get_ctx eval

  let on_finish = Hook.on_finish

end



(** List of registered hoolks *)
let hooks : (module HOOK) list ref = ref []

(** List of active hooks *)
let active_hooks : (module HOOK) list ref = ref []

(** Register a new hook *)
let register_hook hook =
  hooks := hook :: !hooks

(** Register a new stateless hook *)
let register_stateless_hook hook =
  let module H = (val hook : STATELESS_HOOK) in
  hooks := (module MakeStatefulHook(H)) :: !hooks


(** Activate a hook *)
let activate_hook name =
  let rec iter = function
    | [] -> raise Not_found
    | hook :: tl ->
      let module H = (val hook : HOOK) in
      if H.name = name then active_hooks := hook :: !active_hooks else iter tl
  in
  iter !hooks

(** Caches of hooks, indexed by used zones *)
module ExecCache = MapExt.Make(struct type t = zone let compare = compare_zone end)
module EvalCache = MapExt.Make(struct type t = zone*zone let compare = compare_zone2 end)

type cache = {
  mutable exec : (module HOOK) list ExecCache.t;
  mutable eval : (module HOOK) list EvalCache.t;
}

let cache = {
  exec = ExecCache.empty;
  eval = EvalCache.empty;
}

let find_exec_hooks zone cache =
  try ExecCache.find zone cache.exec
  with Not_found -> []

let find_eval_hooks zone cache =
  try EvalCache.find zone cache.eval
  with Not_found -> []


(** Initialization *)
let init_hooks interface ctx =
  (* Initialize all hooks *)
  let ctx = List.fold_left (fun ctx hook ->
      let module H = (val hook : HOOK) in
      H.init ctx
    ) ctx !active_hooks
  in

  (* Build the exec caches *)
  let exec_cache =
    interface.iexec.uses |>
    List.fold_left (fun cache zone ->
        let selected_hooks = List.fold_left (fun acc hook ->
            let module H = (val hook : HOOK) in
            if List.exists (fun z -> sat_zone z zone) H.exec_zones
            then hook :: acc
            else acc
          ) [] !active_hooks
        in
        ExecCache.add zone selected_hooks cache
      ) ExecCache.empty
  in

  (* Add all hooks if the zone is Z_any *)
  let exec_cache = ExecCache.add Z_any !active_hooks exec_cache in

  (* Build the eval caches *)
  let eval_cache =
    interface.ieval.uses |>
    List.fold_left (fun cache zone2 ->
        let selected_hooks = List.fold_left (fun acc hook ->
            let module H = (val hook : HOOK) in
            if List.exists (fun z2 -> sat_zone2 z2 zone2) H.eval_zones
            then hook :: acc
            else acc
          ) [] !active_hooks
        in
        EvalCache.add zone2 selected_hooks cache
      ) EvalCache.empty
  in

  (* Add all hooks if the zone is Z_any*Z_any *)
  let eval_cache = EvalCache.add (Z_any,Z_any) !active_hooks eval_cache in

  cache.exec <- exec_cache;
  cache.eval <- eval_cache;

  ctx



(** Fire [on_before_exec] event *)
let on_before_exec zone stmt man flow =
  let hooks = find_exec_hooks zone cache in
  List.fold_left (fun ctx hook ->
      let flow = Flow.set_ctx ctx flow in
      let module H = (val hook : HOOK) in
      H.on_before_exec zone stmt man flow
    ) (Flow.get_ctx flow) hooks



(** Fire [on_after_exec] event *)
let on_after_exec zone stmt man post =
  let hooks = find_exec_hooks zone cache in
  List.fold_left (fun ctx hook ->
      let post = Post.set_ctx ctx post in
      let module H = (val hook : HOOK) in
      H.on_after_exec zone stmt man post
    ) (Post.get_ctx post) hooks


(** Fire [on_before_eval] event *)
let on_before_eval zone stmt man flow =
  let hooks = find_eval_hooks zone cache in
  List.fold_left (fun ctx hook ->
      let flow = Flow.set_ctx ctx flow in
      let module H = (val hook : HOOK) in
      H.on_before_eval zone stmt man flow
    ) (Flow.get_ctx flow) hooks



(** Fire [on_after_eval] event *)
let on_after_eval zone stmt man eval =
  let hooks = find_eval_hooks zone cache in
  List.fold_left (fun ctx hook ->
      let eval = Eval.set_ctx ctx eval in
      let module H = (val hook : HOOK) in
      H.on_after_eval zone stmt man eval
    ) (Eval.get_ctx eval) hooks


let on_finish man flow =
  List.iter (fun hook ->
      let module H = (val hook : HOOK) in
      H.on_finish man flow
    ) !active_hooks
