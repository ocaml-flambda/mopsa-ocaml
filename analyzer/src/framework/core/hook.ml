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
  val on_after_exec : zone -> stmt -> ('a,'a) man -> 'a flow -> 'a post -> 'a ctx
  val on_before_eval : zone*zone -> expr -> ('a,'a) man -> 'a flow -> 'a ctx
  val on_after_eval : zone*zone -> expr -> ('a,'a) man -> 'a flow -> 'a eval -> 'a ctx
  val on_finish : ('a,'a) man -> 'a flow -> unit
end


module type STATELESS_HOOK =
sig
  val name : string
  val exec_zones : zone list
  val eval_zones: (zone * zone) list
  val init : 'a ctx -> unit
  val on_before_exec : zone -> stmt -> ('a,'a) man  -> 'a flow -> unit
  val on_after_exec : zone -> stmt -> ('a,'a) man -> 'a flow -> 'a post -> unit
  val on_before_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> unit
  val on_after_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> 'a eval -> unit
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

  let on_after_exec zone stmt man flow post =
    Hook.on_after_exec zone stmt man flow post;
    Post.get_ctx post

  let on_before_eval zone stmt man flow =
    Hook.on_before_eval zone stmt man flow;
    Flow.get_ctx flow

  let on_after_eval zone stmt man flow eval =
    Hook.on_after_eval zone stmt man flow eval;
    Eval.get_ctx eval

  let on_finish = Hook.on_finish

end


(** Registered hooks *)
let hooks : (string,(module HOOK)) Hashtbl.t = Hashtbl.create 16

(** Active hooks *)
let active_hooks : (string,(module HOOK)) Hashtbl.t = Hashtbl.create 16

(** Initialized hooks *)
let initialized_hooks : (string,(module HOOK)) Hashtbl.t = Hashtbl.create 16


(** Register a new hook *)
let register_hook hook =
  let module H = (val hook : HOOK) in
  Hashtbl.add hooks H.name hook


(** Register a new stateless hook *)
let register_stateless_hook hook =
  let module H = (val hook : STATELESS_HOOK) in
  Hashtbl.add hooks H.name (module MakeStatefulHook(H))

(** Check whether a hook exists *)
let mem_hook name : bool =
  Hashtbl.mem hooks name

(** Find a hook by name *)
let find_hook (name:string) : (module HOOK) =
  Hashtbl.find hooks name

let list_hooks () : (module HOOK) list =
  Hashtbl.fold (fun _ h l -> h::l) hooks []

(** Set of hooks *)
module HookSet = SetExt.Make
    (struct
      type t = (module HOOK)
      let compare (h1:(module HOOK)) (h2:(module HOOK)) =
        let module H1 = (val h1) in
        let module H2 = (val h2) in
        compare H1.name H2.name
    end)

(** Caches of hooks, indexed by used zones *)
module ExecCache = MapExt.Make(struct type t = zone let compare = compare_zone end)
module EvalCache = MapExt.Make(struct type t = zone*zone let compare = compare_zone2 end)

type cache = {
  mutable exec : HookSet.t ExecCache.t;
  mutable eval : HookSet.t EvalCache.t;
}

let cache = {
  exec = ExecCache.empty;
  eval = EvalCache.empty;
}


(** Initialize internals *)
let init interface =
  cache.exec <- List.fold_left (fun acc zone ->
      ExecCache.add zone HookSet.empty acc
    ) ExecCache.empty interface.iexec.uses;
  cache.exec <- ExecCache.add Z_any HookSet.empty cache.exec;

  cache.eval <- List.fold_left (fun acc zone ->
      EvalCache.add zone HookSet.empty acc
    ) EvalCache.empty interface.ieval.uses;
  cache.eval <- EvalCache.add (Z_any,Z_any) HookSet.empty cache.eval


(** Initialize an active hook *)
let init_hook hook ctx =
  if Hashtbl.mem initialized_hooks hook then ctx else
  if not (Hashtbl.mem active_hooks hook) then Exceptions.panic "Inactive hook %s cannot be initialized" hook
  else
    let h = find_hook hook in
    let module H = (val h : HOOK) in
    let () = Hashtbl.add initialized_hooks hook (module H) in

    cache.exec <- ExecCache.fold (fun zone hooks acc ->
        if List.exists (fun z -> sat_zone z zone) H.exec_zones
        then ExecCache.add zone (HookSet.add h hooks) acc
        else acc
      ) cache.exec cache.exec;

    cache.eval <- EvalCache.fold (fun zone hooks acc ->
        if List.exists (fun z -> sat_zone2 z zone) H.eval_zones
        then EvalCache.add zone (HookSet.add h hooks) acc
        else acc
      ) cache.eval cache.eval;

    H.init ctx


(** Initialize all active hooks *)
let init_active_hooks ctx =
  Hashtbl.fold (fun name hook ctx ->
      init_hook name ctx
    ) active_hooks ctx


(** Activate a registered hook *)
let activate_hook name =
  let module H = (val find_hook name) in
  Hashtbl.add active_hooks name (module H)


(** Deactivate an active hook *)
let deactivate_hook name man flow =
  if not (Hashtbl.mem active_hooks name) then ()
  else
    let h = Hashtbl.find active_hooks name in
    let module H = (val h : HOOK) in
    H.on_finish man flow;
    Hashtbl.remove active_hooks name;
    Hashtbl.remove initialized_hooks name;
    cache.exec <- ExecCache.map (HookSet.remove h) cache.exec;
    cache.eval <- EvalCache.map (HookSet.remove h) cache.eval


let find_exec_hooks zone cache =
  try ExecCache.find zone cache.exec
  with Not_found -> HookSet.empty

let find_eval_hooks zone cache =
  try EvalCache.find zone cache.eval
  with Not_found -> HookSet.empty


(** Fire [on_before_exec] event *)
let on_before_exec zone stmt man flow =
  let hooks = find_exec_hooks zone cache in
  HookSet.fold (fun hook ctx ->
      let flow = Flow.set_ctx ctx flow in
      let module H = (val hook : HOOK) in
      H.on_before_exec zone stmt man flow
    ) hooks (Flow.get_ctx flow)



(** Fire [on_after_exec] event *)
let on_after_exec zone stmt man flow post =
  let hooks = find_exec_hooks zone cache in
  HookSet.fold (fun hook ctx ->
      let post = Post.set_ctx ctx post in
      let module H = (val hook : HOOK) in
      H.on_after_exec zone stmt man flow post
    ) hooks (Post.get_ctx post)


(** Fire [on_before_eval] event *)
let on_before_eval zone exp man flow =
  let hooks = find_eval_hooks zone cache in
  HookSet.fold (fun hook ctx ->
      let flow = Flow.set_ctx ctx flow in
      let module H = (val hook : HOOK) in
      H.on_before_eval zone exp man flow
    ) hooks (Flow.get_ctx flow)



(** Fire [on_after_eval] event *)
let on_after_eval zone exp man flow eval =
  let hooks = find_eval_hooks zone cache in
  HookSet.fold (fun hook ctx ->
      let eval = Eval.set_ctx ctx eval in
      let module H = (val hook : HOOK) in
      H.on_after_eval zone exp man flow eval
    ) hooks (Eval.get_ctx eval)


let on_finish man flow =
  Hashtbl.iter (fun name hook ->
      deactivate_hook name man flow
    ) active_hooks
