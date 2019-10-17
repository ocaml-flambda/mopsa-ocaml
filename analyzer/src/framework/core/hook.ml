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


module type HOOK =
sig
  val name : string

  val exec_zones : zone list

  val eval_zones: (zone * zone) list

  val init : 'a ctx -> 'a ctx

  val before_exec : zone -> stmt -> 'a lattice -> 'a flow -> 'a ctx

  val after_exec : zone -> stmt -> 'a lattice -> 'a post -> 'a ctx

  val before_eval : zone*zone -> expr -> 'a lattice -> 'a flow -> 'a ctx

  val after_eval : zone*zone -> expr -> 'a lattice -> 'a eval -> 'a ctx
end


(** List of registered hoolks *)
let hooks : (module HOOK) list ref = ref []

(** Register a new hook *)
let register_hook hook =
  hooks := hook :: !hooks



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



(** Initialization *)
let init interface ctx =
  (* Initialize all hooks *)
  let ctx = List.fold_left (fun ctx hook ->
      let module H = (val hook : HOOK) in
      H.init ctx
    ) ctx !hooks
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
          ) [] !hooks
        in
        ExecCache.add zone selected_hooks cache
      ) ExecCache.empty
  in

  (* Add the all hooks if the zone is Z_any *)
  let exec_cache = ExecCache.add Z_any !hooks exec_cache in

  (* Build the eval caches *)
  let eval_cache =
    interface.ieval.uses |>
    List.fold_left (fun cache zone2 ->
        let selected_hooks = List.fold_left (fun acc hook ->
            let module H = (val hook : HOOK) in
            if List.exists (fun z2 -> sat_zone2 z2 zone2) H.eval_zones
            then hook :: acc
            else acc
          ) [] !hooks
        in
        EvalCache.add zone2 selected_hooks cache
      ) EvalCache.empty
  in

  (* Add the all hooks if the zone is Z_any*Z_any *)
  let eval_cache = EvalCache.add (Z_any,Z_any) !hooks eval_cache in

  cache.exec <- exec_cache;
  cache.eval <- eval_cache;

  ctx



(** Fire [before_exec] event *)
let before_exec zone stmt lattice flow =
  let hooks = ExecCache.find zone cache.exec in
  List.fold_left (fun ctx hook ->
      let flow = Flow.set_ctx ctx flow in
      let module H = (val hook : HOOK) in
      H.before_exec zone stmt lattice flow
    ) (Flow.get_ctx flow) hooks



(** Fire [after_exec] event *)
let after_exec zone stmt lattice post =
  let hooks = ExecCache.find zone cache.exec in
  List.fold_left (fun ctx hook ->
      let post = Post.set_ctx ctx post in
      let module H = (val hook : HOOK) in
      H.after_exec zone stmt lattice post
    ) (Post.get_ctx post) hooks


(** Fire [before_eval] event *)
let before_eval zone stmt lattice flow =
  let hooks = EvalCache.find zone cache.eval in
  List.fold_left (fun ctx hook ->
      let flow = Flow.set_ctx ctx flow in
      let module H = (val hook : HOOK) in
      H.before_eval zone stmt lattice flow
    ) (Flow.get_ctx flow) hooks



(** Fire [after_eval] event *)
let after_eval zone stmt lattice eval =
  let hooks = EvalCache.find zone cache.eval in
  List.fold_left (fun ctx hook ->
      let eval = Eval.set_ctx ctx eval in
      let module H = (val hook : HOOK) in
      H.after_eval zone stmt lattice eval
    ) (Eval.get_ctx eval) hooks
