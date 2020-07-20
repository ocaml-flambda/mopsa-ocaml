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


(** Manager - access to the top-level lattice and transfer functions *)


open Ast.Stmt
open Ast.Expr
open Lattice
open Token
open Flow
open Eval
open Post
open Log
open Context
open Cases
open Semantic


(*==========================================================================*)
(**                             {2 Managers}                                *)
(*==========================================================================*)


(** Managers provide access to full analyzer *)
type ('a, 't) man = {
  (* Lattice operators of the toplevel abstract element ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (* Toplevel transfer functions *)
  exec : stmt -> ?semantic:semantic -> 'a flow -> 'a flow;
  post : stmt -> ?semantic:semantic -> 'a flow -> 'a post;
  eval : expr -> ?semantic:semantic -> 'a flow -> 'a eval;
  ask : 'r. ('a,'r) Query.query -> 'a flow -> 'r;

  (* Accessors to the domain's logs *)
  get_log : log -> log;
  set_log : log -> log -> log;
}


(** Managers provide access to the sub-tree of stacked domain *)
type ('a, 's) stack_man = {
  (* Accessors the sub-domain's abstract element ['s] within ['a] *)
  get_sub : 'a -> 's;
  set_sub : 's -> 'a -> 'a;
}


(*==========================================================================*)
(**                        {2 Utility functions}                            *)
(*==========================================================================*)

let set_env (tk:token) (env:'t) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  Flow.set tk (man.set env (Flow.get tk man.lattice flow)) man.lattice flow

let get_env (tk:token) (man:('a,'t) man) (flow:'a flow) : 't =
  man.get (Flow.get tk man.lattice flow)

let map_env (tk:token) (f:'t -> 't) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  set_env tk (f (get_env tk man flow)) man flow


let assume
    cond ?(semantic=any_semantic)
    ~fthen ~felse
    ?(negate=mk_not cond cond.erange)
    man flow
  =
  let then_post = man.post ~semantic (mk_assume cond cond.erange) flow in
  let flow = Flow.set_ctx (Cases.get_ctx then_post) flow in
  let else_post = man.post ~semantic (mk_assume negate negate.erange) flow in

  let then_res = then_post >>$? fun () then_flow ->
    if man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow)
       && Alarm.AlarmSet.subset (Flow.get_alarms then_flow) (Flow.get_alarms flow)
    then None
    else Some (fthen then_flow)
  in

  let else_res = else_post >>$? fun () else_flow ->
    if man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
       && Alarm.AlarmSet.subset (Flow.get_alarms else_flow) (Flow.get_alarms flow)
    then None
    else Some (felse else_flow)
  in

  match OptionExt.neutral2 Cases.join then_res else_res with
  | None -> Cases.empty_singleton flow
  | Some r -> r


let assume_flow
    ?(semantic=any_semantic) cond
    ~fthen ~felse
    ?(negate=mk_not)
    man flow
  =
  let then_flow = man.exec ~semantic (mk_assume cond cond.erange) flow in
  let flow = Flow.set_ctx (Flow.get_ctx then_flow) flow in
  let else_flow = man.exec ~semantic (mk_assume (negate cond cond.erange) cond.erange) flow in

  match man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow),
        man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
  with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | true, true -> Flow.join man.lattice then_flow else_flow
  | false, false ->
    let then_res = fthen then_flow in
    let else_flow' = Flow.copy_ctx then_res else_flow in
    let else_res = felse else_flow' in
    Flow.join man.lattice then_res else_res


let switch
    (cases : (expr list * ('a Flow.flow -> ('a,'r) cases)) list)
    ?(semantic = any_semantic)
    man flow
  : ('a,'r) cases
  =
  let rec one (cond : expr list) acc f =
    match cond with
    | [] -> f acc
    | x :: tl ->
      let s = mk_assume x x.erange in
      man.post ~semantic s acc >>$ fun _ acc' ->
      if Flow.get T_cur man.lattice acc' |> man.lattice.is_bottom then
        Cases.empty_singleton acc'
      else
        one tl acc' f
  in
  let rec aux cases =
    match cases with
    | [] -> assert false

    | [(cond, t)] -> one cond flow t

    | (cond, t) :: q ->
      let r = one cond flow t in
      let rr = aux q in
      Cases.join r rr
  in
  aux cases


let exec_stmt_on_all_flows stmt man flow =
  Flow.fold (fun flow tk env ->
      (* Put env in T_cur token of flow and remove others *)
      let annot = Flow.get_ctx flow in
      let flow' = Flow.singleton annot T_cur env in

      (* Execute the cleaner *)
      let flow'' = man.exec stmt flow' in

      (* Restore T_cur in tk *)
      Flow.copy T_cur tk man.lattice flow'' flow |>
      Flow.copy_ctx flow''
    ) flow flow


let apply_cleaners block man flow =
  let exec stmt flow =
    if !Cases.opt_clean_cur_only then
      man.exec ~semantic:any_semantic stmt flow
    else
      exec_stmt_on_all_flows stmt man flow
  in
  List.fold_left (fun flow stmt ->
      exec stmt flow
    ) flow block


let post_to_flow man post =
  Cases.apply_full
    (fun _ flow _ cleaners -> apply_cleaners cleaners man flow )
    (Flow.join man.lattice)
    (Flow.meet man.lattice)
    post


let get_pair_fst man = (fun a -> man.get a |> fst)
let set_pair_fst man = (fun a1 a -> let old = man.get a in if a1 == fst old then a else man.set (a1, snd old) a)

let get_pair_snd man = (fun a -> man.get a |> snd)
let set_pair_snd man = (fun a2 a -> let old = man.get a in if a2 == snd old then a else man.set (fst old, a2) a)


let env_exec (f:'a flow -> 'a post) ctx (man:('a,'t) man) (a:'a) : 'a =
  (* Create a singleton flow with the given environment *)
  let flow = Flow.singleton Context.(set_unit ctx empty) T_cur a in
  (* Execute the statement *)
  let flow' = f flow |> post_to_flow man in
  Flow.get T_cur man.lattice flow'


let sub_env_exec (f:'a flow -> 'a post) ctx (man:('a,'t) man) (sman:('a,'s) stack_man) (a:'t) (s:'s) : 't * 's =
  let aa = env_exec f ctx man (man.lattice.top |> man.set a |> sman.set_sub s) in
  man.get aa, sman.get_sub aa
