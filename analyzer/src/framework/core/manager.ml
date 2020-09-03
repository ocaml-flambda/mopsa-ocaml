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
open Route
open Query

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
  exec : ?route:route -> stmt -> 'a flow -> 'a post;
  eval : ?route:route -> expr -> 'a flow -> 'a eval;
  ask : 'r. ?route:route -> ('a,'r) query -> 'a flow -> 'r;

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


let rec exec_stmt_on_all_flows stmt man flow =
  man.exec stmt flow >>% fun flow ->
  Flow.fold (fun acc tk env ->
      match tk with
      (* Skip T_cur since the statement was executed at the beginning
         of the function *)
      | T_cur -> acc
      | _ ->
        (* Put env in T_cur token of flow and remove others *)
        let annot = Flow.get_ctx flow in
        let flow' = Flow.singleton annot T_cur env in

        (* Execute the cleaner *)
        let flow'' = man.exec stmt flow' |> post_to_flow man in

        (* Restore T_cur in tk *)
        Flow.copy T_cur tk man.lattice flow'' flow |>
        Flow.copy_ctx flow''
    ) flow flow |>
  Post.return


and apply_cleaners cleaners man flow =
  let exec stmt flow =
    if !Cases.opt_clean_cur_only then
      man.exec stmt flow
    else
      exec_stmt_on_all_flows stmt man flow
  in
  StmtSet.fold (fun stmt acc ->
      acc >>% exec stmt
    ) cleaners (Post.return flow)

and post_to_flow man post =
  let to_flow f post =
    Cases.reduce f
      ~join:(Flow.join man.lattice)
      ~meet:(Flow.meet man.lattice)
      post
  in
  to_flow
    (fun case flow ->
       let cleaners = Cases.get_case_cleaners case in
       apply_cleaners cleaners man flow |>
       to_flow
         (fun _ flow -> flow)
    )
    post

let assume
    cond ?(route=toplevel)
    ~fthen ~felse
    ?(fboth=(fun then_flow else_flow ->
        Cases.join
          (fthen then_flow)
          (felse else_flow)
      ))
    man flow
  =
  man.eval cond flow ~route >>$ fun cond flow ->
  let then_post = man.exec ~route (mk_assume cond cond.erange) flow in
  let flow = Flow.set_ctx (Cases.get_ctx then_post) flow in
  let else_post = man.exec ~route (mk_assume (mk_not cond cond.erange) cond.erange) flow in
  then_post >>% fun then_flow ->
  else_post >>% fun else_flow ->
  match man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow),
        man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
  with
  | false,true  -> fthen then_flow
  | true,false  -> felse else_flow
  | true,true   -> Cases.empty (Flow.join man.lattice then_flow else_flow)
  | false,false -> fboth then_flow else_flow 


let switch
    (cases : (expr list * ('a Flow.flow -> ('a,'r) cases)) list)
    ?(route = toplevel)
    man flow
  : ('a,'r) cases
  =
  let rec one (cond : expr list) acc f =
    match cond with
    | [] -> f acc
    | x :: tl ->
      man.eval ~route x acc >>$ fun x acc' ->
      let s = mk_assume x x.erange in
      man.exec ~route s acc' >>% fun acc'' ->
      if Flow.get T_cur man.lattice acc'' |> man.lattice.is_bottom then
        Cases.empty acc''
      else
        one tl acc'' f
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
