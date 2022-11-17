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

(** Utility functions *)

open Ast.Stmt
open Ast.Expr
open Manager
open Cases
open Post
open Semantic
open Route
open Token
open Flow
open Mopsa_utils



let rec exec_cleaner stmt man flow =
  let post = man.exec stmt flow in
  if !Cases.opt_clean_cur_only then
    post
  else
    post >>% fun flow ->
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


and exec_cleaners man post =
  let post' =
    post >>= fun case flow ->
    let cleaners = Cases.get_case_cleaners case in
    StmtSet.fold
      (fun stmt acc ->
         acc >>% exec_cleaner stmt man
      ) cleaners (Post.return flow)
  in
  (* Now that post is clean, remove all cleaners *)
  Cases.set_cleaners [] post'


and post_to_flow man post =
  let clean = exec_cleaners man post in
  Cases.reduce
    (fun _ flow -> flow)
    ~join:(Flow.join man.lattice)
    ~meet:(Flow.meet man.lattice)
    clean


let assume
    cond ?(route=toplevel) ?(translate=any_semantic)
    ~fthen ~felse
    ?(fboth=(fun then_flow else_flow ->
        let ret1 = fthen then_flow in
        let ctx1 = Cases.get_ctx ret1 in
        let ret2 = Flow.set_ctx ctx1 else_flow |>
                   felse
        in
        let ret1 = Cases.copy_ctx ret2 ret1 in
        Cases.join ret1 ret2
      ))
    ?(fnone=(fun then_flow else_flow ->
        Cases.join
          (Cases.empty then_flow)
          (Cases.empty else_flow)
      ))
    ?(eval=true)
    man flow
  =
  (* First, evaluate the condition *)
  let evl = if eval then man.eval cond flow ~route ~translate else Eval.singleton cond flow in
  (* Filter flows that satisfy the condition *)
  let then_post = ( evl >>$ fun cond flow -> man.exec (mk_assume cond cond.erange) flow ~route ) |>
                  (* Execute the cleaners of the evaluation here *)
                  exec_cleaners man |>
                  Post.remove_duplicates man.lattice
  in
  (* Propagate the flow-insensitive context to the other branch *)
  let then_ctx = Cases.get_ctx then_post in
  let evl' = Cases.set_ctx then_ctx evl in
  let else_post = ( evl' >>$ fun cond flow -> man.exec (mk_assume (mk_not cond cond.erange) cond.erange) flow ~route ) |>
                  (* Execute the cleaners of the evaluation here *)
                  exec_cleaners man |>
                  Post.remove_duplicates man.lattice
  in
  (* Re-propagate the context to the first branch *)
  let then_post' = Cases.copy_ctx else_post then_post in
  (* Apply transfer functions depending on condition satisfiability *)
  then_post' >>% fun then_flow ->
  else_post >>% fun else_flow ->
  match man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow),
        man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
  with
  | false,true  -> fthen then_flow
  | true,false  -> felse else_flow
  | true,true   -> fnone then_flow else_flow
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
      let s = mk_assume x x.erange in
      man.exec ~route s acc >>% fun acc' ->
      if Flow.get T_cur man.lattice acc' |> man.lattice.is_bottom then
        Cases.empty acc'
      else
        one tl acc' f
  in
  let rec aux ctx cases =
    match cases with
    | [] -> assert false

    | [(cond, t)] -> one cond (Flow.set_ctx ctx flow) t

    | (cond, t) :: q ->
      let r = one cond (Flow.set_ctx ctx flow) t in
      let rr = aux (Cases.get_ctx r) q in
      Cases.join (Cases.copy_ctx rr r) rr
  in
  aux (Flow.get_ctx flow) cases

let set_env (tk:token) (env:'t) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  Flow.set tk (man.set env (Flow.get tk man.lattice flow)) man.lattice flow

let get_env (tk:token) (man:('a,'t) man) (flow:'a flow) : 't =
  man.get (Flow.get tk man.lattice flow)

let map_env (tk:token) (f:'t -> 't) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  set_env tk (f (get_env tk man flow)) man flow

let get_pair_fst man = (fun a -> man.get a |> fst)
let set_pair_fst man = (fun a1 a -> let old = man.get a in if a1 == fst old then a else man.set (a1, snd old) a)

let get_pair_snd man = (fun a -> man.get a |> snd)
let set_pair_snd man = (fun a2 a -> let old = man.get a in if a2 == snd old then a else man.set (fst old, a2) a)

let env_exec (f:'a flow -> 'a post) ctx (man:('a,'t) man) (a:'a) : 'a =
  (* Create a singleton flow with the given environment *)
  let flow = Flow.singleton ctx T_cur a in
  (* Execute the statement *)
  let flow' = f flow |> post_to_flow man in
  Flow.get T_cur man.lattice flow'

let sub_env_exec (f:'a flow -> 'a post) ctx (man:('a,'t) man) (sman:('a,'s) stack_man) (a:'t) (s:'s) : 't * 's =
  let aa = env_exec f ctx man (man.lattice.top |> man.set a |> sman.set_sub s) in
  man.get aa, sman.get_sub aa
