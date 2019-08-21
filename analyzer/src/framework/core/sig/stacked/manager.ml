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


(** Analysis manager of stacked abstract domains *)


open Ast.All
open Lattice
open Token
open Flow
open Zone
open Eval
open Post
open Log
open Context
open Result


let debug fmt = Debug.debug ~channel:"framework.core.sig.stacked.manager" fmt

(*==========================================================================*)
(**                         {2 Stack managers}                              *)
(*==========================================================================*)

(** Managers provide access to full analyzer and the sub-tree
    abstraction of the stack domain.
*)
type ('a, 't, 's) man = {
  (* Lattice operators over global abstract elements ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (* Accessors to the sub-tree abstract element ['s] within ['a] *)
  get_sub : 'a -> 's;
  set_sub : 's -> 'a -> 'a;

  (** Analyzer transfer functions *)
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  post : ?zone:zone -> stmt -> 'a flow -> 'a post;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> 'a eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;

  (** Accessors to the domain's merge logs *)
  get_log : log -> log;
  set_log : log -> log -> log;

  (** Accessors to the sub-tree merge logs *)
  get_sub_log : log -> log;
  set_sub_log : log -> log -> log;

  (** Sub-tree merger *)
  merge_sub : 's -> 's * log -> 's * log -> 's;
}




(*==========================================================================*)
(**                        {2 Utility functions}                            *)
(*==========================================================================*)

let set_env (tk:token) (env:'t) (man:('a,'t,'s) man) (flow:'a flow) : 'a flow =
  Flow.set tk (man.set env (Flow.get tk man.lattice flow)) man.lattice flow

let get_env (tk:token) (man:('a,'t,'s) man) (flow:'a flow) : 't =
  man.get (Flow.get tk man.lattice flow)

let map_env (tk:token) (f:'t -> 't) (man:('a,'t,'s) man) (flow:'a flow) : 'a flow =
  set_env tk (f (get_env tk man flow)) man flow

let set_sub_env (tk:token) (env:'t) (man:('a,'t,'s) man) (flow:'a flow) : 'a flow =
  Flow.set tk (man.set_sub env (Flow.get tk man.lattice flow)) man.lattice flow

let get_sub_env (tk:token) (man:('a,'t,'s) man) (flow:'a flow) : 's =
  man.get_sub (Flow.get tk man.lattice flow)

let map_sub_env (tk:token) (f:'s -> 's) (man:('a,'t,'s) man) (flow:'a flow) : 'a flow =
  set_sub_env tk (f (get_sub_env tk man flow)) man flow


let assume
    cond ?(zone=any_zone)
    ~fthen ~felse
    ?(fnone=(fun flow -> Result.empty flow))
    man flow
  =
  let then_post = man.post ~zone (mk_assume cond cond.erange) flow in
  let flow = Flow.set_ctx (Post.get_ctx then_post) flow in
  let else_post = man.post ~zone (mk_assume (mk_not cond cond.erange) cond.erange) flow in

  then_post >>= fun _ then_flow ->
  else_post >>= fun _ else_flow ->

  match man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow),
        man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
  with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | true, true -> fnone (Flow.join man.lattice then_flow else_flow)
  | false, false ->
    let then_res = fthen then_flow in
    let else_flow' = Flow.set_ctx (Result.get_ctx then_res) else_flow in
    let else_res = felse else_flow' in
    Result.join then_res else_res


let assume_flow
    ?(zone=any_zone) cond
    ~fthen ~felse
    ?(fnone=(fun flow -> flow))
    man flow
  =
  let then_flow = man.exec ~zone (mk_assume cond cond.erange) flow in
  let flow = Flow.set_ctx (Flow.get_ctx then_flow) flow in
  let else_flow = man.exec ~zone (mk_assume (mk_not cond cond.erange) cond.erange) flow in

  match man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow),
        man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
  with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | true, true -> fnone (Flow.join man.lattice then_flow else_flow)
  | false, false ->
    let then_res = fthen then_flow in
    let else_flow' = Flow.copy_ctx then_res else_flow in
    let else_res = felse else_flow' in
    Flow.join man.lattice then_res else_res


let switch
    (cases : (expr list * ('a Flow.flow -> ('a,'r) Result.result)) list)
    ?(zone = any_zone)
    man flow
  : ('a,'r) result
  =
  let rec one (cond : expr list) acc f =
    match cond with
    | [] -> Some (f acc)
    | x :: tl ->
      let s = mk_assume x x.erange in
      man.post ~zone s acc >>=? fun _ acc' ->
      if Flow.get T_cur man.lattice acc' |> man.lattice.is_bottom then
        None
      else
        one tl acc' f
  in
  let rec aux cases =
    match cases with
    | [] -> None

    | (cond, t) :: q ->
      Option.neutral2 Result.join (one cond flow t) (aux q)
  in
  match aux cases with
  | None -> assert false

  | Some x -> x


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


let exec_block_on_all_flows block man flow =
  List.fold_left (fun flow stmt ->
      exec_stmt_on_all_flows stmt man flow
    ) flow block


let post_to_flow man post =
  Result.apply_full
    (fun _ flow _ cleaners -> exec_block_on_all_flows cleaners man flow )
    (Flow.join man.lattice)
    (Flow.join man.lattice)
    post
