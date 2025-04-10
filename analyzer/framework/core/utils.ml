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
open Ast.Semantic
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

let set_env (tk:token) (env:'t) (man:('a,'t) man) (flow:'a flow) : 'a post =
  man.set tk env flow

let set_singleton_env env ctx man abs =
  let flow = Flow.singleton ctx T_cur abs in
  let post = man.set T_cur env flow in
  let oabs =
    Cases.fold
      (fun acc _ flow ->
         let abs = Flow.get T_cur man.lattice flow in
         match acc with
         | None -> Some abs
         | Some acc -> Some (man.lattice.join ctx acc abs)
      ) None post
  in
  match oabs with
  | None     -> man.lattice.bottom
  | Some abs -> abs

let set_env_flow tk env man flow : 'a flow =
  man.set tk env flow |>
  post_to_flow man

let get_env (tk:token) (man:('a,'t) man) (flow:'a flow) : ('a, 't) cases =
  man.get tk flow

let get_singleton_env ctx man abs =
  let flow = Flow.singleton ctx T_cur abs in
  let cases = man.get T_cur flow in
  if Cases.is_singleton cases then
    let env, flow = Cases.choose_result cases in
    env
  else
    Exceptions.panic "get_singleton_env called on a multi-partition environment"

let get_singleton_env_from_flow (tk:token) (man:('a,'t) man) (flow:'a flow) : 't =
  let cases = man.get T_cur flow in
  if Cases.is_singleton cases then
    let env, flow = Cases.choose_result cases in
    env
  else
    Exceptions.panic "get_env_old called on a multi-partition environment"

let map_env (tk:token) (f:'t -> 't) (man:('a,'t) man) (flow:'a flow) : 'a post =
  get_env tk man flow >>$ fun env flow ->
  let env' = f env in
  set_env tk env' man flow

let get_pair_fst man tk flow =
  man.get tk flow |>
  Cases.map_result fst

let set_pair_fst man tk a1 flow =
  get_env tk man flow >>$ fun old flow ->
  if a1 == fst old then
    Post.return flow
  else
    set_env tk (a1, snd old) man flow

let get_pair_snd man tk flow =
  man.get tk flow |>
  Cases.map_result snd

let set_pair_snd man tk a2 flow =
  get_env tk man flow >>$ fun old flow ->
  if a2 == snd old then
    Post.return flow
  else
    set_env tk (fst old, a2) man flow

let env_exec (f:'a flow -> 'a post) ctx (man:('a,'t) man) (a:'a) : 'a =
  (* Create a singleton flow with the given environment *)
  let flow = Flow.singleton ctx T_cur a in
  (* Execute the statement *)
  let flow' = f flow |> post_to_flow man in
  Flow.get T_cur man.lattice flow'

let ask_and_reduce_cases f q ?(bottom = fun () -> assert false) a =
  let cases = f q a in
  Cases.reduce_result
    (fun r flow -> r)
    ~join:(Query.join_query q)
    ~meet:(Query.meet_query q)
    ~bottom
    cases

let ask_and_reduce_list f q ?(bottom = fun () -> assert false) a =
  let l = f q a in
  match l with
  | [] -> bottom ()
  | (_,r)::tl ->
    List.fold_left
      (fun acc (_,r) -> Query.join_query q acc r)
      r tl

let ask_and_reduce = ask_and_reduce_cases

let find_var_by_name ?(function_scope = None) name man flow =
  let vars = ask_and_reduce man.ask (Query.Q_defined_variables function_scope) flow in
  List.find
    (fun v ->
       name = Format.asprintf "%a" Ast.Var.pp_var v
    ) vars

let dummy_range = Location.mk_fresh_range ()

let pp_vars_info man flow fmt vars =
  let printer = Print.empty_printer () in
  List.iter
    (fun v ->
       try man.print_expr flow printer (mk_var v dummy_range)
       with Not_found -> ()
    ) vars;
  Format.fprintf fmt "%a" Print.pflush printer

let pp_vars_info_by_name man flow fmt names =
  pp_vars_info man flow fmt (ListExt.map_filter (fun name ->
      try Some (find_var_by_name name man flow) 
      with Not_found -> None
    ) names)

let pp_expr_vars_info man flow fmt e =
  let vars = Ast.Visitor.expr_vars e in
  pp_vars_info man flow fmt vars

let pp_stmt_vars_info man flow fmt s =
  let vars = Ast.Visitor.stmt_vars s in
  pp_vars_info man flow fmt vars

let breakpoint name man flow =
  let _ = man.exec (mk_breakpoint name dummy_range) flow in
  ()
