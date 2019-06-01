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

(** Post-states with journaling flows. *)

open Ast.Stmt
open Token
open Flow
open Log
open Context
open Lattice


(** Disjunction case of a post-state *)
type 'a case = ('a * log) TokenMap.t

(** Disjunction of journaling flows *)
type 'a post = {
  cases: 'a case list;
  ctx : 'a ctx;
}

let empty ctx = {
  cases = [];
  ctx = ctx;
}

let debug fmt = Debug.debug ~channel:"framework.core.post" fmt


let return flow : 'a post =
  {
    cases = [
      TokenMap.map (fun tk e -> (e, Log.empty)) (Flow.get_token_map flow)
    ];
    ctx = Flow.get_ctx flow;
  }


let pp_case lattice fmt case =
  let tmap' = TokenMap.map (fun tk (e,log) -> e) case in
  TokenMap.print lattice fmt tmap'


let print (lattice:'a lattice) fmt (post:'a post) : unit =
  let open Format in
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ∨@ ")
    (pp_case lattice)
    fmt post.cases


let print_log fmt (post:'a post) : unit =
  let open Format in
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ∨@ ")
    (fun fmt case ->
       TokenMap.iter (fun tk (e,log) ->
           fprintf fmt "%a: %a" pp_token tk Log.print log
         ) case
    )
    fmt post.cases


let join (post1:'a post) (post2:'a post) =
  {
    cases = post1.cases @ post2.cases;
    ctx = Context.get_most_recent post1.ctx post2.ctx
  }


let get_ctx (post:'a post) : 'a ctx =
  post.ctx


let set_ctx (ctx:'a ctx) (post:'a post) =
  {
    post with
    ctx = ctx;
  }


let merge (f:token -> ('a*log) -> ('a*log) -> ('a*log)) (post1:'a post) (post2:'a post) : 'a post =
  let cases =
    List.fold_left (fun acc case1 ->
        acc @ List.map (fun case2 ->
            TokenMap.absorb2 f case1 case2
        ) post2.cases
    ) [] post1.cases |>
    List.filter (fun case ->
        not (TokenMap.is_empty case)
      )
  in
  {
    cases = cases;
    ctx = Context.get_most_recent post1.ctx post2.ctx
  }


let meet (lattice:'a lattice) (post1:'a post) (post2:'a post) : 'a post =
  let ctx = Context.get_most_recent post1.ctx post2.ctx in
  merge (fun token (a1,log1) (a2,log2) ->
      lattice.meet (Context.get_unit ctx) a1 a2, Log.concat log1 log2
    ) post1 post2


let concat_logs (case: 'a case) (post:'b post) : 'b post =
  let cases =
    post.cases |> List.map
      (fun case' ->
         TokenMap.map (fun tk (e,log) ->
             if not (TokenMap.mem tk case)
             then (e,log)
             else
               let _, log' = TokenMap.find tk case in
               (e, Log.concat log' log)
           ) case'
      )
  in
  { post with cases }


let to_flow (lattice: 'a lattice) (post: 'a post) : 'a flow =
  List.fold_left (fun acc case ->
      let flow =
        Flow.create
          post.ctx
          (TokenMap.map (fun tk (e, log) -> e) case)
      in
      Flow.join lattice acc flow
    )
    (Flow.bottom post.ctx) post.cases


let bind (f:'a flow -> 'a post) (post:'a post) : 'a post =
  post.cases |> List.fold_left (fun acc case ->
      let tmap = TokenMap.map (fun tk (e, log) -> e) case in
      let flow = Flow.create acc.ctx tmap in
      let ret = f flow in
      let ret' = concat_logs case ret in
      join ret' acc
    ) (empty post.ctx)


let map_log (f:token -> log -> log) (post:'a post) : 'a post =
  {
    post with
    cases = post.cases |> List.map (fun case ->
        TokenMap.map (fun tk (e,log) -> (e, f tk log)) case
      )
  }

let map (f:token -> 'a -> log -> 'b * log) (ctx: 'b ctx) (post:'a post) : 'b post =
  {
    cases = post.cases |> List.map (fun case ->
        TokenMap.map (fun tk (e,log) -> f tk e log) case
      );
    ctx;
  }
