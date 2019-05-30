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
type 'a case = {
  tmap: ('a * log) TokenMap.t;
  ctx: 'a ctx;
}

(** Disjunction of journaling flows *)
type 'a post = 'a case list


let debug fmt = Debug.debug ~channel:"framework.core.post" fmt


let return flow : 'a post =
  [
    {
      tmap = TokenMap.map (fun tk e -> (e, Log.empty)) (Flow.get_token_map flow);
      ctx = Flow.get_ctx flow;
    }
  ]

let pp_case lattice fmt case =
  let tmap' = TokenMap.map (fun tk (e,log) -> e) case.tmap in
  TokenMap.print lattice fmt tmap'

let print (lattice:'a lattice) fmt (post:'a post) : unit =
  let open Format in
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ∨@ ")
    (pp_case lattice)
    fmt post

let print_log fmt (post:'a post) : unit =
  let open Format in
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@ ∨@ ")
    (fun fmt case ->
       TokenMap.iter (fun tk (e,log) ->
           fprintf fmt "%a: %a" pp_token tk Log.print log
         ) case.tmap
    )
    fmt post


let join (post1:'a post) (post2:'a post) =
  post1 @ post2

let choose_ctx post =
  match post with
  | [] -> Context.empty
  | hd :: _ -> hd.ctx

let set_ctx (ctx:'a ctx) (post:'a post) =
  List.map (fun case -> { case with ctx }) post


let merge (f:token -> ('a*log) -> ('a*log) -> ('a*log)) (post1:'a post) (post2:'a post) : 'a post =
  List.fold_left (fun acc case1 ->
      acc @ List.map (fun case2 ->
          {
            tmap = TokenMap.absorb2 f case1.tmap case2.tmap;
            ctx = case2.ctx
          }
        ) post2
    ) [] post1 |>
  List.filter (fun case ->
      not (TokenMap.is_empty case.tmap)
  )


let meet (lattice:'a lattice) (post1:'a post) (post2:'a post) : 'a post =
  let ctx = choose_ctx post2 in
  merge (fun token (a1,log1) (a2,log2) ->
      lattice.meet (Context.get_unit ctx) a1 a2, Log.concat log1 log2
    ) post1 post2


let concat_logs (case: 'a case) (post:'b post) : 'b post =
  post |> List.map
    (fun case' ->
       {
         case' with
         tmap = TokenMap.map (fun tk (e,log) ->
             if not (TokenMap.mem tk case.tmap)
             then (e,log)
             else
               let _, log' = TokenMap.find tk case.tmap in
               (e, Log.concat log' log)
           ) case'.tmap
       }
    )

let to_flow (lattice: 'a lattice) (post: 'a post) : 'a flow =
  List.fold_left (fun acc case ->
      let flow =
        Flow.create
          case.ctx
          (TokenMap.map (fun tk (e, log) -> e) case.tmap)
      in
      Flow.join lattice acc flow
    )
    (Flow.bottom (choose_ctx post)) post


let bind (f:'a flow -> 'a post) (post:'a post) : 'a post =
  let ret, ctx =
    post |> List.fold_left (fun (acc, ctx) case ->
        let tmap = TokenMap.map (fun tk (e, log) -> e) case.tmap in
        let flow = Flow.create case.ctx tmap in
        let ret = f flow in
        let ret' = concat_logs case ret in
        ret' @ acc, choose_ctx ret'
      ) ([], choose_ctx post)
  in
  set_ctx ctx ret


let map_log (f:token -> log -> log) (post:'a post) : 'a post =
  post |> List.map (fun case ->
      {
        case with
        tmap = TokenMap.map (fun tk (e,log) -> (e, f tk log)) case.tmap
      }
    )

let map (f:token -> 'a -> log -> 'b * log) (ctx: 'b ctx) (post:'a post) : 'b post =
  post |> List.map (fun case ->
      {
        tmap = TokenMap.map (fun tk (e,log) -> f tk e log) case.tmap;
        ctx;
      }
    )
