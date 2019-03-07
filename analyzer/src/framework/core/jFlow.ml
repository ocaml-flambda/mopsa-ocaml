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

(** Journaling flows log statements passed to domains during the computation
    of a post-state.
*)

open Ast.Stmt
open Token
open Flow
open Log
open Context
open Lattice.Sig

(** Case of a journaling disjunction *)
type 'a case = {
  tmap: ('a * log) TokenMap.t;
  ctx: 'a ctx;
}

(** Disjunction of journaling flows *)
type 'a jflow = 'a case list

let return flow : 'a jflow =
  [
    {
      tmap = TokenMap.map (fun tk e -> (e, Log.empty)) (Flow.get_token_map flow);
      ctx = Flow.get_ctx flow;
    }
  ]

let join (jflow1:'a jflow) (jflow2:'a jflow) =
  jflow1 @ jflow2

let meet (lattice:'a lattice) (jflow1:'a jflow) (jflow2:'a jflow) : 'a jflow =
  List.fold_left (fun acc case1 ->
      acc @ List.map (fun case2 ->
          {
            tmap = TokenMap.absorb2
                (fun tk (e1,log1) (e2,log2) ->
                   lattice.meet e1 e2, Log.concat log1 log2
                )
                case1.tmap case2.tmap;
            ctx = case2.ctx
          }
        ) jflow2
    ) [] jflow1

let choose_ctx jflow =
  match jflow with
  | [] -> Context.empty
  | hd :: _ -> hd.ctx

let unify_ctx (ctx:'a ctx) (jflow:'a jflow) =
  List.map (fun case -> { case with ctx }) jflow

let concat_logs (case: 'a case) (jflow:'b jflow) : 'b jflow =
  jflow |> List.map
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


let bind (f:'a flow -> 'a jflow) (jflow:'a jflow) : 'a jflow =
  let ret, ctx =
    jflow |> List.fold_left (fun (acc, ctx) case ->
        let tmap = TokenMap.map (fun tk (e, log) -> e) case.tmap in
        let flow = Flow.create case.ctx tmap in
        let ret = f flow in
        let ret' = concat_logs case ret in
        ret' @ acc, choose_ctx ret'
      ) ([], choose_ctx jflow)
  in
  unify_ctx ctx ret

let bind_eval
  (lattice:'a lattice)
  (f:'e -> 'a flow -> 'a jflow)
  (evl:('e, 'a) Eval.eval)
  : 'a jflow
  =
  let dnf = Eval.to_dnf evl in
  let ret, ctx = Dnf.fold2 (fun ctx (e, flow) ->
      let flow = Flow.set_ctx ctx flow in
      match e with
      | None -> return flow, ctx
      | Some ee ->
        let jflow = f ee flow in
        let ctx = choose_ctx jflow in
        jflow, ctx
    ) join (meet lattice) (Eval.choose_ctx evl) dnf
  in
  unify_ctx ctx ret


let map_log (f:token -> log -> log) (jflow:'a jflow) : 'a jflow =
  jflow |> List.map (fun case ->
      {
        case with
        tmap = TokenMap.map (fun tk (e,log) -> (e, f tk log)) case.tmap
      }
    )

let map (f:token -> 'a -> log -> 'b * log) (ctx: 'b ctx) (jflow:'a jflow) : 'b jflow =
  jflow |> List.map (fun case ->
      {
        tmap = TokenMap.map (fun tk (e,log) -> f tk e log) case.tmap;
        ctx;
      }
    )

let to_flow (lattice: 'a lattice) (jflow: 'a jflow) : 'a flow =
  List.fold_left (fun acc case ->
      let flow =
        Flow.create
          case.ctx
          (TokenMap.map (fun tk (e, log) -> e) case.tmap)
      in
      Flow.join lattice flow acc
    )
    (Flow.bottom Context.empty) jflow
