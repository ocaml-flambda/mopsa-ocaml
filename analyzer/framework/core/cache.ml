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

(** Cache of post-conditions and evaluations *)

open Flow
open Eval
open Post
open Manager
open Ast.Expr
open Ast.Stmt
open Route
open Alarm
open Mopsa_utils

let debug fmt = Debug.debug ~channel:"framework.core.cache" fmt

let opt_cache = ref 2


(****************************************************************************)
(**                             {2 Queue}                                   *)
(****************************************************************************)

module type KEY =
sig
  type t
  val equal : t -> t -> bool
end


module Queue(Key:KEY) =
struct

  type 'a t = {
    elements: (Key.t * 'a) option array;
    size: int;
    mutable next: int;
  }


  let create (size:int) : 'a t =
    {
      elements = Array.make size None;
      size = size;
      next = 0;
    }


  let add (k:Key.t) (v:'a) (q:'a t) : unit =
    Array.set q.elements q.next (Some (k, v));
    q.next <- (q.next + 1) mod q.size


  let find (k:Key.t) (q:'a t) : 'a =
    let prev i =
      if i = 0 then q.size - 1 else i - 1
    in

    let rec aux i =
      if i = q.next
      then raise Not_found

      else match Array.get q.elements i with
        | None -> raise Not_found
        | Some (k',v) when Key.equal k k' -> v
        | _ -> aux (prev i)
    in
    aux (prev q.next)
      

end



(****************************************************************************)
(**                             {2 Cache}                                   *)
(****************************************************************************)

module Make(Domain: sig type t end) =
struct

  (** {2 Cache of post-conditions} *)
  (** **************************** *)

  module ExecCache = Queue(
    struct
      type t = route * stmt * Domain.t Token.TokenMap.t * Alarm.report
      let equal (route1,stmt1,tmap1,report1) (route2,stmt2,tmap2,report2) =
        compare_route route1 route2 = 0 &&
        stmt1 == stmt2 &&
        tmap1 == tmap2 &&
        report1 == report2
    end
    )

  let exec_cache : Domain.t post option ExecCache.t = ExecCache.create !opt_cache

  let exec f semantic stmt man flow =
    if !opt_cache = 0
    then f stmt man flow

    else try
        let tmap = Flow.get_token_map flow in
        let report = Flow.get_report flow in
        let opost = ExecCache.find (semantic,stmt,tmap,report) exec_cache in
        OptionExt.lift (fun post ->
            Cases.set_ctx (
              Context.most_recent_ctx (Cases.get_ctx post) (Flow.get_ctx flow)
            ) post
          ) opost
      with Not_found ->
        let post = f stmt man flow in
        ExecCache.add (semantic, stmt, Flow.get_token_map flow, Flow.get_report flow) post exec_cache;
        post


  (** {2 Cache of evaluations} *)
  (** ************************ *)

  module EvalCache = Queue(
    struct
      type t = route * expr * Domain.t Token.TokenMap.t * Alarm.report
      let equal (route1,exp1,tmap1,report1) (route2,exp2,tmap2,report2) =
        compare_route route1 route2 = 0 &&
        exp1 == exp2 &&
        tmap1 == tmap2 &&
        report1 == report2
    end
    )

  let eval_cache : Domain.t eval option EvalCache.t = EvalCache.create !opt_cache

  let eval f route exp man flow =
    if !opt_cache = 0
    then f exp man flow
    else try
        let tmap = Flow.get_token_map flow in
        let report = Flow.get_report flow in
        let evls = EvalCache.find (route,exp,tmap,report) eval_cache in
        OptionExt.lift (fun evl ->
            let ctx = Context.most_recent_ctx (Cases.get_ctx evl) (Flow.get_ctx flow) in
            Cases.set_ctx ctx evl
          ) evls
      with Not_found ->
        let evals = f exp man flow in
        EvalCache.add (route, exp, Flow.get_token_map flow, Flow.get_report flow) evals eval_cache;
        evals

end
