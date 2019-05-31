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
open Sig.Domain.Lowlevel
open Ast.Expr
open Ast.Stmt
open Zone

let debug fmt = Debug.debug ~channel:"framework.core.cache" fmt

let opt_cache = ref 10


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
      type t = zone * stmt * Domain.t Token.TokenMap.t
      let equal (zone1,stmt1,tmap1) (zone2,stmt2,tmap2) =
        zone1 = zone2 &&
        stmt1 == stmt2 &&
        tmap1 == tmap2
    end
    )

  let exec_cache : Domain.t post ExecCache.t = ExecCache.create !opt_cache

  let exec f zone stmt man flow =
    let ff () =
      match f stmt man flow with
      | None ->
        if Flow.is_bottom man.lattice flow
        then Post.return flow

        else Exceptions.panic
            "Unable to analyze statement in %a:@\n @[%a@]"
            Location.pp_range stmt.srange
            pp_stmt stmt

      | Some post -> post
    in
    if !opt_cache = 0
    then ff ()

    else try
        let tmap = Flow.get_token_map flow in
        let post = ExecCache.find (zone,stmt,tmap) exec_cache in
        Post.set_ctx (Flow.get_ctx flow) post
      with Not_found ->
        let post = ff () in
        ExecCache.add (zone, stmt, Flow.get_token_map flow) post exec_cache;
        post


  (** {2 Cache of evaluations} *)
  (** ************************ *)

  module EvalCache = Queue(
    struct
      type t = (zone * zone) * expr * Domain.t Token.TokenMap.t
      let equal (zone1,exp1,tmap1) (zone2,exp2,tmap2) =
        zone1 = zone2 &&
        exp1 == exp2 &&
        tmap1 == tmap2
    end
    )

  let eval_cache : (expr, Domain.t) eval option EvalCache.t = EvalCache.create !opt_cache

 
  let eval f zone exp man flow =
    if !opt_cache = 0
    then f exp man flow

    else try
        let tmap = Flow.get_token_map flow in
        let evls = EvalCache.find (zone,exp,tmap) eval_cache in
        Option.lift (Eval.map_flow (Flow.copy_ctx flow)) evls
      with Not_found ->
        let evals = f exp man flow in
        EvalCache.add (zone, exp, Flow.get_token_map flow) evals eval_cache;
        evals

end
