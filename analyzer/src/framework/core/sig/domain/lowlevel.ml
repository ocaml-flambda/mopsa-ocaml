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

(** Low level signature of domains. Similar to the general-purpose domain
    signature, except that lattice operators are defined on the global
    abstraction.
*)


open Ast
open Program
open Expr
open Stmt
open Context
open Token
open Lattice
open Flow
open Eval
open Query
open Log
open Post
open Zone
open Id
open Interface
open Channel


(*==========================================================================*)
(**                         {2 Domain manager}                              *)
(*==========================================================================*)


(** Managers provide access to full analyzer, i.e. (i) the lattice
    operators of the global abstraction ['a], (ii) the transfer functions
    over ['a flow] and (iii) accessors to the domain's abstract element ['t]
    within ['a].
*)
type ('a, 't) man = {
  (* Lattice operators over global abstract elements ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Analyzer transfer functions *)
  post : ?zone:zone -> stmt -> 'a flow -> 'a post;
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> (expr, 'a) eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;

  (** Accessors to the domain's merging logs *)
  get_log : log -> log;
  set_log : log -> log -> log;
}



(*==========================================================================*)
(**                            {2 Signature}                                *)
(*==========================================================================*)


(** Low-level signature of an abstract domain *)
module type DOMAIN =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t domain
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Interface of the domain *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val is_bottom: t -> bool
  (** [is_bottom man a] tests whether [a] is bottom or not. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val subset: ('a,t) man -> uctx -> 'a -> 'a -> bool
  (** [subset man a1 a2] provides a partial order relation over
      elements of the domain by testing whether [a1] is related to (or
     included in) [a2]. *)

  val join: ('a,t) man -> uctx -> 'a -> 'a -> t
  (** [join man a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: ('a,t) man -> uctx -> 'a -> 'a -> t
  (** [meet man a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: ('a,t) man -> uctx -> 'a -> 'a -> t
  (** [widen man ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)

  val merge: uctx -> t -> t * log -> t * log -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t) man -> 'a flow -> 'a flow
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)

  val refine : channel -> ('a,t) man -> 'a flow -> 'a flow with_channel
  (** Refinement using reduction channels *)

end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let domains : (module DOMAIN) list ref = ref []

let register_domain dom =
  domains := dom :: !domains

let find_domain name =
  List.find (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let mem_domain name =
  List.exists (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let names () =
  List.map (fun dom ->
      let module D = (val dom : DOMAIN) in
      D.name
    ) !domains



(*==========================================================================*)
(**                        {2 Utility functions}                            *)
(*==========================================================================*)

let log_post_stmt stmt man post =
  Post.map_log (fun tk log ->
      match tk with
      | T_cur -> man.set_log (man.get_log log |> Log.append stmt) log
      | _ -> log
    ) post

let set_domain_env (tk:token) (env:'t) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  Flow.set tk (man.set env (Flow.get tk man.lattice flow)) man.lattice flow

let get_domain_env (tk:token) (man:('a,'t) man) (flow:'a flow) : 't =
  man.get (Flow.get tk man.lattice flow)

let map_domain_env (tk:token) (f:'t -> 't) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  set_domain_env tk (f (get_domain_env tk man flow)) man flow

let mem_domain_env (tk:token) (f:'t -> bool) (man:('a,'t) man) (flow:'a flow) : bool =
  get_domain_env tk man flow |>
  f

let assume cond ?(zone = any_zone)
    ~fthen ~felse ~fboth ~fnone
    man flow
  =
  let then_flow = man.exec ~zone (mk_assume cond cond.erange) flow in
  let flow = Flow.copy_ctx then_flow flow in
  let else_flow = man.exec ~zone (mk_assume (mk_not cond cond.erange) cond.erange) flow in
  let then_flow = Flow.copy_ctx else_flow then_flow in
  match man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow),
        man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
  with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | false, false -> fboth then_flow else_flow
  | true, true -> fnone (Flow.join man.lattice then_flow else_flow)

let assume_eval cond ?(zone = any_zone)
    ~fthen ~felse
    ?(fboth = (fun flow1 flow2 ->
        let fthen_r = fthen flow1 in
        let flow2 = Flow.set_ctx (Eval.choose_ctx fthen_r) flow2 in
        let felse_r = felse flow2 in
        Eval.join fthen_r felse_r))
    ?(fnone = (fun flow -> empty_singleton flow))
    man flow
  =
  assume cond ~zone ~fthen ~felse ~fboth ~fnone man flow

let assume_post cond ?(zone = any_zone)
    ~fthen ~felse
    ?(fboth = (fun flow1 flow2 ->
        let fthen_r = fthen flow1 in
        let flow2 = Flow.set_ctx (Post.choose_ctx fthen_r) flow2 in
        Post.join fthen_r (felse flow2)))
    ?(fnone = (fun flow -> Post.return flow))
    man flow
  : 'a post  =
  assume cond ~zone ~fthen ~felse ~fboth ~fnone man flow

let switch
    (cases : (((expr * bool) list) * ('a Flow.flow -> 'b)) list)
    ~join
    ?(zone = any_zone)
    man flow
  : 'b  =
  match cases with
  | [] -> assert false

  | (cond, t) :: q ->
    let one (cond : (expr * bool) list) t =
      List.fold_left (fun acc (x, b) ->
          let s =
            if b then (mk_assume x x.erange)
            else (mk_assume (mk_not x x.erange) x.erange)
          in
          man.exec ~zone s acc
        ) flow cond
      |> t
    in
    List.fold_left (fun acc (cond, t) -> join (one cond t) acc) (one cond t) q

let switch_eval = switch ~join:Eval.join

let switch_post = switch ~join:Post.join

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


let exec_eval
  (man:('a,'t) man)
  (f:'e -> 'a flow -> 'a flow)
  (evl:('e, 'a) eval)
  : 'a flow
  =
  let ctx, ret = Eval.fold_apply
      (fun ctx e flow cleaners ->
         let flow = Flow.set_ctx ctx flow in
         match e with
         | None ->
           let flow' = exec_block_on_all_flows cleaners man flow in
           Flow.get_ctx flow', flow'

         | Some ee ->
           let flow' = f ee flow in
           let flow'' = exec_block_on_all_flows cleaners man flow' in
           Flow.get_ctx flow'', flow''
      )
      (Flow.join man.lattice)
      (Flow.meet man.lattice)
      (Eval.choose_ctx evl) evl
  in
  Flow.set_ctx ctx ret


let post_eval
    (man:('a,'t) man)
    (f:'e -> 'a flow -> 'a post)
    (evl:('e, 'a) Eval.eval)
  : 'a post
  =
  let ctx, ret = Eval.fold_apply
      (fun ctx e flow cleaners ->
         let flow = Flow.set_ctx ctx flow in
         match e with
         | None ->
           let post' = exec_block_on_all_flows cleaners man flow |> Post.return in
           Post.choose_ctx post', post'

         | Some ee ->
           let post = f ee flow in
           let post' = Post.bind (fun flow ->
               exec_block_on_all_flows cleaners man flow |>
               Post.return (* FIXME: do we need to log cleaners? *)
             ) post
           in
           Post.choose_ctx post', post'
      )
      Post.join
      (Post.meet man.lattice)
      (Eval.choose_ctx evl) evl
  in
  Post.set_ctx ctx ret

let post_eval_with_cleaners
    (man:('a,'t) man)
    (f:'e -> 'a flow -> stmt list -> 'a post)
    (evl:('e, 'a) Eval.eval)
  : 'a post
  =
  let ctx, ret = Eval.fold_apply
      (fun ctx e flow cleaners ->
         let flow = Flow.set_ctx ctx flow in
         match e with
         | None ->
           let flow = exec_block_on_all_flows cleaners man flow in
           Flow.get_ctx flow, Post.return flow

         | Some ee ->
           let post = f ee flow cleaners in
           Post.choose_ctx post, post
      )
      Post.join
      (Post.meet man.lattice)
      (Eval.choose_ctx evl) evl
  in
  Post.set_ctx ctx ret
