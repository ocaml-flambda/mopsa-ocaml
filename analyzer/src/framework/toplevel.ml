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

(** Toplevel abstraction
 
    There are two main differences with domains. First, transfer functions are
    indexed by zones to enable a faster access. Second, transfer functions are 
    not partial functions and return always a result.
*)

open Core.All
open Sig.Combiner.Stacked


(** Signature of the toplevel abstraction *)
module type TOPLEVEL =
sig

  (** {2 Abstraction header} *)
  (** ********************** *)

  type t

  val bottom: t

  val top: t

  val is_bottom: t -> bool

  val print: Format.formatter -> t -> unit


  (** {2 Lattice operators} *)
  (** ********************* *)

  val subset: (t, t) man -> uctx -> t -> t -> bool

  val join: (t, t) man -> uctx -> t -> t -> t

  val meet: (t, t) man -> uctx -> t -> t -> t

  val widen: (t, t) man -> uctx -> t -> t -> t

  val merge : t -> t * log -> t * log -> t


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> (t, t) man -> t flow

  val exec : ?semantic:semantic -> stmt -> (t, t) man -> t flow -> t flow

  val post : ?semantic:semantic -> stmt -> (t, t) man -> t flow -> t post

  val eval : ?semantic:semantic -> expr -> (t, t) man -> t flow -> t eval

  val ask  : ?semantic:semantic -> (t,'r) query -> (t, t) man -> t flow -> 'r

end


(*==========================================================================*)
(**             {2 Domain encapsulation into an abstraction}                *)
(*==========================================================================*)


let debug fmt = Debug.debug ~channel:"framework.abstraction.toplevel" fmt


(** Encapsulate a domain into a top-level abstraction *)
module Make(Domain:STACKED_COMBINER) : TOPLEVEL with type t = Domain.t
=
struct

  let () = debug "wiring table:@,%a" pp_wirinings Domain.wirings

  (** {2 Abstraction header} *)
  (** ********************** *)

  type t = Domain.t

  let bottom = Domain.bottom

  let top = Domain.top

  let is_bottom = Domain.is_bottom

  let print = Domain.print

  (** {2 Lattice operators} *)
  (** ********************* *)

  let sman : (t,unit) stack_man = {
    get_sub = (fun _ -> ());
    set_sub = (fun () a -> a);
  }

  let subset man ctx a a' =
    let b, (), () = Domain.subset man sman ctx (a,()) (a',()) in
    b

  let join man ctx a a' =
    let a, (), () = Domain.join man sman ctx (a,()) (a',()) in
    a

  let meet man ctx a a' =
    let a, (), () = Domain.meet man sman ctx (a,()) (a',()) in
    a

  let widen man ctx a a' =
    let a, (), (), _ = Domain.widen man sman ctx (a,()) (a',()) in
    a

  let merge = Domain.merge


  (** {2 Caches and semantic maps} *)
  (** **************************** *)

  (* Cache of previous evaluations and post-conditions *)
  module Cache = Core.Cache.Make(struct type t = Domain.t end)


  (** Map giving transfer functions of each semantic *)
  module SemanticMap = MapExt.Make(struct
      type t = semantic
      let compare = compare_semantic
    end)


  (** {2 Initialization} *)
  (** ****************** *)

  let init prog man : Domain.t flow =
    (* Initialize the context with an empty callstack *)
    let ctx = Context.empty |>
              Context.add_unit Context.callstack_ctx_key Callstack.empty_callstack
    in

    (* The initial flow is a singleton ⊤ environment *)
    let flow0 = Flow.singleton ctx T_cur man.lattice.top in

    (* Initialize domains *)
    let res = Domain.init prog man flow0 in

    (* Initialize hooks *)
    let () = Hook.init () in
    let ctx = Hook.init_active_hooks (Flow.get_ctx res) in

    Flow.set_ctx ctx res


  (** {2 Statement execution} *)
  (** *********************** *)

  (** Build the map of exec functions *)
  let exec_map : (stmt -> (t,t) man -> t flow -> t post option) SemanticMap.t =
    (* Add the initial implicit binding for any_semantic *)
    let map = SemanticMap.singleton any_semantic (Domain.exec []) in
    (* Iterate over the semantic dependencies *)
    Domain.dependencies |>
    List.fold_left (fun map semantic ->
        if SemanticMap.mem semantic map then map else
        try
          let domains = find_wirings semantic Domain.wirings in
          debug "exec for %a found" pp_semantic semantic;
          SemanticMap.add semantic (Domain.exec domains) map
        with Not_found -> Exceptions.panic "exec for %a not found" pp_semantic semantic
      ) map

  let post ?(semantic = any_semantic) (stmt: stmt) man (flow: Domain.t flow) : Domain.t post =
    let ctx = Hook.on_before_exec semantic stmt man flow in
    let flow = Flow.set_ctx ctx flow in

    let fexec =
      try SemanticMap.find semantic exec_map
      with Not_found -> Exceptions.panic_at stmt.srange "exec for %a not found" pp_semantic semantic
    in
    try
      let post =
        match Cache.exec fexec semantic stmt man flow with
        | None ->
          if Flow.is_bottom man.lattice flow
          then Post.return flow
          else
            Exceptions.panic_at stmt.srange
              "unable to analyze statement %a in semantic %a"
              pp_stmt stmt
              pp_semantic semantic

        | Some post -> post
      in
      let ctx = Hook.on_after_exec semantic stmt man flow post in
      Cases.set_ctx ctx post
    with
    | Exceptions.Panic(msg, line) ->
      Printexc.raise_with_backtrace
        (Exceptions.PanicAtFrame(stmt.srange, (Flow.get_callstack flow),msg, line))
        (Printexc.get_raw_backtrace())

    | Exceptions.PanicAtLocation(range, msg, line) ->
      Printexc.raise_with_backtrace
        (Exceptions.PanicAtFrame(range, (Flow.get_callstack flow),msg, line))
        (Printexc.get_raw_backtrace())

    | Sys.Break -> raise Sys.Break

    | e when (match e with Exceptions.PanicAtFrame _ -> false | _ -> true) ->
      Printexc.raise_with_backtrace
        (Exceptions.PanicAtFrame(stmt.srange, (Flow.get_callstack flow), Printexc.to_string e, ""))
        (Printexc.get_raw_backtrace())


  let exec ?(semantic = any_semantic) (stmt: stmt) man (flow: Domain.t flow) : Domain.t flow =
    let post = post ~semantic stmt man flow in
    post_to_flow man post


  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  (** Build the map of [eval] functions *)
  let eval_map : (expr -> (t,t) man -> t flow -> t rewrite option) SemanticMap.t =
    (* Add the implicit eval for any_semantic *)
    let map = SemanticMap.singleton any_semantic (Domain.eval []) in

    (* Iterate over the semantic dependencies *)
    Domain.dependencies |>
    List.fold_left (fun map semantic ->
        if SemanticMap.mem semantic map then map else
          try SemanticMap.add semantic (Domain.eval (find_wirings semantic Domain.wirings)) map
          with Not_found -> Exceptions.panic "eval for %a not found" pp_semantic semantic
      ) map

  (** Evaluation of expressions. *)
  let eval ?(semantic=any_semantic) exp man flow =
    let ctx = Hook.on_before_eval semantic exp man flow in
    let flow = Flow.set_ctx ctx flow in

    (* Get the actual semantic of the expression in case of a
       variable, since variable can have an intrinsic semantic *)
    let semantic =
      if compare_semantic semantic any_semantic = 0 then
        match ekind exp with
        | E_var (v,_) -> v.vsemantic
        | _ -> semantic
      else
        semantic
    in

    let feval =
      try SemanticMap.find semantic eval_map
      with Not_found -> Exceptions.panic_at exp.erange "eval for %a not found" pp_semantic semantic
    in
    let evl =
      (* Ask domains to perform the evaluation *) 
      match Cache.eval feval semantic exp man flow with
      | Some evl ->
        (* Check if the domain asks to forward the evaluation to some semantics *)
        evl >>$ fun erw f' ->
        begin match erw with
          | Return e'       -> Cases.singleton e' f'
          | Forward (e',s') -> man.eval ~semantic:s' e' f'
        end

      | None ->
        (* No answer, so try to visit sub-expressions *)
        let parts, builder = structure_of_expr exp in
        begin match parts with
          | {exprs; stmts = []} ->
            (* Iterate on sub-expressions *)
            Cases.bind_list exprs (fun e flow -> man.eval ~semantic e flow) flow >>$ fun exprs' f' ->
            (* Rebuild the expression from its evaluated parts *)
            let e' = builder {exprs = exprs'; stmts = []} in
            Cases.singleton e' f'

          (* XXX sub-statements are not handled for the moment *)
          | _ -> Cases.singleton exp flow
        end

    in

    (* Updates the expression transformation lineage *)
    let ret =
      evl >>$ fun exp' flow' ->
      if exp == exp' then Cases.singleton exp' flow' else Cases.singleton { exp' with eprev = Some exp } flow'
    in

    let ctx = Hook.on_after_eval semantic exp man flow ret in
    Cases.set_ctx ctx ret


  (** {2 Handler of queries} *)
  (** ********************** *)

  type ask = { doit : 'r. (t,'r) query -> (t,t) man -> t flow -> 'r option }
  
  (** Map binding semantics to the associated [ask] function *)
  let ask_map : ask SemanticMap.t =
    (* Add the implicit eval for any_semantic *)
    let map = SemanticMap.singleton any_semantic { doit = (fun q man flow -> Domain.ask [] q man flow) } in

    (* Iterate over the semantic dependencies *)
    Domain.dependencies |>
    List.fold_left (fun map semantic ->
        if SemanticMap.mem semantic map then map else
        try SemanticMap.add semantic { doit = (fun q man flow -> Domain.ask (find_wirings semantic Domain.wirings) q man flow) } map
        with Not_found -> Exceptions.panic "ask for %a not found" pp_semantic semantic
      ) map

  
  let ask : type r. ?semantic:semantic -> (t,r) query -> (t,t) man -> t flow -> r =
    fun ?(semantic=any_semantic) query man flow ->
    let f = SemanticMap.find semantic ask_map in
    match f.doit query man flow with
    | None -> raise Not_found
    | Some r -> r



end
