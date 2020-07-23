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

  val exec : ?route:route -> stmt -> (t, t) man -> t flow -> t flow

  val post : ?route:route -> stmt -> (t, t) man -> t flow -> t post

  val eval : ?route:route -> expr -> (t, t) man -> t flow -> t eval

  val ask  : ?route:route -> (t,'r) query -> (t, t) man -> t flow -> 'r

end


(*==========================================================================*)
(**             {2 Domain encapsulation into an abstraction}                *)
(*==========================================================================*)


let debug fmt = Debug.debug ~channel:"framework.abstraction.toplevel" fmt


(** Encapsulate a domain into a top-level abstraction *)
module Make(Domain:STACKED_COMBINER) : TOPLEVEL with type t = Domain.t
=
struct

  let () = debug "routing table:@,%a" pp_routing_table Domain.routing_table

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


  (** {2 Caches and route maps} *)
  (** **************************** *)

  (* Cache of previous evaluations and post-conditions *)
  module Cache = Core.Cache.Make(struct type t = Domain.t end)


  (** Map giving transfer functions of each route *)
  module RouteMap = MapExt.Make(struct
      type t = route
      let compare = compare_route
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
  let exec_map : (stmt -> (t,t) man -> t flow -> t post option) RouteMap.t =
    (* Add the initial implicit binding for toplevel route *)
    let map = RouteMap.singleton toplevel (Domain.exec []) in
    (* Iterate over all routes *)
    get_routes Domain.routing_table |>
    List.fold_left (fun map route ->
        if RouteMap.mem route map then
          map
        else
          let domains = resolve_route route Domain.routing_table in
          RouteMap.add route (Domain.exec domains) map
      ) map

  let post ?(route = toplevel) (stmt: stmt) man (flow: Domain.t flow) : Domain.t post =
    let ctx = Hook.on_before_exec route stmt man flow in
    let flow = Flow.set_ctx ctx flow in

    let fexec =
      try RouteMap.find route exec_map
      with Not_found -> Exceptions.panic_at stmt.srange "exec for %a not found" pp_route route
    in
    try
      let post =
        match Cache.exec fexec route stmt man flow with
        | None ->
          if Flow.is_bottom man.lattice flow
          then Post.return flow
          else
            Exceptions.panic_at stmt.srange
              "unable to analyze statement %a in %a"
              pp_stmt stmt
              pp_route route

        | Some post -> post
      in
      let ctx = Hook.on_after_exec route stmt man flow post in
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


  let exec ?(route = toplevel) (stmt: stmt) man (flow: Domain.t flow) : Domain.t flow =
    let post = post ~route stmt man flow in
    post_to_flow man post


  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  (** Build the map of [eval] functions *)
  let eval_map : (expr -> (t,t) man -> t flow -> t eval option) RouteMap.t =
    (* Add the implicit eval for toplevel *)
    let map = RouteMap.singleton toplevel (Domain.eval []) in

    (* Iterate over all routes *)
    get_routes Domain.routing_table |>
    List.fold_left (fun map route ->
        if RouteMap.mem route map then
          map
        else
          let domains = resolve_route route Domain.routing_table in
          RouteMap.add route (Domain.eval domains) map
      ) map

  (** Evaluation of expressions. *)
  let eval ?(route=toplevel) exp man flow =
    let ctx = Hook.on_before_eval route exp man flow in
    let flow = Flow.set_ctx ctx flow in


    (* Get the actual route of the expression in case of a
       variable, since variable can have an intrinsic semantic *)
    let refine_route_with_var_semantic route e =
      if compare_route route toplevel = 0 then
        match ekind e with
        | E_var (v,_) -> Semantic v.vsemantic
        | _ -> route
      else
        route
    in

    let route = refine_route_with_var_semantic route exp in

    let feval =
      try RouteMap.find route eval_map
      with Not_found -> Exceptions.panic_at exp.erange "eval for %a not found" pp_route route
    in
    let evl =
      (* Ask domains to perform the evaluation *) 
      match Cache.eval feval route exp man flow with
      | Some evl -> evl

      | None ->
        (* No answer, so try to visit sub-expressions *)
        let parts, builder = structure_of_expr exp in
        begin match parts with
          | {exprs; stmts = []} ->
            (* Iterate over sub-expressions *)
            Cases.bind_list exprs (fun e flow -> man.eval ~route e flow) flow >>$ fun exprs' f' ->
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

    let ctx = Hook.on_after_eval route exp man flow ret in
    Cases.set_ctx ctx ret


  (** {2 Handler of queries} *)
  (** ********************** *)


  let ask : type r. ?route:route -> (t,r) query -> (t,t) man -> t flow -> r =
    fun ?(route=toplevel) query man flow ->
    (* FIXME: the map of transfer functions indexed by routes is not constructed offline, due to the GADT query *)
    let domains = if compare_route route toplevel = 0 then [] else resolve_route route Domain.routing_table in
    match Domain.ask domains query man flow with
    | None -> raise Not_found
    | Some r -> r


end
