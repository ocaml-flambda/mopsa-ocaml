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
    indexed by paths to enable a faster access. Second, transfer functions are
    not partial functions and return always a result.
*)

open Mopsa_utils
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


  (** {2 Lattice operators} *)
  (** ********************* *)

  val subset: (t, t) man -> t ctx -> t -> t -> bool

  val join: (t, t) man -> t ctx -> t -> t -> t

  val meet: (t, t) man -> t ctx -> t -> t -> t

  val widen: (t, t) man -> t ctx -> t -> t -> t

  val merge : t -> t * teffect -> t * teffect -> t


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> (t, t) man -> t flow

  val exec : ?route:route -> stmt -> (t, t) man -> t flow -> t post

  val eval : ?route:route -> ?translate:semantic -> ?translate_when:(semantic*(expr->bool)) list -> expr -> (t, t) man -> t flow -> t eval

  val ask  : ?route:route -> (t,'r) query -> (t, t) man -> t flow -> 'r


  (** {2 Pretty printing} *)
  (** ******************* *)

  val print_state : ?route:route -> printer -> t -> unit

  val print_expr  : ?route:route -> (t,t) man -> t flow -> printer -> expr -> unit

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
    let ctx =
      singleton_ctx
        Context.callstack_ctx_key
        Callstack.empty_callstack
    in

    (* The initial flow is a singleton ⊤ environment *)
    let flow0 = Flow.singleton ctx T_cur man.lattice.top in

    (* Initialize domains *)
    let res = Domain.init prog man flow0 in

    (* Initialize hooks *)
    let () = Hook.init () in
    let ctx = Hook.init_active_hooks (Flow.get_ctx res) in

    (** Initialize the domains stack *)
    Stack.clear dstack;
    push_domain "<toplevel>";

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

  (** Hooks should not be activated within hooks exec/eval.
      The flag [inside_hook_flag] is set whenever the analyzer emits a hook
      event, in order to prevent subsequent exec/eval (i.e. called inside
      the hook) to emit hook events.
  *)
  let inside_hook_flag = ref false
  let inside_hook () = !inside_hook_flag
  let enter_hook () = assert(not (inside_hook())); inside_hook_flag := true
  let exit_hook () = assert(inside_hook ()); inside_hook_flag := false

  let exec ?(route = toplevel) (stmt: stmt) man (flow: Domain.t flow) : Domain.t post =
    let flow =
      if inside_hook () then
        flow
      else
        let () = enter_hook() in
        let x = Hook.on_before_exec route stmt man flow in
        let () = exit_hook() in
        match x with None -> flow | Some ctx -> Flow.set_ctx ctx flow
    in

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

        | Some post ->
          (* Check that all cases were handled *)
          let handled, not_handled = Cases.partition (fun c flow -> match c with NotHandled -> false | _ -> true) post in
          match not_handled with
          | None -> post
          | Some x ->
            let post' =
              x >>= fun _ flow ->
              (* Not handled cases with empty flows are OK *)
              if Flow.is_bottom man.lattice flow
              then Post.return flow
              else
                Exceptions.panic_at stmt.srange
                  "unable to analyze statement %a in %a"
                  pp_stmt stmt
                  pp_route route
            in
            match handled with
            | None -> post'
            | Some y -> Post.join x post'
      in
      let clean_post = exec_cleaners man post in
      if inside_hook () then
        clean_post
      else
        let () = enter_hook() in
        let x = Hook.on_after_exec route stmt man flow clean_post in
        let () = exit_hook () in
        match x with None -> clean_post | Some ctx -> Cases.set_ctx ctx clean_post
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

  (** Get the actual route of the expression in case of a
       variable, since variable can have an intrinsic semantic *)
  let refine_route_with_var_semantic route e =
    if compare_route route toplevel = 0 then
      match ekind e with
      | E_var (v,_) -> Semantic v.vsemantic
      | _ -> route
    else
      route

  (** Evaluate sub-nodes of an expression and rebuild it *)
  let eval_sub_expressions ?(route=toplevel) exp man flow =
    let parts, builder = structure_of_expr exp in
    match parts with
    | {exprs; stmts = []} ->
      (* Evaluate each sub-expression *)
      let n = List.length parts.exprs in
      Cases.bind_list exprs (fun e flow ->
          push_domain "<toplevel>";
          let r = man.eval ~route e flow in
          let _ = pop_domain () in
          r
        ) flow
      >>$ fun exprs flow ->
      (* Normalize translation maps by ensuring that sub-expression should be
         translated to the same semanitcs *)
      let semantics =
        List.fold_left
          (fun acc e ->
             SemanticSet.union acc
               (SemanticMap.bindings e.etrans |> List.map fst |> SemanticSet.of_list)
          ) SemanticSet.empty exprs in
      let exprs =
        List.map
          (SemanticSet.fold
             (fun s e ->
                if SemanticMap.mem s e.etrans then e else { e with etrans = SemanticMap.add s e e.etrans }
             ) semantics
          ) exprs in
      (* Rebuild the expression [exp] from its evaluated parts *)
      let exp = builder {exprs; stmts = []} in
      (* Rebuild also the translations of [exp] from the translations of its parts *)
      let etrans =
        SemanticSet.fold
          (fun s etrans ->
             let exprs =
               List.fold_left
                 (fun acc e -> SemanticMap.find s e.etrans :: acc)
                 [] exprs in
             let exp = builder {exprs = List.rev exprs; stmts=[]} in
             SemanticMap.add s exp etrans
          ) semantics SemanticMap.empty in
      Eval.singleton { exp with etrans } flow

    (* XXX sub-statements are not handled for the moment *)
    | _ -> Eval.singleton exp flow


  (** Evaluate non-handled cases *)
  let eval_not_handled ?(route=toplevel) exp man evl =
    let handled,not_handled = Cases.partition (fun c flow -> match c with NotHandled -> false | _ -> true) evl in
    let not_handled_ret =
      match not_handled with
      | None -> None
      | Some evl ->
        (* Evaluate sub-expressions of the not-handled cases *)
        let evl =
          Eval.remove_duplicates man.lattice evl >>= fun _ flow ->
          eval_sub_expressions ~route exp man flow
        in
        Some evl
    in
    OptionExt.neutral2 Cases.join handled not_handled_ret |>
    OptionExt.none_to_exn

  (** Evaluation of expressions. *)
  let eval ?(route=toplevel) ?(translate=any_semantic) ?(translate_when=[]) exp man flow =
    (* Get the translation semantic *)
    let semantic =
      if not (is_any_semantic translate) then
        translate
      else
        match List.find_opt (fun (s,f) -> f exp) translate_when with
        | Some (s,_) -> s
        | None       -> any_semantic
    in

    let flow =
      if inside_hook () then
        flow
      else
        let () = enter_hook() in
        let x = Hook.on_before_eval route semantic exp man flow in
        let () = exit_hook() in
        match x with None -> flow | Some ctx -> Flow.set_ctx ctx flow
    in

    let route = refine_route_with_var_semantic route exp in

    let feval =
      try Cache.eval (RouteMap.find route eval_map) route
      with Not_found -> Exceptions.panic_at exp.erange "eval for %a not found" pp_route route
    in
    let evl =
      match feval exp man flow with
      | None   -> eval_sub_expressions exp man flow
      | Some evl -> eval_not_handled ~route exp man evl
    in

    (* Updates the expression history *)
    let ret =
      Cases.map_result
        (fun exp' ->
           if exp == exp'
           then exp'
           else { exp' with ehistory = exp :: exp'.ehistory }
        ) evl
    in

    (** Return a translation if it was requested *)
    let ret' =
      if is_any_semantic semantic
      then ret
      else
        Cases.map_result
          (fun e ->
             try get_expr_translation semantic e
             with Not_found ->
               e (* XXX If the requested semantics is not found, we return the
                    evaluated expression and let the caller domain react *)
          ) ret
    in

    if inside_hook () then
        ret'
    else
      let () = enter_hook() in
      let x = Hook.on_after_eval route semantic exp man flow ret' in
      let () = exit_hook () in
      match x with
      | None    -> ret'
      | Some ctx -> Cases.set_ctx ctx ret'


  (** {2 Handler of queries} *)
  (** ********************** *)


  let ask : type r. ?route:route -> (t,r) query -> (t,t) man -> t flow -> r =
    fun ?(route=toplevel) query man flow ->
    (* FIXME: the map of transfer functions indexed by routes is not constructed offline, due to the GADT query *)
    let domains = if compare_route route toplevel = 0 then [] else resolve_route route Domain.routing_table in
    match Domain.ask domains query man flow with
    | None -> raise Not_found
    | Some r -> r


  (** {2 Pretty printer of states} *)
  (** **************************** *)

  (** Build the map of [print_state] functions *)
  let print_state_map : (printer -> t -> unit) RouteMap.t =
    (* Add the implicit printer for toplevel *)
    let map = RouteMap.singleton toplevel (Domain.print_state []) in

    (* Iterate over all routes *)
    get_routes Domain.routing_table |>
    List.fold_left (fun map route ->
        if RouteMap.mem route map then
          map
        else
          let domains = resolve_route route Domain.routing_table in
          RouteMap.add route (Domain.print_state domains) map
      ) map

  (** Pretty print of states *)
  let print_state ?(route=toplevel) printer a =
    match RouteMap.find_opt route print_state_map with
    | Some f ->
      f printer a

    | None ->
      Exceptions.panic "pretty printer for %a not found" pp_route route


  (** {2 Pretty printer of expressions} *)
  (** ********************************* *)

  (** Build the map of [print_expr] functions *)
  let print_expr_map : ((t,t) man -> t flow -> printer -> expr -> unit) RouteMap.t =
    (* Add the implicit printer for toplevel *)
    let map = RouteMap.singleton toplevel (Domain.print_expr []) in

    (* Iterate over all routes *)
    get_routes Domain.routing_table |>
    List.fold_left (fun map route ->
        if RouteMap.mem route map then
          map
        else
          let domains = resolve_route route Domain.routing_table in
          RouteMap.add route (Domain.print_expr domains) map
      ) map

  (** Pretty print of expression values *)
  let print_expr ?(route=toplevel) man flow printer exp =
    if Print.mem_printed_expr printer exp then () else
    let route = refine_route_with_var_semantic route exp in
    match RouteMap.find_opt route print_expr_map with
    | Some f ->
      f man flow printer exp;
      Print.add_printed_expr printer exp

    | None ->
      Exceptions.panic_at exp.erange "pretty printer for %a not found" pp_route route

end



(** {1 Utility functions} *)
(** ********************* *)

let assume
    cond ?(route=toplevel)
    ~fthen ~felse
    ?(fboth=(fun then_flow else_flow ->
        Cases.join
          (fthen then_flow)
          (felse else_flow)
      ))
    ?(fnone=(fun flow ->
        Cases.empty flow
      ))
    man flow
  =
  (* First, evaluate the condition *)
  let evl = man.eval cond flow ~route in
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
  then_post >>% fun then_flow ->
  else_post >>% fun else_flow ->
  match man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow),
        man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
  with
  | false,true  -> fthen then_flow
  | true,false  -> felse else_flow
  | true,true   -> fnone (Flow.join man.lattice then_flow else_flow)
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
  let rec aux cases =
    match cases with
    | [] -> assert false

    | [(cond, t)] -> one cond flow t

    | (cond, t) :: q ->
      let r = one cond flow t in
      let rr = aux q in
      Cases.join r rr
  in
  aux cases

