(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)



(** Analyzer - Central orchestrer of the analysis architecture. *)



open Ast
open Visitor
open Lattice
open Flow
open Manager
open Domains.Stateful
open Eval
    
let debug fmt = Debug.debug ~channel:"framework.analyzer" fmt


(**
   Functor to create an [Analyzer] module from an top-level abstract domain.
*)
module Make(Domain : DOMAIN) =
struct

  let env_manager = {
    bottom = Domain.bottom;
    top = Domain.top;
    is_bottom = Domain.is_bottom;
    is_top = Domain.is_top;
    leq = Domain.leq;
    join = Domain.join;
    meet = Domain.meet;
    widening = Domain.widening;
    print = Domain.print;
  }

  let flow_manager = Flow.lift_lattice_manager env_manager

  (** Top-level accessor. *)
  let ax = {
    get = (fun gabs -> gabs);
    set = (fun gabs' gabs -> gabs');
  }

  (** Abstract transfer function of statements. *)

  let rec exec ctx stmt (fa: Domain.t flow) =
    debug
      "exec stmt in %a:@\n @[%a@]@\n input:@\n  @[%a@]"
      Pp.pp_range_verbose stmt.srange
      Pp.pp_stmt stmt manager.flow.print fa
    ;
    let timer = Timing.start () in
    let res =
      let fa1 = Domain.exec manager ctx stmt fa in
      match fa1 with
      | None ->
        Exceptions.panic
          "Unable to analyze statement in %a:@\n @[%a@]"
          Pp.pp_range_verbose stmt.srange
          Pp.pp_stmt stmt

      | Some fa1 ->
        fa1
    in
    let t = Timing.stop timer in
    Debug.debug
      ~channel:"framework.analyzer.profiler"
      "exec done in %.6fs of:@\n@[<v>  %a@]"
      t Pp.pp_stmt stmt
    ;
    debug
      "exec stmt done:@\n @[%a@]@\n input:@\n@[  %a@]@\n output@\n@[  %a@]"
      Pp.pp_stmt stmt manager.flow.print fa manager.flow.print res
    ;
    res

  (** Evaluation of expressions. *)
  and eval ctx exp fa =
    debug
      "eval expr in %a:@\n @[%a@]@\n input:@\n  @[%a@]"
      Pp.pp_range_verbose exp.erange
      Pp.pp_expr exp manager.flow.print fa
    ;
    let timer = Timing.start () in
    let res =
      let evl = Domain.eval manager ctx exp fa in
      match evl with
      | Some evl -> evl
      | None -> eval_singleton (Some exp, fa, [])
    in
    let t = Timing.stop timer in
    Debug.debug
      ~channel:"framework.analyzer.profiler"
      "eval done in %.6fs of @[%a@]"
      t Pp.pp_expr exp
    ;
    debug
      "eval expr done:@\n @[%a@]@\n input:@\n@[  %a@]@\n output@\n@[  %a@]"
      Pp.pp_expr exp manager.flow.print fa (pp_evals Pp.pp_expr) res
    ;
    res


  (** Query handler. *)
  and ask : type b. Context.context -> b Query.query -> 'a -> b option =
    fun ctx query gabs ->
      Domain.ask manager ctx query gabs

  and ecache = ref []
  and scache = ref []

  and exec_cache ctx stmt flow =
    if Options.(common_options.cache_size) == 0 then exec ctx stmt flow
    else try
        let flow' = List.assoc (ctx, stmt, flow) !scache in
        debug "use cache";
        flow'
      with Not_found ->
        debug "not in cache";
        let flow' = exec ctx stmt flow in
        scache := ((ctx, stmt, flow), flow') :: (if List.length !scache < Options.(common_options.cache_size) then !scache else List.rev @@ List.tl @@ List.rev !scache);
        flow'
        
  and eval_cache ctx exp flow =
    if Options.(common_options.cache_size) == 0 then eval ctx exp flow
    else try
        let evals = List.assoc (ctx, exp, flow) !ecache in
        debug "use cache";
        evals
      with Not_found ->
        debug "not in cache";
        let evals = eval ctx exp flow in
        ecache := ((ctx, exp, flow), evals) :: (if List.length !ecache < Options.(common_options.cache_size) then !ecache else List.rev @@ List.tl @@ List.rev !ecache);
        evals

  (** Top level manager *)

  and manager = {
    env = env_manager;
    flow = flow_manager;
    exec = exec_cache;
    eval = eval_cache;
    ask = ask;
    ax = ax;
  }

  let init prog =
    let fa = flow_manager.bottom |>
             flow_manager.set TCur env_manager.top
    in
    Domain.init  manager Context.empty prog fa


end
