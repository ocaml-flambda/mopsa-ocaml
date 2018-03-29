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
open Domains.Global

let debug fmt = Debug.debug ~channel:"framework.analyzer" fmt


(**
   Functor to create an [Analyzer] module from an top-level abstract domain.
*)
module Make(Domain : Domains.Global.DOMAIN) =
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

  let rec exec stmt ctx (fa: Domain.t flow) =
    debug
      "exec stmt in %a:@\n @[%a@]@\n input:@\n  @[%a@]"
      Pp.pp_range_verbose stmt.srange
      Pp.pp_stmt stmt manager.flow.print fa
    ;
    let timer = Timing.start () in
    try
      let res =
        let fa1 = Domain.exec stmt manager ctx fa in
        match fa1 with
        | None ->
          Debug.warn
            "Non-analyzed statement in %a:@\nstmt: @[%a@]@\n@[%a@]"
            Pp.pp_range_verbose stmt.srange
            Pp.pp_stmt stmt
            manager.flow.print fa
          ;
          raise (StmtPanic stmt)

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
    with Panic ->
      raise (StmtPanic stmt)

  (** Evaluation of expressions. *)
  and eval exp ctx fa =
    try
      let evl = Domain.eval exp manager ctx fa in
      match evl with
      | Some evl -> evl
      | None ->
        (* Debug.warn
         *   "Non-evaluated expression in %a:@\nexpr: @[%a@]@\n@[%a@]"
         *   Pp.pp_range_verbose exp.erange
         *   Pp.pp_expr exp
         *   manager.flow.print fa
         * ; *)
        (* raise (ExprPanic exp) *)
        eval_singleton (Some exp, fa, [])
    with Panic ->
      raise (ExprPanic exp)

  (** Query handler. *)
  and ask : type b. b Query.query -> Context.context -> 'a -> b option =
    fun query ctx gabs ->
      Domain.ask query manager ctx gabs

  (** Top level manager *)

  and manager = {
    env = env_manager;
    flow = flow_manager;
    exec = exec;
    eval = eval;
    ask = ask;
    ax = ax;
  }

  let init prog =
    let fa = flow_manager.bottom |>
             flow_manager.set TCur env_manager.top
    in
    Domain.init prog manager fa


end
