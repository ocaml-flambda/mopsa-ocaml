(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Analyzer - Central orchestrer of the analysis architecture. *)

open Ast
open Lattice
open Flow
open Manager
open Domain
open Eval
open Post
open Zone

let debug fmt = Debug.debug ~channel:"framework.analyzer" fmt


(**
   Functor to create an [Analyzer] module from an top-level abstract domain.
*)
module Make(Domain : DOMAIN) =
struct

  module Cache = Cache.Make(struct type t = Domain.t end)


  (** Map giving the [exec] transfer function of a zone *)
  module ExecMap = MapExt.Make(struct type t = Zone.zone let compare = compare end)


  (** Map giving the [eval] evaluation function of a zone path *)
  module EvalMap = MapExt.Make(struct type t = Zone.zone * Zone.zone let compare = Zone.compare2 end)


  (*==========================================================================*)
  (**                        {2 Initialization}                               *)
  (*==========================================================================*)

  let rec init prog : Domain.t flow =
    let flow0 = Flow.bottom Annotation.empty  |>
                Flow.set T_cur man.top man
    in
    match Domain.init prog man flow0 with
    | None -> flow0
    | Some flow -> flow



  (*==========================================================================*)
  (**                     {2 Statements execution}                            *)
  (*==========================================================================*)

  (** Build the map of exec functions *)
  and exec_map =
    (* Iterate over the required zones of domain D *)
    List.fold_left (fun acc zone ->
        if ExecMap.mem zone acc then acc
        else
          begin
            debug "Searching for an exec function for the zone %a" Zone.print zone;
            if List.exists (fun z -> Zone.subset z zone) Domain.exec_interface.export then
              begin
                debug "exec for %a found" Zone.print zone;
                ExecMap.add zone (Domain.exec zone) acc
              end
            else
              Exceptions.panic "exec for %a not found" Zone.print zone
          end
      ) ExecMap.empty (List.sort_uniq Pervasives.compare (Zone.top :: Domain.exec_interface.import))


  and exec ?(zone = Zone.top) (stmt: Ast.stmt) (flow: Domain.t flow) : Domain.t flow =
    debug
      "exec stmt in %a:@\n @[%a@]@\n input:@\n  @[%a@]"
      Location.pp_range_verbose stmt.srange
      pp_stmt stmt (Flow.print man) flow
    ;
    let timer = Timing.start () in

    let zone_exec = ExecMap.find zone exec_map in

    let flow' = Cache.exec (fun stmt flow ->
        match zone_exec stmt man flow with
        | None ->
          Exceptions.panic
            "Unable to analyze statement in %a:@\n @[%a@]"
            Location.pp_range_verbose stmt.srange
            pp_stmt stmt

        | Some post -> post.flow
      ) zone stmt flow
    in

    let t = Timing.stop timer in
    Debug.debug ~channel:"framework.analyzer.profiler"
      "exec done in %.6fs of:@\n@[<v>  %a@]"
      t pp_stmt stmt
    ;

    debug
      "exec stmt done:@\n @[%a@]@\n input:@\n@[  %a@]@\n output@\n@[  %a@]"
      pp_stmt stmt (Flow.print man) flow (Flow.print man) flow'
    ;
    flow'



  (*==========================================================================*)
  (**                   {2 Evaluation of expressions}                         *)
  (*==========================================================================*)

  (** Build the map of [eval] functions *)
  and eval_map =
    (* Iterate over the required zone paths of domain Domain *)
    List.fold_left (fun acc zpath ->
        if EvalMap.mem zpath acc then acc
        else
          begin
            debug "Searching for eval function for the zone path %a" Zone.print2 zpath;
            if List.exists (fun p -> Zone.subset2 p zpath) Domain.eval_interface.export then
              begin
                debug "eval for %a found" Zone.print2 zpath;
                EvalMap.add zpath (Domain.eval zpath) acc
              end
            else
              Exceptions.panic "eval for %a not found" Zone.print2 zpath
          end
      ) EvalMap.empty (List.sort_uniq Pervasives.compare ((Zone.top, Zone.top)  :: Domain.eval_interface.import))


  (** Evaluation of expressions. *)
  and eval_opt ?(zone = (Zone.top, Zone.top)) (exp: Ast.expr) (flow: Domain.t flow) : (Domain.t, Ast.expr) evl option =
    debug
      "eval expr on zone %a in %a:@\n @[%a@]@\n input:@\n  @[%a@]"
      Zone.print2 zone
      Location.pp_range_verbose exp.erange
      pp_expr exp (Flow.print man) flow
    ;
    let timer = Timing.start () in
    let path_eval = EvalMap.find zone eval_map in

    let evl = Cache.eval (fun exp flow ->
        path_eval exp man flow
      ) zone exp flow
    in

    let t = Timing.stop timer in
    Debug.debug
      ~channel:"framework.analyzer.profiler"
      "eval done in %.6fs of @[%a@]"
      t pp_expr exp
    ;
    debug
      "eval expr done:@\n @[%a@]@\n input:@\n@[  %a@]@\n output@\n@[  %a@]"
      pp_expr exp
      (Flow.print man) flow
      (Option.print (Eval.print ~pp:pp_expr)) evl
    ;
    evl

  and eval ?(zone = (Zone.top, Zone.top)) (exp: Ast.expr) (flow: Domain.t flow) : (Domain.t, Ast.expr) evl =
    match eval_opt ~zone exp flow with
    | Some evl -> evl
    | None -> Eval.singleton exp flow


  (** Query handler. *)
  and ask : type r. r Query.query -> _ -> r =
    fun query flow ->
      match Domain.ask query man flow with
      | None -> raise Not_found
      | Some r -> r


  (** Top level manager *)

  and man : (Domain.t, Domain.t) man = {
    bottom = Domain.bottom;
    top = Domain.top;
    is_bottom = Domain.is_bottom;
    subset = Domain.subset;
    join = Domain.join;
    meet = Domain.meet;
    widen = Domain.widen;
    print = Domain.print;
    get = (fun flow -> flow);
    set = (fun flow _ -> flow);
    exec = exec;
    eval = eval;
    eval_opt = eval_opt;
    ask = ask;
  }

end
