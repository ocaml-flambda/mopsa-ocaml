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

let mk_exec_of_zone_list (l: Zone.zone list) exec =
  let l = List.sort_uniq Pervasives.compare l in
  let exec_list = List.map exec l in
  (fun (stmt: Ast.stmt) (man: ('a, 't) man) (flow: 'a flow) : 'a Post.post option ->
     let rec aux =
       function
       | [] -> None
       | f :: tl ->
         match f stmt man flow with
         | None -> aux tl
         | Some ret -> Some ret
     in
     aux exec_list
  )

let mk_eval_of_zone_list (l: (Zone.zone * Zone.zone) list) eval =
  let l = List.sort_uniq Pervasives.compare l in
  let eval_list = List.map eval l in
  (fun (exp: Ast.expr) (man: ('a, 't) man) (flow: 'a flow) : ('a, Ast.expr) evl option ->
     let rec aux =
       function
       | [] -> None
       | f :: tl ->
         match f exp man flow with
         | None -> aux tl
         | Some ret -> Some ret
     in
     aux eval_list
  )


(**
   Functor to create an [Analyzer] module from an top-level abstract domain.
*)
module Make(Domain : DOMAIN) =
struct


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
            match List.find_all (fun z -> Zone.subset z zone) Domain.exec_interface.export with
            | [] ->
              Debug.warn "exec for %a not found" Zone.print zone;
              acc

            | l ->
              let f = mk_exec_of_zone_list l Domain.exec in

              debug "exec for %a found" Zone.print zone;
              ExecMap.add zone f acc
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

    let flow' = match zone_exec stmt man flow with
      | None ->
        Exceptions.panic
          "Unable to analyze statement in %a:@\n @[%a@]"
          Location.pp_range_verbose stmt.srange
          pp_stmt stmt

      | Some post -> post.flow

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
            match List.find_all (fun p -> Zone.subset2 p zpath) Domain.eval_interface.export with
            | [] ->
              Debug.warn "eval for %a not found" Zone.print2 zpath;
              acc

            | l ->
              let f = mk_eval_of_zone_list l Domain.eval in
              debug "eval for %a found" Zone.print2 zpath;
              EvalMap.add zpath f acc

          end
      ) EvalMap.empty (List.sort_uniq Pervasives.compare ((Zone.top, Zone.top)  :: Domain.eval_interface.import))


  (** Evaluation of expressions. *)
  and eval ?(zone = (Zone.top, Zone.top)) (exp: Ast.expr) (flow: Domain.t flow) : (Domain.t, Ast.expr) evl =
    debug
      "eval expr on zone %a in %a:@\n @[%a@]@\n input:@\n  @[%a@]"
      Zone.print2 zone
      Location.pp_range_verbose exp.erange
      pp_expr exp (Flow.print man) flow
    ;
    let timer = Timing.start () in
    let path_eval = EvalMap.find zone eval_map in
    let evl = match path_eval exp man flow with
      | Some evl -> evl
      | None -> Eval.singleton exp flow

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
      (Eval.print ~pp:pp_expr) evl
    ;
    evl

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
    ask = ask;
  }

end
