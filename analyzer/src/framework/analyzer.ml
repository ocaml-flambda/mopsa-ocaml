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

let profiler fmt = Debug.debug ~channel:"framework.analyzer.profiler" fmt

(**
   Functor to create an [Analyzer] module from an top-level abstract domain.
*)
module Make(Domain : DOMAIN) =
struct

  (* Html output of the evaluation/execution tree *)
  module Out = Output.Html.Make(Domain)

  let output_actions () = Out.dump_html_actions ()
  (* Cache of previous evaluations and post-conditions *)
  module Cache = Cache.Make(struct type t = Domain.t end)


  (** Map giving the [exec] transfer function of a zone *)
  module ExecMap = MapExt.Make(struct
      type t = zone
      let compare = compare_zone
    end)


  (** Map giving the [eval] evaluation function of a zone path *)
  module EvalMap = MapExt.Make(struct
      type t = zone * zone
      let compare = compare_zone2
    end)

  let eval_graph = Zone.build_eval_graph Domain.eval_interface.export


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
    let required = Domain.exec_interface.import in
    (* Add implicit import of Z_any *)
    let map = ExecMap.singleton any_zone (Domain.exec any_zone) in
    (* Iterate over the required zones of domain D *)
    required |>
    List.fold_left (fun map zone ->
        if ExecMap.mem zone map
        then map
        else
          let () = debug "Searching for an exec function for the zone %a" pp_zone zone in
          if List.exists (fun z -> sat_zone z zone) Domain.exec_interface.export
          then
            let () = debug "exec for %a found" pp_zone zone in
            ExecMap.add zone (Domain.exec zone) map
          else
            let () = Exceptions.warn "exec for %a not found" pp_zone zone in
            map
      ) map

  and exec ?(zone = any_zone) (stmt: Ast.stmt) (flow: Domain.t flow) : Domain.t flow =
    Out.push_action (Exec({s = stmt; z= zone}));
    debug "exec stmt in %a:@\n @[%a@]@\n zone: %a@\n input:@\n  @[%a@]"
      Location.pp_range_verbose stmt.srange
      pp_stmt stmt
      pp_zone zone
      (Flow.print man) flow
    ;
    let timer = Timing.start () in

    let fexec =
      try ExecMap.find zone exec_map
      with Not_found -> Exceptions.panic_at stmt.srange "exec for %a not found" pp_zone zone
    in
    let flow' = Cache.exec fexec zone stmt man flow in

    let t = Timing.stop timer in
    profiler "exec done in %.3fs of:@\n@[<v>  %a@]" t pp_stmt stmt;
    Out.push_action (ExecDone({s_res = flow'}));
    debug "exec stmt done:@\n @[%a@]@\n zone: %a@\n input:@\n@[  %a@]@\n output@\n@[  %a@]"
      pp_stmt stmt
      pp_zone zone
      (Flow.print man) flow
      (Flow.print man) flow'
    ;
    flow'



  (*==========================================================================*)
  (**                   {2 Evaluation of expressions}                         *)
  (*==========================================================================*)

  (** Build the map of [eval] functions *)
  and eval_map =
    debug "Eval graph: @[%a@]" Zone.pp_graph eval_graph;

    (* Add the implicit [* -> *] eval path that uses all domains *)
    let map = EvalMap.singleton
        (any_zone, any_zone)
        [[(any_zone, any_zone, [any_zone, any_zone], Domain.eval (any_zone, any_zone))]]
    in

    (* Iterate over the required zone paths of domain Domain *)
    let required = Domain.eval_interface.import in
    required |>
    List.fold_left (fun acc (src, dst) ->
        if EvalMap.mem (src, dst) acc then acc
        else
          begin
            debug "Searching for an eval function for the zone %a" pp_zone2 (src, dst);
            let paths = Zone.find_all_eval_paths src dst eval_graph in
            if List.length paths = 0
            then
              let () = Exceptions.warn "eval for %a not found" pp_zone2 (src, dst) in
              acc
            else
              let () = debug "eval for %a found@\npaths: @[%a@]"
                  pp_zone2 (src, dst)
                  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") Zone.pp_eval_path) paths
              in
              (* Map each hop to an eval function *)
              let eval_paths = List.map (fun path ->
                  let rec aux =
                    function
                    | [] -> []
                    | (z1, z2) :: tl -> (z1, z2, path, Domain.eval (z1, z2)) :: aux tl
                  in
                  aux path
                ) paths
              in
              EvalMap.add (src, dst) eval_paths acc
          end
      )
      map

  (** Evaluation of expressions. *)
  and eval ?(zone = (any_zone, any_zone)) (exp: Ast.expr) (flow: Domain.t flow) : (Domain.t, Ast.expr) evl =
    Out.push_action (Eval({e = exp ; zs = zone}));
    debug "eval expr in %a:@\n @[%a@]@\n zone: %a@\n input:@\n  @[%a@]"
      Location.pp_range_verbose exp.erange
      pp_expr exp
      pp_zone2 zone
      (Flow.print man) flow
    ;
    let timer = Timing.start () in

    let ret =
      (* Check whether exp is already in the desired zone *)
      match Zone.eval exp (snd zone) with
      | Keep ->
        debug "already in zone";
        Eval.singleton exp flow

      | other_action ->
        (* Try available eval paths in sequence *)
        let paths =
          try EvalMap.find zone eval_map
          with Not_found -> Exceptions.panic_at exp.erange "eval for %a not found" pp_zone2 zone
        in
        match eval_over_paths paths exp man flow with
        | Some evl -> evl
        | None ->
          match other_action with
          | Keep -> assert false (* already done *)
          | Process -> Eval.singleton exp flow
          | Visit ->
            debug "visiting sub-expressions";
            let open Visitor in
            let parts, builder = split_expr exp in
            match parts with
            | {exprs; stmts = []} ->
              Eval.eval_list exprs (fun exp flow ->
                  match eval_over_paths paths exp man flow with
                  | None -> Eval.singleton exp flow
                  | Some evl -> evl
                ) flow |>
              Eval.bind @@ fun exprs flow ->
              let exp = builder {exprs; stmts = []} in
              Eval.singleton exp flow

            | _ -> Eval.singleton exp flow
    in

    let t = Timing.stop timer in
    profiler "eval done in %.3fs of @[%a@]" t pp_expr exp;
    Out.push_action (EvalDone({e_res = ret}));
    debug "eval expr done:@\n expr: @[%a@]@\n zone: %a@\n input:@\n@[  %a@]@\n output@\n@[  %a@]"
      pp_expr exp
      pp_zone2 zone
      (Flow.print man) flow
      (Eval.print ~pp:pp_expr) ret
    ;
    ret

  and eval_over_paths paths exp man flow =
    match paths with
    | [] -> None
    | path :: tl ->
      match eval_over_path path man exp flow with
      | None -> eval_over_paths tl exp man flow
      | ret -> ret

  and eval_over_path path man exp flow =
    match path with
    | [] -> None
    | [(z1, z2, path, feval)] ->
      debug "trying path %a" pp_eval_path path;
      eval_hop z1 z2 feval man exp  flow
    | (z1, z2, path, feval) :: tl ->
      debug "trying path %a" pp_eval_path path;
      eval_hop z1 z2 feval man exp flow |>
      OptionExt.bind @@
      Eval.bind_opt @@
      eval_over_path tl man

  and eval_hop z1 z2 feval man exp flow =
    debug "eval %a in hop %a" pp_expr exp pp_zone2 (z1, z2);
    match Zone.eval exp z2 with
    | Keep ->
      debug "already in zone";
      Eval.singleton exp flow |>
      OptionExt.return
    | other_action ->
      match Cache.eval feval (z1, z2) exp man flow with
      | Some evl -> Some evl
      | None ->
        match other_action with
        | Keep -> assert false
        | Process ->
          debug "no answer";
          None
        | Visit ->
          debug "visiting sub-expressions";
          let open Visitor in
          let parts, builder = split_expr exp in
          match parts with
          | {exprs; stmts = []} ->
            Eval.eval_list_opt exprs (eval_hop z1 z2 feval man) flow |>
            OptionExt.lift @@ Eval.bind @@ fun exprs flow ->
            let exp = builder {exprs; stmts = []} in
            Eval.singleton exp flow

          | _ -> None


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
