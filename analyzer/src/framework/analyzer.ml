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

let progress fmt = Debug.debug ~channel:"framework.progress" fmt

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

  (* Filter paths that pass through [via] zone *)
  let find_eval_paths_via (src, dst) via map =
    let paths = EvalMap.find (src, dst) map in
    List.filter (fun path ->
        List.exists (fun (z1, z2, _, _) -> Zone.sat_zone z1 via || Zone.sat_zone z2 via) path
      ) paths

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
    | Some flow ->
      let rec call_callbacks flow callbacks =
        match callbacks with
        | [] -> flow
        | hd :: tl ->
          call_callbacks (hd flow) tl
      in
      call_callbacks flow.flow flow.callbacks


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
          if List.exists (fun z -> sat_zone z zone) Domain.exec_interface.export
          then
            ExecMap.add zone (Domain.exec zone) map
          else
            let () = Exceptions.warn "exec for %a not found" pp_zone zone in
            map
      ) map

  and exec ?(zone = any_zone) (stmt: Ast.stmt) (flow: Domain.t flow) : Domain.t flow =
    Out.push_action (Exec({s = stmt; z= zone}));
    progress "analyzing %a" Location.pp_range (Location.untag_range stmt.srange);
    debug "exec:@\n stmt: @[%a@]@\n loc: @[%a@]@\n zone: %a@\n input:@\n  @[%a@]"
      pp_stmt stmt
      Location.pp_range stmt.srange
      pp_zone zone
      (Flow.print man) flow
    ;

    let timer = Timing.start () in

    let fexec =
      try ExecMap.find zone exec_map
      with Not_found -> Exceptions.panic_at stmt.srange "exec for %a not found" pp_zone zone
    in
    let flow' = Cache.exec fexec zone stmt man flow in

    Out.push_action (ExecDone({s_res = flow'}));
    debug "exec done:@\n stmt: @[%a@]@\n loc: @[%a@]@\n zone: %a@\n input:@\n@[  %a@]@\n output@\n@[  %a@]@\n time: %.4fs"
      pp_stmt stmt
      Location.pp_range stmt.srange
      pp_zone zone
      (Flow.print man) flow
      (Flow.print man) flow'
      (Timing.stop timer)
    ;
    flow'



  (*==========================================================================*)
  (**                   {2 Evaluation of expressions}                         *)
  (*==========================================================================*)

  (** Build the map of [eval] functions *)
  and eval_map =
    (* Add the implicit [* -> *] eval path that uses all domains *)
    let map = EvalMap.singleton
        (any_zone, any_zone)
        [[(any_zone, any_zone, [any_zone, any_zone], Domain.eval (any_zone, any_zone))]]
    in

    debug "eval graph:@\n @[%a@]" Zone.pp_graph eval_graph;

    (* Iterate over the required zone paths of domain Domain *)
    let required = Domain.eval_interface.import in
    required |>
    List.fold_left (fun acc (src, dst) ->
        if EvalMap.mem (src, dst) acc then acc
        else
          begin
            let paths = Zone.find_all_eval_paths src dst eval_graph in
            if List.length paths = 0
            then
              let () = Exceptions.warn "eval for %a not found" pp_zone2 (src, dst) in
              acc
            else
              (* Map each hop to an eval function *)
              let () = debug "eval paths for %a" pp_zone2 (src, dst) in
              let eval_paths = List.mapi (fun i path ->
                  debug " path #%d: %a" i pp_eval_path path;
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
  and eval ?(zone = (any_zone, any_zone)) ?(via=any_zone) (exp: Ast.expr) (flow: Domain.t flow) : (Domain.t, Ast.expr) evl =
    Out.push_action (Eval({e = exp ; zs = zone}));
    debug "eval:@\n expr: @[%a@]@\n loc: @[%a@]@\n zone: %a@\n input:@\n  @[%a@]"
      pp_expr exp
      Location.pp_range exp.erange
      pp_zone2 zone
      (Flow.print man) flow
    ;

    let timer = Timing.start () in

    let ret =
      (* Check whether exp is already in the desired zone *)
      match sat_zone via (snd zone), Zone.eval exp (snd zone) with
      | true, Keep -> Eval.singleton exp flow

      | _, other_action ->
        (* Try available eval paths in sequence *)
        let paths =
          try find_eval_paths_via zone via eval_map
          with Not_found -> Exceptions.panic_at exp.erange "eval for %a not found" pp_zone2 zone
        in
        match eval_over_paths paths exp man flow with
        | Some evl -> evl
        | None ->
          match other_action with
          | Keep -> Eval.singleton exp flow

          | Process ->
            Exceptions.warn_at exp.erange "%a not evaluated" pp_expr exp;
            Eval.singleton exp flow

          | Visit ->
            let open Visitor in
            let parts, builder = split_expr exp in
            match parts with
            | {exprs; stmts = []} ->
              Eval.eval_list exprs (fun exp flow ->
                  match eval_over_paths paths exp man flow with
                  | None ->
                    Exceptions.warn_at exp.erange "%a not evaluated" pp_expr exp;
                    Eval.singleton exp flow

                  | Some evl -> evl
                ) flow |>
              Eval.bind @@ fun exprs flow ->
              let exp = builder {exprs; stmts = []} in
              Eval.singleton exp flow

            | _ -> Eval.singleton exp flow
    in

    Out.push_action (EvalDone({e_res = ret}));
    debug "eval done:@\n expr: @[%a@]@\n loc: @[%a@]@\n zone: %a@\n input:@\n@[  %a@]@\n output: @[  %a@]@\n time: %.4fs"
      pp_expr exp
      Location.pp_range exp.erange
      pp_zone2 zone
      (Flow.print man) flow
      (Eval.print ~pp:pp_expr) ret
      (Timing.stop timer)
    ;
    ret

  and eval_over_paths paths exp man flow =
    match paths with
    | [] -> None
    | path :: tl ->
      (* let p = List.hd path |> (fun (_, _, path, _) -> path) in
       * debug "trying eval %a over path %a" pp_expr exp pp_eval_path p; *)
      match eval_over_path path man exp flow with
      | None -> eval_over_paths tl exp man flow
      | ret -> ret

  and eval_over_path path man exp flow =
    match path with
    | [] -> None

    | [(z1, z2, path, feval)] -> eval_hop z1 z2 feval man exp  flow

    | (z1, z2, path, feval) :: tl ->
      eval_hop z1 z2 feval man exp flow |>
      OptionExt.bind @@
      Eval.bind_opt @@
      eval_over_path tl man

  and eval_hop z1 z2 feval man exp flow =
    (* debug "trying eval %a in hop %a" pp_expr exp pp_zone2 (z1, z2); *)
    match Zone.eval exp z2 with
    | Keep ->
      Eval.singleton exp flow |>
      OptionExt.return

    | other_action ->
      match Cache.eval feval (z1, z2) exp man flow with
      | Some evl -> Some evl
      | None ->
        match other_action with
        | Keep -> assert false

        | Process ->
          (* debug "no answer"; *)
          None

        | Visit ->
          (* debug "visiting %a" pp_expr exp; *)
          let open Visitor in
          let parts, builder = split_expr exp in
          match parts with
          | {exprs; stmts = []} ->
            Eval.eval_list_opt exprs (eval_hop z1 z2 feval man) flow |>
            OptionExt.lift @@ Eval.bind @@ fun exprs flow ->
            let exp' = builder {exprs; stmts = []} in
            (* debug "%a -> %a" pp_expr exp pp_expr exp'; *)
            Eval.singleton exp' flow

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
