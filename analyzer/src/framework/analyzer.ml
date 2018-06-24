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

let debug fmt = Debug.debug ~channel:"framework.analyzer" fmt

let mk_exec_of_zone_list (l: Zone.t list) exec =
  (fun (stmt: Ast.stmt) (man: ('a, 't) manager) ctx (flow: 'a flow) : 'a Post.post option ->
     let rec aux =
       function
       | [] -> None
       | z :: tl ->
         match exec z stmt man ctx flow with
         | None -> aux tl
         | Some ret -> Some ret
     in
     aux l
  )

let mk_eval_of_zone_path_list (l: Zone.path list) eval =
  (fun (exp: Ast.expr) (man: ('a, 't) manager) ctx (flow: 'a flow) : (Ast.expr, 'a) eval option ->
     let rec aux =
       function
       | [] -> None
       | p :: tl ->
         match eval p exp man ctx flow with
         | None -> aux tl
         | Some ret -> Some ret
     in
     aux l
  )


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

  let flow_manager = flow_of_lattice_manager env_manager

  (** Top-level accessor. *)
  let ax = {
    get = (fun gabs -> gabs);
    set = (fun gabs' gabs -> gabs');
  }


  (** Map giving the [exec] transfer function of a zone *)
  module ExecMap = MapExt.Make(struct type t = Zone.t let compare = compare end)


  (** Map giving the [eval] evaluation function of a zone path *)
  module EvalMap = MapExt.Make(struct type t = Zone.path let compare = Zone.compare_path end)


  let exec_cache = ref []


  let eval_cache = ref []


  (*==========================================================================*)
  (**                        {2 Initialization}                               *)
  (*==========================================================================*)

  let rec init prog =
    let flow = flow_manager.bottom |>
               flow_manager.set TCur env_manager.top
    in
    match Domain.init prog manager Context.empty flow with
    | None -> Context.empty, flow
    | Some (ctx, flow) -> ctx, flow



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
            match List.find_all (fun z -> Zone.leq z zone) Domain.exec_interface.export with
            | [] -> Debug.fail "exec for %a not found" Zone.print zone

            | l ->
              let f = mk_exec_of_zone_list l Domain.exec in

              debug "exec for %a found" Zone.print zone;
              ExecMap.add zone f acc
          end
      ) ExecMap.empty (Zone.top :: Domain.exec_interface.import)


  and exec ?(zone = Zone.top) (stmt: Ast.stmt) (ctx: Context.context) (flow: Domain.t flow) : Domain.t flow =
    debug
      "exec stmt in %a:@\n @[%a@]@\n input:@\n  @[%a@]"
      Location.pp_range_verbose stmt.srange
      pp_stmt stmt manager.flow.print flow
    ;
    let timer = Timing.start () in

    let zone_exec = ExecMap.find zone exec_map in

    use_cache
      (zone, ctx, stmt, flow) exec_cache
      ~otherwise:(fun () ->
          match zone_exec stmt manager ctx flow with
          | None ->
            Exceptions.panic
              "Unable to analyze statement in %a:@\n @[%a@]"
              Location.pp_range_verbose stmt.srange
              pp_stmt stmt

          | Some post -> post.flow
        )
    |>
    (fun res ->
       let t = Timing.stop timer in
       Debug.debug ~channel:"framework.analyzer.profiler"
         "exec done in %.6fs of:@\n@[<v>  %a@]"
         t pp_stmt stmt
       ;
       debug
         "exec stmt done:@\n @[%a@]@\n input:@\n@[  %a@]@\n output@\n@[  %a@]"
         pp_stmt stmt manager.flow.print flow manager.flow.print res
       ;
       res
    )




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
            debug "Searching for eval function for the zone path %a" Zone.print_path zpath;
            match List.find_all (fun p -> Zone.path_leq p zpath) Domain.eval_interface.export with
            | [] -> Debug.fail "eval for %a not found" Zone.print_path zpath

            | l ->
              let f = mk_eval_of_zone_path_list l Domain.eval in
              debug "eval for %a found" Zone.print_path zpath;
              EvalMap.add zpath f acc

          end
      ) EvalMap.empty (Zone.path_top :: Domain.eval_interface.import)


  (** Evaluation of expressions. *)
  and eval ?(zpath = Zone.path_top) (exp: Ast.expr) (ctx: Context.context) (flow: Domain.t flow) : (Ast.expr, Domain.t) eval =
    debug
      "eval expr in %a:@\n @[%a@]@\n input:@\n  @[%a@]"
      Location.pp_range_verbose exp.erange
      pp_expr exp manager.flow.print flow
    ;
    let timer = Timing.start () in
    let path_eval = EvalMap.find zpath eval_map in
    use_cache (zpath, ctx, exp, flow)
      eval_cache
      ~otherwise:(fun () ->
         match path_eval exp manager ctx flow with
           | Some evl ->
             Eval.iter_ (fun e flow ->
                 add_to_cache (zpath, ctx, e, flow) (Eval.case (Some e) flow) eval_cache;
               ) evl;
             evl
           | None -> Eval.case (Some exp) flow
        )
    |>
    (fun res ->
       let t = Timing.stop timer in
       Debug.debug
         ~channel:"framework.analyzer.profiler"
         "eval done in %.6fs of @[%a@]"
         t pp_expr exp
       ;
       debug
         "eval expr done:@\n @[%a@]@\n input:@\n@[  %a@]@\n output@\n@[  %a@]"
         pp_expr exp
         manager.flow.print flow
         (Eval.print ~pp:pp_expr) res
       ;
       res
    )


  (** Query handler. *)
  and ask : type r. r Query.query -> _ -> _ -> r option =
    fun query ctx flow -> Domain.ask query manager ctx flow


  and use_cache : type a b. a -> (a * b) list ref -> otherwise:(unit -> b) -> b =
    fun k cache ~otherwise ->
    if Options.(common_options.cache_size) == 0 then
      otherwise ()
    else
      try
        let res = List.assoc k !cache in
        debug "from cache";
        res
      with Not_found ->
        let res = otherwise () in
        add_to_cache k res cache;
        res

  and add_to_cache : type a b. a -> b -> (a * b) list ref -> unit =
    fun k v cache ->
      cache := (k, v) :: (
          if List.length !cache < Options.(common_options.cache_size) then !cache
          else List.rev @@ List.tl @@ List.rev !cache
        )


  (** Top level manager *)

  and manager = {
    env = env_manager;
    flow = flow_manager;
    exec = exec;
    eval = eval;
    ask = ask;
    ax = ax;
  }

end
