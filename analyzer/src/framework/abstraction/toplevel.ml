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
open Combiners.Domain.Topology
open Ast.All


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

  val subset: (t, t, unit) man -> uctx -> t -> t -> bool

  val join: (t, t, unit) man -> uctx -> t -> t -> t

  val meet: (t, t, unit) man -> uctx -> t -> t -> t

  val widen: (t, t, unit) man -> uctx -> t -> t -> t

  val merge : t -> t * log -> t * log -> t


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> (t, t, unit) man -> t flow

  val exec : ?semantic:semantic -> stmt -> (t, t, unit) man -> t flow -> t flow

  val post : ?semantic:semantic -> stmt -> (t, t, unit) man -> t flow -> t post

  val eval : ?semantic:semantic -> expr -> (t, t, unit) man -> t flow -> t eval

  val ask  : ?semantic:semantic -> 'r query -> (t, t, unit) man -> t flow -> 'r

end


(*==========================================================================*)
(**             {2 Domain encapsulation into an abstraction}                *)
(*==========================================================================*)


let debug fmt = Debug.debug ~channel:"framework.core.abstraction" fmt


(** Encapsulate a domain into a top-level abstraction *)
module Make(Domain:STACKED) : TOPLEVEL with type t = Domain.t
=
struct

  (** {2 Abstraction header} *)
  (** ********************** *)

  type t = Domain.t

  let bottom = Domain.bottom

  let top = Domain.top

  let is_bottom = Domain.is_bottom

  let print = Domain.print

  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset man ctx a a' =
    let b, (), () = Domain.subset man ctx (a,()) (a',()) in
    b

  let join man ctx a a' =
    let a, (), () = Domain.join man ctx (a,()) (a',()) in
    a

  let meet man ctx a a' =
    let a, (), () = Domain.meet man ctx (a,()) (a',()) in
    a

  let widen man ctx a a' =
    let a, (), (), _ = Domain.widen man ctx (a,()) (a',()) in
    a

  let merge = Domain.merge


  (** {2 Caches and zone maps} *)
  (** ************************ *)

  (* Cache of previous evaluations and post-conditions *)
  module Cache = Core.Cache.Make(struct type t = Domain.t end)


  (** Map giving the [exec] transfer function of a zone *)
  module ExecMap = MapExt.Make(struct
      type t = string
      let compare = String.compare
    end)


  (** Map giving the [eval] evaluation function of a zone path *)
  module EvalMap = MapExt.Make(struct
      type t = zone * zone
      let compare = compare_zone2
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
    let () = Hook.init Domain.interface in
    let ctx = Hook.init_active_hooks (Flow.get_ctx res) in

    Flow.set_ctx ctx res


  (** {2 Statement execution} *)
  (** *********************** *)

  (** Build the map of exec functions *)
  let exec_map =
    let required = Domain.interface.iexec.uses in
    (* Add implicit import of Z_any *)
    let map = ExecMap.singleton any_zone (Domain.exec any_zone) in
    (* Iterate over the required zones of domain D *)
    required |>
    List.fold_left (fun map zone ->
        if ExecMap.mem zone map
        then map
        else
        if List.exists (fun z -> sat_zone z zone) Domain.interface.iexec.provides
        then
          ExecMap.add zone (Domain.exec zone) map
        else
          let () = Exceptions.warn "exec for %a not found" pp_zone zone in
          map
      ) map

  let post ?(zone = any_zone) (stmt: stmt) man (flow: Domain.t flow) : Domain.t post =
    let ctx = Hook.on_before_exec zone stmt man flow in
    let flow = Flow.set_ctx ctx flow in

    let fexec =
      try ExecMap.find zone exec_map
      with Not_found -> Exceptions.panic_at stmt.srange "exec for %a not found" pp_zone zone
    in
    try
      let post = Cache.exec fexec zone stmt man flow in
      let ctx = Hook.on_after_exec zone stmt man flow post in
      Post.set_ctx ctx post
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


  let exec ?(zone = any_zone) (stmt: stmt) man (flow: Domain.t flow) : Domain.t flow =
    let post = post ~zone stmt man flow in
    post_to_flow man post


  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  let eval_graph = Core.Zone.build_eval_graph Domain.interface.ieval.provides

  (** Build the map of [eval] functions *)
  let eval_map =
    (* Add the implicit [* -> *] eval path that uses all domains *)
    let map = EvalMap.singleton
        (any_zone, any_zone)
        [[(any_zone, any_zone, [any_zone, any_zone], Domain.eval (any_zone, any_zone))]]
    in

    debug "eval graph:@\n @[%a@]" pp_graph eval_graph;

    (* Iterate over the required zone paths of domain Domain *)
    let required = Domain.interface.ieval.uses in
    required |>
    List.fold_left (fun acc (src, dst) ->
        if EvalMap.mem (src, dst) acc then acc
        else
          begin
            let paths = find_all_eval_paths src dst eval_graph in
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
  let rec eval ?(zone = (any_zone, any_zone)) ?(via=any_zone) exp man flow =
    let ctx = Hook.on_before_eval zone exp man flow in
    let flow = Flow.set_ctx ctx flow in

    let ret =
      (* Check whether exp is already in the desired zone *)
      match sat_zone via (snd zone), eval_template exp (snd zone) with
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
            if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
            then Eval.empty_singleton flow
            else Exceptions.panic_at exp.erange
                "unable to evaluate %a in zone %a"
                pp_expr exp
                pp_zone2 zone


          | Visit ->
            let open Ast.Visitor in
            let parts, builder = structure_of_expr exp in
            match parts with
            | {exprs; stmts = []} ->
              Cases.bind_list exprs
                (fun exp flow ->
                   match eval_over_paths paths exp man flow with
                   | None ->
                     if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
                     then Eval.empty_singleton flow
                     else Exceptions.panic_at exp.erange
                         "unable to evaluate %a in zone %a"
                         pp_expr exp
                         pp_zone2 zone

                   | Some evl -> evl
                ) flow |>
              bind_some @@ fun exprs flow ->
              let exp = builder {exprs; stmts = []} in
              Eval.singleton exp flow

            | _ ->
              if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
              then Eval.empty_singleton flow
              else Exceptions.panic_at exp.erange
                  "unable to evaluate %a in zone %a"
                  pp_expr exp
                  pp_zone2 zone

    in
    let ctx = Hook.on_after_eval zone exp man flow ret in
    Eval.set_ctx ctx ret


  and eval_over_paths paths exp man flow =
    match paths with
    | [] -> None
    | path :: tl ->
      debug "trying eval %a over path %a"
        pp_expr exp
        pp_eval_path (List.hd path |> (fun (_, _, path, _) -> path))
      ;
      match eval_over_path path man exp flow with
      | None -> eval_over_paths tl exp man flow
      | ret -> ret

  and eval_over_path path man exp flow =
    match path with
    | [] -> None

    | [(z1, z2, path, feval)] -> eval_hop z1 z2 feval man exp  flow

    | (z1, z2, path, feval) :: tl ->
      eval_hop z1 z2 feval man exp flow |>
      OptionExt.bind @@ bind_some_opt @@ fun e flow ->
      eval_over_path tl man e flow

  and eval_hop z1 z2 feval man exp flow =
    debug "trying eval %a in hop %a" pp_expr exp pp_zone2 (z1, z2);
    match eval_template exp z2 with
    | Keep ->
      Eval.singleton exp flow |>
      OptionExt.return

    | other_action ->
      match
        Cache.eval (fun e man flow ->
            match feval e man flow with
            | None -> None
            | Some evl ->
              let evl' = Eval.remove_duplicates man.lattice evl in
              (* Update the eprev field in returned expressions to indicate the
                  previous form of the result *)
              let evl'' = Eval.map (fun ee -> { ee with eprev = Some e }) evl' in
              Some evl''
          ) (z1, z2) exp man flow
      with
      | Some evl -> Some evl
      | None ->
        match other_action with
        | Keep -> assert false


        | Process ->
          (* No answer from domains, so let's try other eval paths, if any. *)
          debug "no answer";
          None

        | Visit ->
          debug "visiting %a" pp_expr exp;
          let open Ast.Visitor in
          let parts, builder = structure_of_expr exp in
          match parts with
          | {exprs; stmts = []} ->
            debug "eval parts of %a" pp_expr exp;
            bind_list_opt exprs (eval_hop z1 z2 feval man) flow |>
            OptionExt.lift @@ bind_some @@ fun exprs flow ->
            let exp' = builder {exprs; stmts = []} in
            debug "%a -> %a" pp_expr exp pp_expr exp';
            (* Update the eprev field in returned expressions to
               indicate the previous form of the result *)
            let exp'' = { exp' with eprev = Some exp } in
            Eval.singleton exp'' flow

          | _ ->
            debug "%a is a leaf expression" pp_expr exp;
            None

  (* Filter paths that pass through [via] zone *)
  and find_eval_paths_via (src, dst) via map =
    let paths = EvalMap.find (src, dst) map in
    List.filter (fun path ->
        List.exists (fun (z1, z2, _, _) -> sat_zone z1 via || sat_zone z2 via) path
      ) paths


  (** {2 Handler of queries} *)
  (** ********************** *)

  let ask : type r. r query -> (t,t,unit) man -> t flow -> r =
    fun query man flow ->
      match Domain.ask query man flow with
      | None -> Exceptions.panic "query not handled"
      | Some r -> r



end
