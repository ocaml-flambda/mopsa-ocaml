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

(** An abstraction is the encapsulation of the overall abstract domain used by
    the analyzer.

    There are three main differences with domains. First, maps of zoned
    transfer functions are constructed at creation. Second, transfer functions
    are not partial functions and return always a result. Finally, post
    conditions of statements are merged into flows.
*)

open Token
open Context
open Ast.All
open Lattice
open Flow
open Eval
open Post
open Zone
open Query
open Log


type ('a, 't) man = ('a, 't) Sig.Domain.Lowlevel.man = {
  (* Lattice operators over global abstract elements ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Analyzer transfer functions *)
  post : ?zone:zone -> stmt -> 'a flow -> 'a post;
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> (expr, 'a) eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;

  (** Accessors to the domain's merging logs *)
  get_log : log -> log;
  set_log : log -> log -> log;
}


(** Signature of an encapsulated abstraction *)
module type ABSTRACTION =
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



  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> (t, t) man -> t flow

  val exec : ?zone:zone -> stmt -> (t, t) man -> t flow -> t flow

  val post : ?zone:zone -> stmt -> (t, t) man -> t flow -> t post

  val eval : ?zone:(zone * zone) -> ?via:zone -> expr -> (t, t) man -> t flow -> (expr, t) eval

  val ask  : 'r Query.query -> (t, t) man -> t flow -> 'r

end


(*==========================================================================*)
(**             {2 Domain encapsulation into an abstraction}                *)
(*==========================================================================*)


let debug fmt = Debug.debug ~channel:"framework.core.abstraction" fmt


(** Encapsulate a domain into an abstraction *)
module Make(Domain:Sig.Domain.Lowlevel.DOMAIN)
  : ABSTRACTION with type t = Domain.t
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

  let subset man a a' = Domain.subset man a a'

  let join man a a' = Domain.join man a a'

  let meet man a a' = Domain.meet man a a'

  let widen man ctx a a' = Domain.widen man ctx a a'


  (** {2 Caches and zone maps} *)
  (** ************************ *)

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


  (** {2 Initialization} *)
  (** ****************** *)

  let init prog man : Domain.t flow =
    (* Initialize the context with an empty callstack *)
    let ctx = Context.empty |>
              Context.add_unit Callstack.ctx_key Callstack.empty
    in

    (* The initial flow is a singleton ⊤ environment *)
    let flow0 = Flow.singleton ctx T_cur man.lattice.top in
    debug "flow0 = %a" (Flow.print man.lattice) flow0;

    (* Initialize domains *)
    Domain.init prog man flow0


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
    Debug_tree.reach stmt.srange;
    Debug_tree.exec stmt zone man.lattice flow;

    let timer = Timing.start () in
    let fexec =
      try ExecMap.find zone exec_map
      with Not_found -> Exceptions.panic_at stmt.srange "exec for %a not found" pp_zone zone
    in
    let post = Cache.exec fexec zone stmt man flow in

    Debug_tree.exec_done stmt zone (Timing.stop timer) man.lattice post;
    post

  let exec ?(zone = any_zone) (stmt: stmt) man (flow: Domain.t flow) : Domain.t flow =
    let post = post ~zone stmt man flow in
    Post.to_flow man.lattice post



  (** {2 Evaluation of expressions} *)
  (** ***************************** *)

  let eval_graph = Zone.build_eval_graph Domain.interface.ieval.provides

  (** Build the map of [eval] functions *)
  let eval_map =
    (* Add the implicit [* -> *] eval path that uses all domains *)
    let map = EvalMap.singleton
        (any_zone, any_zone)
        [[(any_zone, any_zone, [any_zone, any_zone], Domain.eval (any_zone, any_zone))]]
    in

    debug "eval graph:@\n @[%a@]" Zone.pp_graph eval_graph;

    (* Iterate over the required zone paths of domain Domain *)
    let required = Domain.interface.ieval.uses in
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
  let rec eval ?(zone = (any_zone, any_zone)) ?(via=any_zone) exp man flow =
    Debug_tree.eval exp zone man.lattice flow;
    let timer = Timing.start () in

    let ret =
      (* Check whether exp is already in the desired zone *)
      match sat_zone via (snd zone), Zone.eval_template exp (snd zone) with
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
            let open Ast.Visitor in
            let parts, builder = split_expr exp in
            match parts with
            | {exprs; stmts = []} ->
              Eval.eval_list
                (fun exp flow ->
                   match eval_over_paths paths exp man flow with
                   | None ->
                     Exceptions.warn_at exp.erange "%a not evaluated" pp_expr exp;
                     Eval.singleton exp flow

                   | Some evl -> evl
                )
                exprs flow
              |>
              Eval.bind @@ fun exprs flow ->
              let exp = builder {exprs; stmts = []} in
              Eval.singleton exp flow

            | _ -> Eval.singleton exp flow
    in

    Debug_tree.eval_done exp zone (Timing.stop timer) ret;
    ret

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
      Option.bind @@
      Eval.bind_opt @@
      eval_over_path tl man

  and eval_hop z1 z2 feval man exp flow =
    debug "trying eval %a in hop %a" pp_expr exp pp_zone2 (z1, z2);
    match Zone.eval_template exp z2 with
    | Keep ->
      Eval.singleton exp flow |>
      Option.return

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
          debug "visiting %a" pp_expr exp;
          let open Ast.Visitor in
          let parts, builder = split_expr exp in
          match parts with
          | {exprs; stmts = []} ->
            Eval.eval_list_opt (eval_hop z1 z2 feval man) exprs flow |>
            Option.lift @@ Eval.bind @@ fun exprs flow ->
            let exp' = builder {exprs; stmts = []} in
            debug "%a -> %a" pp_expr exp pp_expr exp';
            Eval.singleton exp' flow

          | _ -> None

  (* Filter paths that pass through [via] zone *)
  and find_eval_paths_via (src, dst) via map =
    let paths = EvalMap.find (src, dst) map in
    List.filter (fun path ->
        List.exists (fun (z1, z2, _, _) -> Zone.sat_zone z1 via || Zone.sat_zone z2 via) path
      ) paths


  (** {2 Handler of queries} *)
  (** ********************** *)

  let ask : type r. r Query.query -> (t,t) man -> t flow -> r =
    fun query man flow ->
      match Domain.ask query man flow with
      | None -> Exceptions.panic "query not handled"
      | Some r -> r



end
