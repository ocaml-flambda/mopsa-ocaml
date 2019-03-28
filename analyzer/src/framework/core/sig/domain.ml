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

(** Unified domain signature.

    The signature DOMAIN is useful for domains that are not parameterized by
    other domains and that require a full accessing to the analyzer. In other
    words, their concretization function γ, their lattice operators and their
    transfer functions do not depend on other external abstractions.

*)


open Ast
open Program
open Expr
open Stmt
open Context
open Flow
open Manager
open Eval
open Log
open Post
open Zone
open Id
open Interface



(*==========================================================================*)
(**                            {2 Signature}                                *)
(*==========================================================================*)


(** Unified signature of an abstract domain *)
module type DOMAIN =
sig

  (** {2 Declaration header} *)
  (** ********************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t did
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Interface of the domain *)


  (** {2 Lattice special values} *)
  (** ************************** *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Lattice predicates} *)
  (** ********************** *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: uctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)

  val merge: t -> t * log -> t * log -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two post-conditions
      [post1] and [post2] using a common pre-condition [pre] after a fork-join
      trajectory in the abstraction DAG.

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions.
  *)


  (** {2 Pretty printing} *)
  (** ******************* *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t) man -> 'a flow -> 'a flow option
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)

end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let domains : (module DOMAIN) list ref = ref []

let register_domain dom =
  domains := dom :: !domains

let find_domain name =
  List.find (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let names () =
  List.map (fun dom ->
      let module D = (val dom : DOMAIN) in
      D.name
    ) !domains




(*==========================================================================*)
(**                 {2 Encapsulation into abstractions}                     *)
(*==========================================================================*)

(** Encapsulate a domain into an abstraction *)
module MakeAbstraction(Domain:DOMAIN)
  : Abstraction.ABSTRACTION with type t = Domain.t
=
struct

  (* Trivial lifting *)
  type t = Domain.t
  type a
  let bottom = Domain.bottom
  let top = Domain.top
  let is_bottom = Domain.is_bottom
  let subset = Domain.subset
  let join = Domain.join
  let meet = Domain.meet
  let widen = Domain.widen
  let print = Domain.print


  (** Initial environment *)
  let init program man flow =
    match Domain.init program man flow with
    | Some flow' -> flow'
    | None -> flow


  (*========================================================================*)
  (**                   {2 Execution of statements}                         *)
  (*========================================================================*)

  (** Map of zoned transfer functions *)
        (** Map giving the [exec] transfer function of a zone *)
  module ExecMap = MapExt.Make(struct
      type t = zone
      let compare = compare_zone
    end)


  let exec_map =
    let required = Domain.interface.exec.uses in
    (* Add implicit import of Z_any *)
    let map = ExecMap.singleton any_zone (Domain.exec any_zone) in
    (* Iterate over the required zones of domain D *)
    required |>
    List.fold_left (fun map zone ->
        if ExecMap.mem zone map
        then map
        else
          if List.exists (fun z -> sat_zone z zone) Domain.interface.exec.provides
          then
            ExecMap.add zone (Domain.exec zone) map
          else
            let () = Exceptions.warn "exec for %a not found" pp_zone zone in
            map
      ) map

  let exec zone (stmt: stmt) (man:(a,t) man) (flow: a flow) : a flow =
    let fexec =
      try ExecMap.find zone exec_map
      with Not_found -> Exceptions.panic_at stmt.srange "exec for %a not found" pp_zone zone
    in
    match fexec stmt man flow with
    | Some post ->
      Post.to_flow man.lattice post

    | None ->
      Exceptions.panic
        "unable to analyze statement in %a:@\n @[%a@]"
        Location.pp_range stmt.srange
        pp_stmt stmt


  (*========================================================================*)
  (**                   {2 Evaluation of expressions}                       *)
  (*========================================================================*)

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


  let eval_graph = Zone.build_eval_graph Domain.interface.eval.provides


  (** Build the map of [eval] functions *)
  let eval_map =
    (* Add the implicit [* -> *] eval path that uses all domains *)
    let map = EvalMap.singleton
        (any_zone, any_zone)
        [[(any_zone, any_zone, [any_zone, any_zone], Domain.eval (any_zone, any_zone))]]
    in

    debug "eval graph:@\n @[%a@]" Zone.pp_graph eval_graph;

    (* Iterate over the required zone paths of domain Domain *)
    let required = Domain.interface.eval.uses in
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
  let rec eval zone via exp (man:(a,t) man) flow =
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
          let open Visitor in
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
      match feval exp man flow with
      | Some evl -> Some evl
      | None ->
        match other_action with
        | Keep -> assert false

        | Process ->
          debug "no answer";
          None

        | Visit ->
          debug "visiting %a" pp_expr exp;
          let open Visitor in
          let parts, builder = split_expr exp in
          match parts with
          | {exprs; stmts = []} ->
            Eval.eval_list_opt (eval_hop z1 z2 feval man) exprs flow |>
            Option.lift @@
            Eval.bind @@ fun exprs flow ->
            let exp' = builder {exprs; stmts = []} in
            debug "%a -> %a" pp_expr exp pp_expr exp';
            Eval.singleton exp' flow

          | _ -> None




  (** Query handler. *)
  let ask query man flow =
    match Domain.ask query man flow with
    | Some r -> r
    | None ->
      Exceptions.panic "query not handled by domain %s" Domain.name





end
