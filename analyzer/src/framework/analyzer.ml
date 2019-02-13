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


(** Create an [Analyzer] module over some abstract domain. *)
module Make(Domain : DOMAIN) =
struct


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

  let rec init prog man : Domain.t flow =
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

  and exec ?(zone = any_zone) (stmt: Ast.stmt) man (flow: Domain.t flow) : Domain.t flow =
    Logging.reach (Location.untag_range stmt.srange);
    Logging.exec stmt zone man flow;

    let timer = Timing.start () in
    let fexec =
      try ExecMap.find zone exec_map
      with Not_found -> Exceptions.panic_at stmt.srange "exec for %a not found" pp_zone zone
    in
    let flow' = Cache.exec fexec zone stmt man flow in

    Logging.exec_done stmt zone (Timing.stop timer) man flow';
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
  and eval ?(zone = (any_zone, any_zone)) ?(via=any_zone) (exp: Ast.expr) man (flow: Domain.t flow) : (Domain.t, Ast.expr) evl =
    Logging.eval exp zone man flow;
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

    Logging.eval_done exp zone (Timing.stop timer) ret;
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
    exec = (fun ?(zone=any_zone) stmt flow -> exec ~zone stmt man flow);
    eval = (fun ?(zone=(any_zone, any_zone)) ?(via=any_zone) exp flow -> eval ~zone ~via exp man flow);
    ask = ask;
  }


  (** {2 Interactive mode} *)
  (** ******************** *)

  open Format

  (** Analysis breakpoint *)
  type breakpoint = {
    brk_file: string option;
    brk_line: int;
  }

  (** Compare two breakpoints *)
  let compare_breakpoint brk1 brk2 =
    Compare.compose [
      (fun () -> Compare.option compare brk1.brk_file brk2.brk_file);
      (fun () -> compare brk1.brk_line brk2.brk_line);
    ]

  (** Set of active breakpoints *)
  module BreakpointSet = Set.Make(struct type t = breakpoint let compare = compare_breakpoint end)
  let breakpoints : BreakpointSet.t ref = ref BreakpointSet.empty

  (** Parse a breakpoint string *)
  let parse_breakpoint breakpoint =
    if Str.string_match (Str.regexp "\\(.+\\):\\([0-9]+\\)$") breakpoint 0
    then Some {
      brk_file = Some (Str.matched_group 1 breakpoint);
      brk_line = int_of_string (Str.matched_group 2 breakpoint);
    }

    else if Str.string_match (Str.regexp "\\([0-9]+\\)$") breakpoint 0
    then Some {
      brk_file = None;
      brk_line = int_of_string (Str.matched_group 1 breakpoint);
    }

    else None

  (** Check if a range matches a breakpoint *)
  let match_breakpoint range breakpoint =
    match breakpoint.brk_file with
    | None -> Location.match_range_line breakpoint.brk_line range

    | Some file -> Location.match_range_file file range &&
                   Location.match_range_line breakpoint.brk_line range

  (** Find the breakpoint covering location [range] *)
  let find_breakpoint_at range =
    BreakpointSet.filter (match_breakpoint range) !breakpoints |>
    BreakpointSet.choose_opt

  (* Debugging commands *)
  type command =
    | Run    (** Analyze and stop at next breakpoint *)
    | Next   (** Analyze and stop at next program point in the same level *)
    | Step   (** Step into current program point  *)
    | Print  (** Print the current abstract state *)
    | Env    (** Print the current  abstract environment associated to token T_cur *)
    | Where  (** Show current program point *)


  (** Reference to the last received command *)
  let last_command : command option ref = ref None

  (** Print help message *)
  let print_usage () =
    printf "Available commands:@.";
    printf "  b[reak] <loc>    add a breakpoint at location <loc>@.";
    printf "  r[un]            analyze until next breakpoint@.";
    printf "  n[ext]           analyze until next program point in the same level@.";
    printf "  s[tep]           analyze until next program point in the level beneath@.";
    printf "  p[rint]          print the abstract state@.";
    printf "  e[nv]            print the current abstract environment@.";
    printf "  w[here]          show current program point@.";
    printf "  h[elp]           print this message@.";
    ()

  let print_prompt range () =
    printf "%a >> @?" (Debug.color "blue" Location.pp_range) range

  (** Read a debug command *)
  let rec read_command range () =
    print_prompt range ();
    let l = read_line () in
    let c = match l with
      | "run"   | "r"   -> Run
      | "next"  | "n"   -> Next
      | "step"  | "s"   -> Step
      | "print" | "p"   -> Print
      | "env"   | "e"   -> Env
      | "where" | "w"   -> Where

      | "help"  | "h"   ->
        print_usage ();
        read_command range ()

      | "" -> (
        match !last_command with
          | None ->  read_command range ()
          | Some c -> c
      )

      | _ ->
        if Str.string_match (Str.regexp "\\(b\\|break\\) \\(.*\\)") l 0
        then (
          let loc = Str.matched_group 2 l in
          let () =
            match parse_breakpoint loc with
            | Some brk ->
              breakpoints := BreakpointSet.add brk !breakpoints;

            | None ->
              printf "Invalid breakpoint syntax@."
          in
          read_command range ()
        )
        else (
          printf "Unrecognized command: %s@." l;
          print_usage ();
          read_command range ()
        )
    in
    last_command := Some c;
    c

  (** Breakpoint flag *)
  let break = ref true

  (** Last breakpoint reached *)
  let last_breakpoint = ref {
      brk_file = None;
      brk_line = -1;
    }


  (** Interpreter actions *)
  type _ action =
    | Exec : stmt * zone -> Domain.t flow action
    | Eval : expr * (zone * zone) * zone -> (Domain.t, expr) evl action


  (** Print the current analysis action *)
  let pp_action : type a. formatter -> a action * Domain.t flow -> unit = fun fmt (action, flow) ->
    let () =
      match action with
      | Exec(stmt,zone) ->
        fprintf fmt " @[<v 3>%a @[%a@]@] %a in zone %a@."
          (Debug.color_str "IndianRed") "𝕊 ⟦"
          pp_stmt stmt
          (Debug.color_str "IndianRed") "⟧"
          pp_zone zone

      | Eval(exp,zone,_) ->
        fprintf fmt " @[<v 3>%a %a@] %a in zone %a@."
          (Debug.color_str "IndianRed") "𝔼 ⟦"
          pp_expr exp
          (Debug.color_str "IndianRed") "⟧"
          pp_zone2 zone
    in
    let cs = Callstack.get flow in
    fprintf fmt " @[<hv 2>%a:@  %a@]@."
      (Debug.color_str "Teal") "Call stack"
      Callstack.print cs



  (** Apply an action on a flow and return its result *)
  let rec apply_action : type a. a action -> Domain.t flow -> a =
    fun action flow ->
      match action with
      | Exec(stmt, zone) -> exec ~zone stmt interactive_man flow
      | Eval(exp, zone, via) -> eval ~zone ~via exp interactive_man flow


  (** Interact with the user input *)
  and interact : type a. ?where:bool -> a action -> Location.range -> Domain.t flow -> a =
    fun ?(where=true) action range flow ->
      if not !break then (
        (* Search for a breakpoint at current location *)
        match find_breakpoint_at range with
        | None ->
          (* No breakpoint here *)
          ()

        | Some brk ->
          (* Check if brk is different than the current breakpoint *)
          if compare_breakpoint brk !last_breakpoint != 0 then (
            break := true;
            last_breakpoint := brk;
          )
      );

      if not !break
      then apply_action action flow

      else (
        if where then pp_action std_formatter (action, flow);
        match read_command range () with
        | Run ->
          break := false;
          apply_action action flow

        | Step ->
          break := true;
          apply_action action flow

        | Next ->
          break := false;
          let ret = apply_action action flow in
          break := true;
          ret

        | Print ->
          printf "%a@." (Flow.print interact_man) flow;
          interact ~where:false action range flow

        | Env ->
          let env = Flow.get T_cur interact_man flow in
          printf "%a@." interact_man.print env;
          interact ~where:false action range flow

        | Where ->
          pp_action std_formatter (action, flow);
          interact ~where:false action range flow
      )

  and interactive_exec ?(zone=any_zone) stmt flow =
    interact (Exec (stmt, zone)) stmt.srange flow

  and interactive_eval ?(zone=(any_zone, any_zone)) ?(via=any_zone) exp flow =
    interact (Eval (exp, zone, via)) exp.erange flow

  and interactive_man = {
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
    exec = interactive_exec;
    eval = interactive_eval;
    ask = ask;
  }

end
