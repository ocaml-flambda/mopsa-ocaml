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

(** Engine for interactive analysis sessions *)

open Mopsa_utils
open Core.All
open Toplevel
open Format
open Location
open Callstack
open Breakpoint
open Query
module I = Interface
open Interface
open Action
open Envdb
open Trace


(** {2 Interactive engine} *)
(** ********************** *)

module Make(Toplevel : TOPLEVEL)(InterfaceFunctor : I.INTERFACE) =
struct

  module Interface = InterfaceFunctor(Toplevel)

  type t = Toplevel.t

  let debug fmt = Debug.debug ~channel:"framework.engines.interactive" fmt

  (** Catch Ctrl+C interrupts as a Break exception*)
  let () = Sys.catch_break true


  (** {2 Actions on the abstract domain} *)
  (** ********************************** *)

  (** Actions on abstract domain *)
  type _ return_action =
    | Exec : stmt * route -> Toplevel.t post return_action
    | Eval : expr * route * semantic -> Toplevel.t eval return_action

  (* Actions with hidden return type *)
  type xaction = Action : 'a return_action -> xaction


  (** Get the program location related to an action *)
  let action_range : type a. a return_action -> range = function
    | Exec(stmt,_)    -> stmt.srange
    | Eval(exp,_,_) -> exp.erange


  let interface_action (type a) (action: a return_action) : Action.action =
    match action with
    | Exec(stmt, route) -> Exec(stmt, route)
    | Eval(expr, route, tran) -> Eval(expr, route, tran)

  (** {2 Interaction detection} *)
  (** ************************* *)

  (* Test if the currest state corresponds to a function call *)
  let is_call old cur =
    callstack_begins_with cur old

  (* Test if the currest state corresponds to a function return *)
  let is_return old cur =
    callstack_begins_with old cur

  (* Test if an action corresponds to a new line of code *)
  let is_new_loc_action : type a. state -> a return_action -> bool = fun old action ->
    match action with
    | Eval _ -> false
    | Exec({skind = S_block _},_) -> false
    | Exec(stmt,_) ->
      let range = untag_range stmt.srange in
      is_orig_range range &&
      ( match old.loc with
        | None        -> true
        | Some range' ->
          let p = get_range_start range in
          let p' = get_range_start range' in
          p.pos_file <> p'.pos_file ||
          p.pos_line <> p'.pos_line )


  (* Check if the analyzer reached an interaction point *)
  let is_range_breakpoint () =
    match state.loc with
    | None -> false
    | Some range ->
      BreakpointSet.exists
        (function
          | B_line(file,line) -> match_range_file file range
                                 && match_range_line line range
          | _ -> false
        ) !breakpoints


  (** Test if there is a breakpoint at a given function *)
  let is_function_breakpoint () =
    not (is_empty_callstack state.callstack)
    && ( let call = callstack_top state.callstack in
         BreakpointSet.exists
           (function
             | B_function f -> f = call.call_fun_orig_name
             | _ -> false
           ) !breakpoints )

  let is_named_breakpoint name =
    BreakpointSet.exists
      (function
        | B_named name' -> name = name'
        | _ -> false
      ) !breakpoints

  let is_alarm_breakpoint_active () =
    BreakpointSet.mem B_alarm !breakpoints

  (* Check if the analyzer reached an interaction point *)
  let is_interaction_point (type a) old (action: a return_action) =
    match action with
    | Exec({skind = S_breakpoint name}, _) when is_named_breakpoint name ->
      true

    | _ ->
      match state.command with
      (* Always interact with [StepI] *)
      | StepI -> true

      (* [NextI] stops only if the current depth is less than
         the depth when the user entered the [NextI] command *)
      | NextI ->
        state.depth <= state.command_depth

      (* [Step] stops at any new line of codes *)
      | Step ->
        is_new_loc_action old action

      (* [Next] stops at new lines of codes that are not in inner calls (unless if
         there is a breakpoint) *)
      | Next ->
        is_new_loc_action old action &&
        ( not (is_call state.command_callstack state.callstack) ||
          is_range_breakpoint () ||
          ( state.call_preamble && is_function_breakpoint () ) )

      (* [Continue] stops at new lines of code with attached breakpoints *)
      | Continue ->
        is_new_loc_action old action &&
        ( is_range_breakpoint () ||
          ( state.call_preamble && is_function_breakpoint () ) )

      (* [Finish] stops at new lines of code after function return or at breakpoints *)
      | Finish ->
        is_new_loc_action old action &&
        ( is_return state.command_callstack state.callstack ||
          is_range_breakpoint () ||
          is_function_breakpoint () )

      | Backward -> assert false

  let get_new_alarms (type a) (action: a return_action) (ret:a) =
    let doit cases =
      let alarms =
        Cases.fold
          (fun acc _ flow ->
             let report = Flow.get_report flow in
             fold_report
               (fun diag acc ->
                  match diag.diag_kind with
                  | Error | Warning    -> AlarmSet.union diag.diag_alarms acc
                  | Safe | Unreachable -> acc
               ) report acc
          ) AlarmSet.empty cases
      in
      AlarmSet.diff alarms state.alarms
    in
    match action with
    | Exec _ -> doit ret
    | Eval _ -> doit ret

  (*************************)
  (** Environment database *)
  (*************************)

  let envdb : t envdb ref = ref empty_envdb

  (** {2 Interactive engine} *)
  (** ********************** *)

  (** Apply an action on a flow and return its result *)
  let rec apply_action : type a. a return_action -> t flow -> a =
    fun action flow ->
    match action with
    | Exec(stmt, route) ->
      state.depth <- state.depth + 1;
      let ret = Toplevel.exec ~route stmt man flow in
      state.depth <- state.depth - 1;
      ret
    | Eval(exp, route, translate)  ->
      state.depth <- state.depth + 1;
      let ret = Toplevel.eval ~route ~translate exp man flow in
      state.depth <- state.depth - 1;
      ret

(** Wait for user input and process it *)
  and interact: type a. a return_action -> t flow -> a = fun action flow ->
    let cmd = Interface.read_command (interface_action action) !envdb man flow in
    state.command <- cmd;
    state.command_depth <- state.depth;
    state.command_callstack <- (Flow.get_callstack flow);
    match cmd with
    | Backward -> raise Toplevel.GoBackward
    | _        -> apply_action action flow


  (** Interact with the user input or apply the action *)
  and interact_or_apply_action : type a. a return_action -> Location.range -> Toplevel.t flow -> a =
    fun action range flow ->
    let old = copy_state state in
    state.depth <- state.depth + 1;
    state.callstack <- Flow.get_callstack flow;
    try
      (* When entering a function, we push the old loc to locstack, so that we
         can retrieve it when returning from the function to check if we
         encounter a new loc after the call. *)
      ( if is_call old.callstack state.callstack then
          ( state.call_preamble <- true;
            state.locstack <- old.loc :: old.locstack )
        else
        (* When returning from a function, we pop the loc from the stack,
           and we consider it as the old loc *)
        if is_return old.callstack state.callstack then
          ( state.call_preamble <- false;
            match old.locstack with
            | []     ->
              old.loc <- None;
              state.loc <- None
            | hd::tl ->
              old.loc <- hd;
              state.loc <- hd;
              state.locstack <- tl ) );

      let cur = Flow.get T_cur man.lattice flow in

      (* Check if we reached a new loc *)
      let new_loc = is_new_loc_action old action in
      let last_trace_element_id = ref 0 in
      ( if new_loc then
          let range = action_range action in
          state.loc <- Some range;
          state.trace <- begin_trace_element (interface_action action) state.trace;
          last_trace_element_id := get_last_trace_element_id state.trace;
          let ctx = Flow.get_ctx flow in
          envdb := add_envdb (interface_action action) ctx cur man !envdb
      );
      (* Check if we reached an interaction point *)
      let interaction = is_interaction_point old action &&
                        not (man.lattice.is_bottom cur)
      in
      (* Reset call flag if we reached a new loc *)
      if new_loc then state.call_preamble <- false;
      let ret =
        if interaction then (
          Interface.reach (interface_action action) man flow;
          interact action flow
        ) else
          apply_action action flow
      in
      (* Check if there are new alarms *)
      if new_loc then (
        state.trace <- end_trace_element !last_trace_element_id (interface_action action) state.trace;
        let new_alarms = get_new_alarms action ret in
        state.alarms <- AlarmSet.union state.alarms new_alarms;
        if not (AlarmSet.is_empty new_alarms) then (
          Interface.alarm (AlarmSet.elements new_alarms) (interface_action action) man flow;
          if is_alarm_breakpoint_active () then (
            Interface.reach (interface_action action) man flow;
            let _ = interact action flow in
            ()
          )
        );
      );
      (* Return *)
      state.depth <- state.depth - 1;
      ret
    with
    | Toplevel.SysBreak _
    | Sys.Break ->
      interact action flow

    | Toplevel.GoBackward ->
      let cs = Flow.get_callstack flow in
      if is_call cs state.callstack then (
        state.depth <- old.depth + 1;
        state.callstack <- Flow.get_callstack flow;
        state.alarms <- AlarmSet.of_list (Alarm.report_to_alarms (Flow.get_report flow));
        Interface.reach (interface_action action) man flow;
        interact action flow
      ) else
        raise Toplevel.GoBackward


    | Exit -> raise Exit

    | e ->
      Interface.error e;
      Interface.reach (interface_action action) man flow;
      interact action flow

  (** {2 Engine functions} *)
  (** ******************** *)

  and subset a a' =
    state.depth <- state.depth + 1;
    let b = Toplevel.subset man a a' in
    state.depth <- state.depth - 1;
    b

  and join a a' =
    state.depth <- state.depth + 1;
    let aa = Toplevel.join man a a' in
    state.depth <- state.depth - 1;
    aa

  and meet a a' =
    state.depth <- state.depth + 1;
    let aa = Toplevel.meet man a a' in
    state.depth <- state.depth - 1;
    aa

  and widen ctx a a' =
    state.depth <- state.depth + 1;
    let aa = Toplevel.widen man ctx a a' in
    state.depth <- state.depth - 1;
    aa

  and init prog =
    Interface.init ();
    Toplevel.init prog man;

  and analyze stmt flow =
    try
      init_state state;
      let ret =
        (
          exec stmt flow >>% fun flow ->
          exec (mk_stmt (S_block ([], [])) (tag_range stmt.srange "end")) flow
        )
        |> post_to_flow man in
      Interface.finish man ret;
      ret
    with
    | Exit ->
      raise Exit

    | Toplevel.GoBackward ->
      analyze stmt flow

    | e ->
      Interface.error e;
      raise e


  and exec ?(route=toplevel) stmt flow =
    interact_or_apply_action (Exec (stmt, route)) stmt.srange flow

  and eval ?(route=toplevel) ?(translate=any_semantic) ?(translate_when=[]) exp flow =
    interact_or_apply_action (Eval (exp, route, translate)) exp.erange flow

  and ask : type r. ?route:route -> (Toplevel.t,r) query -> Toplevel.t flow -> (Toplevel.t, r) cases =
    fun ?(route=toplevel)query flow ->
      Toplevel.ask query man flow

  and print_expr ?(route=toplevel) flow printer exp =
    Toplevel.print_expr ~route man flow printer exp

  and lattice : Toplevel.t lattice = {
    bottom = Toplevel.bottom;
    top = Toplevel.top;
    is_bottom = Toplevel.is_bottom;
    subset = (fun a a' -> Toplevel.subset man a a');
    join = (fun a a' -> Toplevel.join man a a');
    meet = (fun a a' -> Toplevel.meet man a a');
    widen = (fun ctx a a' -> Toplevel.widen man ctx a a');
    merge = Toplevel.merge;
    print = Toplevel.print_state;
  }

  and man : (Toplevel.t, Toplevel.t) man = {
    lattice;
    get = (fun a -> a);
    set = (fun a _ -> a);
    get_effects = (fun e -> e);
    set_effects = (fun e _ -> e);
    exec = exec;
    eval = eval;
    ask = ask;
    print_expr = print_expr;
  }


end
