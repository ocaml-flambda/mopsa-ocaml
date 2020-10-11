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

open Core.All
open Toplevel
open Format
open Location
open Callstack


(** {2 Debug queries} *)
(** ***************** *)

(* In order to retrieve structured information from the abstract environment,
   each language should implement the queries [Q_debug_variables] and
   [Q_debug_variable_value]:

   - The query [Q_debug_variables] retrieves the variables accessible in
   the current scope.

   - The query [Q_debug_variable_value] retrieves the value of a given
   variable as a [var_value] record, containing a textual representation of
   the value, and a structural encoding of the eventual sub-values.
*)

(** Value of a variable *)
type var_value = {
  var_value: string option;            (** Direct value of the variable *)
  var_value_type : typ;                (** Type of the value *)
  var_sub_value: var_sub_value option; (** Sub-values of the variable *)
}


(** Sub-value of a variable *)
and var_sub_value =
  | Named_sub_value   of (string (** key *) * var_value (** value *)) list
  (** Named sub-values are maps from field names to values *)

  | Indexed_sub_value of var_value list
  (** Indexed sub-values are arrays of values *)


(** Query to retrieve the list of variables in the current scope *)
type ('a,_) query_kind += Q_debug_variables : ('a,var list) query_kind


(** Query to retrieve the value of a given variable *)
type ('a,_) query_kind += Q_debug_variable_value : var -> ('a,var_value) query_kind


(** Compare two var values *)
let rec compare_var_value v1 v2 =
  Compare.compose [
    (fun () -> Compare.option compare v1.var_value v2.var_value);
    (fun () -> Compare.option compare_var_sub_value v1.var_sub_value v2.var_sub_value);
  ]


(** Compare two var sub-values *)
and compare_var_sub_value sv1 sv2 =
  match sv1, sv2 with
  | Named_sub_value m1, Named_sub_value m2 ->
    Compare.list (fun x1 x2 -> Compare.pair compare compare_var_value x1 x2) m1 m2

  | Indexed_sub_value l1, Indexed_sub_value l2 ->
    Compare.list compare_var_value l1 l2

  | _ -> compare sv1 sv2



(** Print a key with its type *)
let pp_key_with_type fmt (k,t) =
  match t with
  | T_any -> pp_print_string fmt k
  | _      ->Format.fprintf fmt "%s : %a" k pp_typ t


(** Print a variable with its type *)
let pp_var_with_type fmt (v,t) =
  match t with
  | T_any -> pp_var fmt v
  | _      ->Format.fprintf fmt "%a : %a" pp_var v pp_typ t


(** Print the value of a variable *)
let rec pp_var_value fmt v =
  pp_print_option (Debug.color_str "blue") fmt v.var_value;
  match v.var_sub_value with
  | None -> ()
  | Some sv -> fprintf fmt "@,@[<v2>  %a@]" pp_var_sub_value sv


(** Print the values of sub-variables *)
and pp_var_sub_value fmt = function
  | Named_sub_value l ->
    fprintf fmt "%a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
         (fun fmt (k,v) -> fprintf fmt "%a = %a" pp_key_with_type (k,v.var_value_type) pp_var_value v)
      ) l

  | Indexed_sub_value l ->
    (* Group consecutive elements with the same value *)
    let rec group_by_value = function
      | []        -> []
      | (i,v)::tl ->
        match group_by_value tl with
        | [] -> [(i,i,v)]
        | (a,b,w)::tl ->
          if compare_var_value v w = 0
          then (i,b,v)::tl
          else (i,i,v)::(a,b,w)::tl
    in
    List.mapi (fun i v -> (i,v)) l |>
    group_by_value |>
    fprintf fmt "%a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
         (fun fmt (i,j,v) ->
            if i = j
            then fprintf fmt "[%d] = %a" i pp_var_value v
            else fprintf fmt "[%d-%d] = %a" i j pp_var_value v
         )
      )


(** {2 Interactive engine} *)
(** ********************** *)

module Make(Toplevel : TOPLEVEL) =
struct

  type t = Toplevel.t

  let debug fmt = Debug.debug ~channel:"framework.engines.interactive" fmt

  (** Catch Ctrl+C interrupts as a Break exception*)
  let () = Sys.catch_break true


  (** {2 Breakpoints} *)
  (** *************** *)

  (** Breakpoint *)
  type breakpoint =
    | B_function of string (** function *)
    (** Break at the beginning of a function  *)

    | B_line of string (** file *) * int (** line *)
    (** Break at line *)


  (** Compare two breakpoints *)
  let compare_breakpoint b1 b2 : int =
    match b1, b2 with
    | B_function(f1), B_function(f2) ->
      compare f1 f2

    | B_line(file1,line1), B_line(file2,line2) ->
      Compare.pair compare compare (file1,line1) (file2,line2)

    | _ -> compare b1 b2


  (** Print a breakpoint *)
  let pp_breakpoint fmt = function
    | B_function f -> Format.fprintf fmt "%s" f
    | B_line(file,line) -> Format.fprintf fmt "%s:%d" file line


  (** Set of breakpoints *)
  module BreakpointSet = SetExt.Make(struct
      type t = breakpoint let
      compare = compare_breakpoint
    end)


  (** Print a set of breakpoints *)
  let pp_breakpoint_set fmt bs =
    Format.fprintf fmt "@[<v>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") pp_breakpoint)
      (BreakpointSet.elements bs)


  (** Test there is a breakpoint at a given program location *)
  let is_range_breakpoint range breakpoints =
    BreakpointSet.exists
      (function B_line(file,line) -> match_range_file file range
                                     && match_range_line line range
              | B_function _ -> false
      ) breakpoints


  (** Test if there is a breakpoint at a given function *)
  let is_function_breakpoint cs breakpoints =
    not (is_empty_callstack cs)
    && ( let call = callstack_top cs in
         BreakpointSet.exists
           (function
             | B_function f -> f = call.call_fun_orig_name
             | B_line _ -> false
           ) breakpoints )


  (** Exception raised when parsing an invalid breakpoint string *)
  exception Invalid_breakpoint_syntax


  (** Parse a breakpoint string *)
  let parse_breakpoint range (s:string) : breakpoint =
    if Str.string_match (Str.regexp "\\(.+\\):\\([0-9]+\\)$") s 0
    then
      let file = Str.matched_group 1 s in
      let line = int_of_string (Str.matched_group 2 s) in
      B_line(file, line) else

    if Str.string_match (Str.regexp "\\([0-9]+\\)$") s 0
    then
      let file = get_range_file range in
      let line = int_of_string (Str.matched_group 1 s) in
      B_line(file, line) else

    if Str.string_match (Str.regexp "\\([^0-9].*\\)$") s 0
    then
      let func = Str.matched_group 1 s in
      B_function(func)

    else raise Invalid_breakpoint_syntax


  (** {2 User commands} *)
  (** ***************** *)

  (** Commands *)
  type command =
    | Break of string
    (** Add a breakpoint *)

    | Continue
    (** Stop at next breakpoint *)

    | Next
    (** Stop at next statement and skip function calls *)

    | Step
    (** Step into function calls  *)

    | Finish
    (** Finish current function *)

    | NextI
    (** Stop at next statement and skip nodes in the interpretation sub-tree *)

    | StepI
    (** Step into interpretation sub-tree  *)

    | PrintVar of string
    (** Print the value of a variable *)

    | Print
    (** Print the current abstract state *)

    | Env
    (** Print the current abstract environment, associated to token T_cur *)

    | Where
    (** Show current program point *)

    | LoadHook of string
    (** Activate a hook *)

    | UnloadHook of string
    (** Deactivate a hook *)

    | Info of info_command
    (** Print extra information *)

    | Backtrace
    (** Print the callstack *)

    | Debug of string
    (** Set debug channels *)

    | Save of string
    (** Save the environment in a file *)


  (** Information sub-commands *)
  and info_command =
    | Alarms
    | Breakpoints
    | Tokens
    | Variables


  (** Print a command *)
  let pp_command fmt = function
    | Break loc   -> Format.fprintf fmt "break %s" loc
    | Continue    -> Format.pp_print_string fmt "continue"
    | Next        -> Format.pp_print_string fmt "next"
    | Finish      -> Format.pp_print_string fmt "finish"
    | NextI       -> Format.pp_print_string fmt "nexti"
    | Step        -> Format.pp_print_string fmt "step"
    | StepI       -> Format.pp_print_string fmt "stepi"
    | PrintVar v  -> Format.fprintf fmt "print %s" v
    | Print       -> Format.pp_print_string fmt "print"
    | Env         -> Format.pp_print_string fmt "env"
    | Where       -> Format.pp_print_string fmt "where"
    | LoadHook h  -> Format.fprintf fmt "hook %s" h
    | UnloadHook h     -> Format.fprintf fmt "unload %s" h
    | Info Alarms      -> Format.fprintf fmt "info alarms"
    | Info Breakpoints -> Format.fprintf fmt "info breakpoints"
    | Info Tokens      -> Format.fprintf fmt "info tokens"
    | Info Variables   -> Format.fprintf fmt "info variables"
    | Backtrace   -> Format.fprintf fmt "backtrace"
    | Debug ch    -> Format.fprintf fmt "debug %s" ch
    | Save file   -> Format.fprintf fmt "save %s" file


  (** Print help message *)
  let print_usage () =
    printf "Available commands:@.";
    printf "  b[reak] <[file:]line> add a breakpoint at a line@.";
    printf "  b[reak] <function>    add a breakpoint at a function@.";
    printf "  c[ontinue]            run until next breakpoint@.";
    printf "  n[ext]                stop at next statement and skip function calls.@.";
    printf "  n[ext]i               stop at next statement and skip nodes in the interpretation sub-tree@.";
    printf "  s[tep]                step into function calls@.";
    printf "  s[tep]i               step into interpretation sub-tree@.";
    printf "  f[inish]              finish current function@.";
    printf "  p[rint] <variable>    print the value of a variable@.";
    printf "  p[rint]               print the abstract state@.";
    printf "  e[nv]                 print the current abstract environment@.";
    printf "  b[ack]t[race]         print the current call stack@.";
    printf "  w[here]               show current program point@.";
    printf "  h[oo] <hook>          activate a hook@.";
    printf "  u[nload] <hook>       deactivate a hook@.";
    printf "  d[ebug] <channels>    set debug channels@.";
    printf "  i[info] a[larms]      print the list of alarms@.";
    printf "  i[info] b[reakpoints] print the list of breakpoints@.";
    printf "  i[info] t[okens]      print the list of flow tokens@.";
    printf "  i[info] v[ariables]   print the list of variables@.";
    printf "  save <file>           save the abstract state in a file@.";
    printf "  help                  print this message@.";
    ()


  (** Print input prompt *)
  let print_prompt () =
    printf "%a %a @?"
      (Debug.color_str "teal") "mopsa"
      (Debug.color_str "green") ">>"


  (** Context of LineEdit library *)
  let linedit_ctx = LineEdit.create_ctx ()


  (** The last entered command *)
  let last_command = ref None


  (** Read a command from user input *)
  let rec read_command () =
    print_prompt ();
    let l =  LineEdit.read_line linedit_ctx |> String.trim in
    let parts = String.split_on_char ' ' l in
    let c = match parts with
      | ["continue" | "c"]   -> Continue
      | ["next"     | "n"]   -> Next
      | ["step"     | "s"]   -> Step
      | ["finish"   | "f"]   -> Finish
      | ["nexti"    |"ni"]   -> NextI
      | ["stepi"    |"si"]   -> StepI
      | ["print"    | "p"]   -> Print
      | ["env"      | "e"]   -> Env
      | ["where"    | "w"]   -> Where
      | ["backtrace"|"bt"]   -> Backtrace
      | ["break"    | "b"; loc] -> Break loc
      | ["print"    | "p"; var] -> PrintVar var

      | ["help" | "h"]   ->
        print_usage ();
        read_command ()


      | ["info" |"i"; "tokens"      | "t"] | ["it"] -> Info Tokens
      | ["info" |"i"; "breakpoints" | "b"] | ["ib"] -> Info Breakpoints
      | ["info" |"i"; "alarms"      | "a"] | ["ia"] -> Info Alarms
      | ["info" |"i"; "variables" | "vars" | "var"  | "v"] | ["iv"] -> Info Variables

      | [""] ->
        ( match !last_command with
          | None ->  read_command ()
          | Some c -> c )

      | ["hook"   | "h"; hook] -> LoadHook hook
      | ["unload" | "u"; hook] -> UnloadHook hook

      | ["debug"  | "d"; channel] -> Debug channel

      | ["save"; file] -> Save file

      | _ ->
        printf "Unknown command %s@." l;
        print_usage ();
        read_command ()
    in
    last_command := Some c;
    c


  (** {2 Actions on the abstract domain} *)
  (** ********************************** *)

  (** Actions on abstract domain *)
  type _ action =
    | Exec : stmt * route -> Toplevel.t post action
    | Eval : expr * route -> Toplevel.t eval action


  (** Get the program location related to an action *)
  let action_range : type a. a action -> range = function
    | Exec(stmt,_) -> stmt.srange
    | Eval(exp,_)  -> exp.erange


  (** Flag to print welcome message at the beginning *)
  let print_welcome = ref true


  (** Print an action *)
  let pp_action : type a. Toplevel.t flow -> formatter -> a action -> unit = fun flow fmt action ->
    if !print_welcome then (
      print_welcome := false;
      fprintf fmt "@.%a@.Type '%a' to get the list of commands.@.@."
        (Debug.bold pp_print_string) "Welcome to Mopsa v1.0!"
        (Debug.bold pp_print_string) "help"
    )
    else (
      fprintf fmt "%a@." (Debug.color "fushia" pp_range) (action_range action);
      match action with
      | Exec(stmt,route) ->
        fprintf fmt "@[<v 4>S[ %a@] ] in %a@."
          pp_stmt stmt
          pp_route route

      | Eval(exp,route) ->
        fprintf fmt "@[<v 4>E[ %a@] : %a ] in %a@."
          pp_expr exp
          pp_typ (etyp exp)
          pp_route route
    )

  (** Check that an action is atomic *)
  let is_atomic_action: type a. a action -> bool = fun action ->
    match action with
    | Exec(stmt,_) -> is_orig_range stmt.srange && is_atomic_stmt stmt
    | Eval _       -> false



  (** {2 Function state automaton} *)
  (** **************************** *)

  (* We use an automaton to track the current position within the
     analyzed function *)

  (** Function states *)
  type fstate =
    | FunStartNonAtomic
    | FunStartAtomic
    | NonAtomic
    | Atomic
    | InsideAtomic of int


  (** Print a function state *)
  let pp_fstate fmt = function
    | FunStartNonAtomic -> Format.pp_print_string fmt "fun-start-non-atomic"
    | FunStartAtomic    -> Format.pp_print_string fmt "fun-start-atomic"
    | NonAtomic         -> Format.pp_print_string fmt "non-atomic"
    | Atomic            -> Format.pp_print_string fmt "atomic"
    | InsideAtomic d    -> Format.fprintf fmt "inside(%d)" d


  (** Change the state of the automaton before executing an action *)
  let next_fstate_on_pre_action action depth = function
    | FunStartNonAtomic ->
      if is_atomic_action action
      then FunStartAtomic
      else FunStartNonAtomic

    | NonAtomic ->
      if is_atomic_action action
      then Atomic
      else NonAtomic

    | FunStartAtomic | Atomic -> InsideAtomic depth

    | InsideAtomic _ as fs -> fs


  (** Change the state of the automaton after executing an action *)
  let next_fstate_on_post_action action depth = function
    | FunStartNonAtomic    -> FunStartNonAtomic
    | NonAtomic            -> NonAtomic
    | FunStartAtomic       -> NonAtomic
    | Atomic               -> NonAtomic
    | InsideAtomic d as fs -> if d = depth then NonAtomic else fs


  (** {2 Global state} *)
  (** **************** *)

  (** Structure of the global state *)
  type state = {
    mutable breakpoints: BreakpointSet.t;
    (** Registered breakpoints *)

    mutable depth: int;
    (** Current depth of the interpretation tree *)

    mutable command: command;
    (** Last entered command *)

    mutable command_depth: int;
    (** Depth of the interpretation tree when the command was issued *)

    mutable command_callstack : callstack;
    (** Callstack when the command was issued *)

    mutable fstack : fstate list;
    (** Stack of function automata *)

    mutable callstack : callstack;
    (** Current call-stack *)
  }


  (** Global state *)
  let state = {
    breakpoints = BreakpointSet.empty;
    command = StepI;
    depth = 0;
    command_depth = 0;
    command_callstack = empty_callstack;
    fstack = [];
    callstack = empty_callstack;
  }


  (** Print the global state *)
  let pp_state fmt () =
    Format.fprintf fmt "@[<v>breakpoints: @[%a@]@,\
                        depth: %d@,\
                        command: @[%a@]@,\
                        command_depth: %d@,\
                        fstack: @[<v>%a@]@,\
                        callstack: @[%a@]@,\
                        command_callstack: @[%a@]@]"
      pp_breakpoint_set state.breakpoints
      state.depth
      pp_command state.command
      state.command_depth
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") pp_fstate) state.fstack
      pp_callstack state.callstack
      pp_callstack state.command_callstack


  (** Detect if we are in a new call *)
  let detect_call action prev cur =
    callstack_begins_with cur prev


  (** Detect if we returned from a call *)
  let detect_return action prev cur =
    callstack_begins_with prev cur


  (** Change the state when an action is going to be executed *)
  let on_pre_action action flow : unit =
    let cs = Flow.get_callstack flow in
    let prev_depth = state.depth in
    let prev_cs = state.callstack in
    state.depth <- state.depth + 1;
    state.callstack <- cs;
    if detect_call action prev_cs cs then
      let fs0 = if is_atomic_action action then FunStartAtomic else FunStartNonAtomic in
      let fs = next_fstate_on_pre_action action prev_depth fs0 in
      state.fstack <- fs :: state.fstack
    else
    if detect_return action prev_cs cs then
      let fs,tl =
        match state.fstack with
        | _::hd::tl -> hd,tl
        | _         -> assert false
      in
      let fs' = next_fstate_on_pre_action action prev_depth fs in
      state.fstack <- fs' :: tl
    else
      let fs,tl =
        match state.fstack with
        | hd::tl -> hd,tl
        | []     -> NonAtomic,[]
      in
      let fs' = next_fstate_on_pre_action action prev_depth fs in
      state.fstack <- fs' :: tl

  (** Change the state when an action was executed *)
  let on_post_action action : unit =
    let fs,tl =
      match state.fstack with
      | hd::tl -> hd,tl
      | _ -> assert false
    in
    let fs' = next_fstate_on_post_action action state.depth fs in
    state.fstack <- fs' :: tl;
    state.depth <- state.depth - 1


  (** Check if we are at a breakpoint *)
  let is_breakpoint action =
    let fs = List.hd state.fstack in
    ( (fs = Atomic || fs = FunStartAtomic)
      && is_range_breakpoint (action_range action) state.breakpoints)
    || ( fs = FunStartAtomic
         && is_function_breakpoint state.callstack state.breakpoints )


  (** Check if we are at an interaction point *)
  let is_interaction_point action =
    match state.command with
    | StepI -> true

    | NextI -> state.depth <= state.command_depth

    | Step | Next | Continue | Finish when not (is_atomic_action action) -> false

    | Step | Next | Continue | Finish when is_breakpoint action -> true

    | Step ->
      let fs = List.hd state.fstack in
      fs = Atomic || fs = FunStartAtomic

    | Next ->
      let fs = List.hd state.fstack in
      fs = Atomic
      && not (callstack_begins_with state.callstack state.command_callstack)

    | Continue -> false

    | Finish ->
      let fs = List.hd state.fstack in
      fs = Atomic
      && state.command_callstack <> []
      && not (callstack_begins_with state.callstack (pop_callstack state.command_callstack |> snd))

    | _ -> false


  (** {2 Interactive engine} *)
  (** ********************** *)

  (** Apply an action on a flow and return its result *)
  let rec apply_action : type a. a action -> Toplevel.t flow -> a =
    fun action flow ->
    match action with
    | Exec(stmt, route) -> Toplevel.exec ~route stmt man flow
    | Eval(exp, route)  -> Toplevel.eval ~route exp man flow


  (** Wait for user input and process it *)
  and interact: type a. a action -> Toplevel.t flow -> a = fun action flow ->
    let cmd = try read_command ()
              with Exit -> exit 0
    in
    state.command <- cmd;
    state.command_depth <- state.depth;
    state.command_callstack <- (Flow.get_callstack flow);
    let range = action_range action in
    match cmd with
    | Break loc ->
      let () =
        try
          let b = parse_breakpoint range loc in
          state.breakpoints <- BreakpointSet.add b state.breakpoints;
        with
        | Invalid_breakpoint_syntax ->
          printf "Invalid breakpoint syntax@."
      in
      interact action flow

    | Continue | Next | NextI | Step | StepI | Finish ->
      apply_action action flow

    | Backtrace ->
      let cs = Flow.get_callstack flow in
      printf "%a@." pp_callstack cs;
      interact action flow

    | Print ->
      printf "%a@." (format (Flow.print man.lattice.print)) flow;
      interact action flow

    | Env ->
      let env = Flow.get T_cur man.lattice flow in
      printf "%a@." (Print.format man.lattice.print) env;
      interact action flow

    | Where ->
      pp_action flow std_formatter action;
      interact action flow

    | LoadHook hook ->
      if not (Hook.mem_hook hook) then (
        printf "Hook '%s' not found@." hook;
        interact action flow
      ) else (
        Hook.activate_hook hook;
        let ctx = Hook.init_hook hook (Flow.get_ctx flow) in
        let flow = Flow.set_ctx ctx flow in
        interact action flow
      )

    | UnloadHook hook ->
      if not (Hook.mem_hook hook) then (
        printf "Hook '%s' not found@." hook;
        interact action flow
      ) else (
        Hook.deactivate_hook hook man flow;
        interact action flow
      )

    | Info Tokens ->
      let tokens = Flow.fold (fun acc tk _ -> tk::acc) [] flow in
      printf "%a@." (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_token) tokens;
      interact action flow

    | Info Alarms ->
      let report = Flow.get_report flow in
      begin
        if Alarm.is_safe_report report then
          printf "No alarm@."
        else (
          let errors,warnings = Alarm.count_alarms report in
          let nb = errors+warnings in
          printf "%d alarm%a found:@." nb Debug.plurial_int nb;
          Alarm.diagnostics_of_report report |>
          DiagnosticSet.filter (fun d -> d.diag_kind = Error || d.diag_kind = Warning) |>
          Alarm.group_diagnostics_by_check |>
          Alarm.CheckMap.iter (fun check ds ->
              printf "  %a: %d@." Alarm.pp_check check (DiagnosticSet.cardinal ds)
            )
        )
      end;
      interact action flow

    | Info Breakpoints ->
      printf "%a@." pp_breakpoint_set state.breakpoints;
      interact action flow

    | Info Variables ->
      let vars = man.ask (mk_query Q_debug_variables) flow in
      printf "@[<v>%a@]@."
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt v -> pp_var_with_type fmt (v,v.vtyp))
        ) vars
      ;
      interact action flow

    | PrintVar vname ->
      let vars = man.ask (mk_query Q_debug_variables) flow in
      begin try
          let var = List.find
              (fun var' ->
                 let vname' = Format.asprintf "%a" pp_var var' in
                 vname = vname'
              ) vars
          in
          let value = man.ask (mk_query (Q_debug_variable_value var)) flow in
          printf "%a = %a@." pp_var_with_type (var,value.var_value_type) pp_var_value value;
          interact action flow
        with Not_found ->
          printf "Variable '%s' not found@." vname;
          interact action flow
      end

    | Debug channel ->
      Debug.set_channels channel;
      interact action flow

    | Save file ->
      let ch = open_out file in
      let file_fmt = formatter_of_out_channel ch in
      Format.kasprintf (fun str ->
          Format.fprintf file_fmt "%s%!" str
        )  "%a" (format (Flow.print man.lattice.print)) flow;
      close_out ch;
      interact action flow


  (** Interact with the user input or apply the action *)
  and interact_or_apply_action : type a. a action -> Location.range -> Toplevel.t flow -> a =
    fun action range flow ->
    try
      on_pre_action action flow;
      let ret =
        if is_interaction_point action then (
          pp_action flow std_formatter action;
          interact action flow
        ) else
          apply_action action flow
      in
      on_post_action action;
      ret
    with Sys.Break ->
      interact action flow


  and init prog =
    Toplevel.init prog man

  and exec ?(route=toplevel) stmt flow =
    interact_or_apply_action (Exec (stmt, route)) stmt.srange flow

  and eval ?(route=toplevel)exp flow =
    interact_or_apply_action (Eval (exp, route)) exp.erange flow

  and ask : type r. ?route:route -> (Toplevel.t,r) query -> Toplevel.t flow -> r =
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
