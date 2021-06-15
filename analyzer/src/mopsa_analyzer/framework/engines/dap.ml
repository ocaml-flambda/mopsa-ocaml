(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2020 The MOPSA Project.                               *)
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

(** Engine supporting parts of the debug adapter protocol *)

open Mopsa_utils
open Core.All
open Toplevel
open Location
open Callstack
open Format
open Yojson.Basic
open Yojson.Basic.Util
open Interactive.Breakpoint
open Interactive.Query


(** {2 DAP engine} *)
(** ************** *)

module Make(Toplevel : TOPLEVEL) =
struct

  type t = Toplevel.t

  let debug fmt = Debug.debug ~channel:"framework.engines.dap" fmt

  (** {2 DAP communication and JSON managing} *)
  (** ********************* *)

  (** Reading the request from the standard input *)
  let read_json_DAP () =
    let s = input_line stdin in
    assert (Str.string_match (Str.regexp "Content-Length: \\([0-9]+\\)") s 0);
    let content_length = int_of_string (Str.matched_group 1 s) in
    let s = input_line stdin in
    let s = if Str.string_match (Str.regexp "Content-Type:") s 0 then input_line stdin else s in
    assert (s = "\r");
    let obj_bytes = Bytes.create content_length in
    let read_length = input stdin obj_bytes 0 content_length in
    assert (read_length = content_length);
    from_string @@ Bytes.to_string obj_bytes

  (** Answering on stdin *)
  let write_json_DAP obj =
    let obj_str = pretty_to_string obj in
    let obj_str_len = String.length obj_str in
    let response = "Content-Length: " ^ (string_of_int obj_str_len) ^ "\r\n\r\n" ^ obj_str  in
    output_string stdout response;
    flush stdout

  (** Extraction functions for some JSON fields *)
  let extract_command request = request |> member "command" |> to_string
  let extract_seq request = request |> member "seq" |> to_int
  let extract_varref request = request |> member "arguments" |> member "variablesReference" |> to_int
  let extract_path request = request |> member "arguments" |> member "source" |> member "path" |> to_string
  let extract_breakpoints request = request |> member "arguments" |> member "breakpoints" |> to_list
  let extract_line_breakpoint bp = bp |> member "line" |> to_int
  let extract_name x = x |> member "name" |> to_string
  let extract_expression request = request |> member "arguments" |>  member "expression" |> to_string

  let create_response request body :Yojson.Basic.t =
    let command = extract_command request
    and req_seq = extract_seq request
    in
    match body with
    | `Null -> `Assoc [
                   ("seq", `Int 0);
                   ("type", `String "response");
                   ("request_seq", `Int req_seq);
                   ("success", `Bool true);
                   ("command", `String command)
                 ]
    | _     -> `Assoc [
                   ("seq", `Int 0);
                   ("type", `String "response");
                   ("request_seq", `Int req_seq);
                   ("success", `Bool true);
                   ("command", `String command);
                   ("body", body)
                 ]

  let create_event name body :Yojson.Basic.t =
    match body with
    | `Null ->  `Assoc [
                    ("seq", `Int 0);
                    ("type", `String "event");
                    ("event", `String name);
                  ]
    |  _    ->  `Assoc [
                    ("seq", `Int 0);
                    ("type", `String "event");
                    ("event", `String name);
                    ("body", body)
                  ]

  let create_body_stopped reason :Yojson.Basic.t=
    `Assoc [
        ("reason", `String reason);
        ("threadId", `Int 1)
      ]

  let createFrame id name line column file path :Yojson.Basic.t=
    `Assoc [
        ("id", `Int id);
        ("line", `Int line);
        ("column", `Int column);
        ("name", `String name);
        ("source",
         `Assoc [
             ("name", `String file);
             ("path", `String path);
             ("sourceReference", `Int 0)
        ])
      ]

  let createScope name variablesHand expensive :Yojson.Basic.t =
    `Assoc [
        ("name", `String name);
        ("variablesReference", `Int variablesHand);
        ("expensive", `Bool expensive)
      ]

  let createVariable name vartype value varref :Yojson.Basic.t=
    match value with
    | None -> `Assoc [
                  ("name", `String name);
                  ("type", `String vartype);
                  ("value", `String "");
                  ("variablesReference", `Int varref)
                ]
    | Some value -> `Assoc [
                        ("name", `String name);
                        ("type", `String vartype);
                        ("value", `String value);
                        ("variablesReference", `Int varref)
                      ]

  let body_initilize :Yojson.Basic.t =
    `Assoc [
        ("supportsFunctionBreakpoints", `Bool true)
      ]

  let body_threads :Yojson.Basic.t =
    `Assoc [
        ("threads",
         `List [
             `Assoc [
                 ("id", `Int 1);
                 ("name",`String "thread 1")
               ]
        ])
      ]

  let body_stackTrace stack_trace_json :Yojson.Basic.t =
    `Assoc [
        ("totalFrames", `Int (List.length stack_trace_json));
        ("stackFrames", `List stack_trace_json)
      ]

  let body_scopes :Yojson.Basic.t =
    `Assoc [
        ("scopes",
         `List [
             createScope "Variables" 1000 false
        ])
      ]

  let body_variables (vars_info : (string * string * string option * int) list) :Yojson.Basic.t=
    let vars_json = List.map (function (varname, vartype, varvalue, varref) -> createVariable varname vartype varvalue varref) vars_info in
    `Assoc [
        ("variables", `List vars_json)
      ]

  let body_breakpoints nb_bps :Yojson.Basic.t=
    `Assoc [
        ("breakpoints", `List (List.init nb_bps (fun _ -> `Assoc [("verified", `Bool true)])))
      ]

  let body_evaluate result  :Yojson.Basic.t=
    `Assoc [
        ("result", `String result);
        ("variablesReference", `Int 0)
      ]

  let last_request = ref `Null

  let initialize_mode = ref true

  (** Extract breakpoints from request *)
  let breakpoints_from_request request =
    let path = extract_path request
    and breakpoints_json = extract_breakpoints request
    in (path, List.map (function bp_json -> B_line (path, (extract_line_breakpoint bp_json))) breakpoints_json)

  (** Extract functional breakpoints from request *)
  let function_breakpoints_from_request request =
    let breakpoints_json = extract_breakpoints request in
    List.map (function bp_json -> B_function (extract_name bp_json)) breakpoints_json

  (** Map from variable reference to associated var_sub_value *)
  module VariablesRefs = Map.Make(struct
      type t = int
      let compare = Int.compare
    end)

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


  (** {2 User commands} *)
  (** ***************** *)

  (** Commands *)
  type command =
    | Initialize
    (** Initialize DA *)

    | Launch
    (** Launch DA *)

    | Threads
    (** Send list of current threads *)

    | StackTrace
    (** Send current stack trace *)

    | Scopes
    (** Send available scopes *)

    | Vars of int (** variables reference *)
    (** Send list of variables associated to the given variables reference*)

    | Breaks of (string (** file *) * breakpoint list (** breakpoints *))
    (** Set breakpoints for file *)

    | FuncBreaks of breakpoint list
    (** Set functional breakpoints *)

    | ExceptBreaks
    (** Set exceptional breakpoints *)

    | Continue
    (** Stop at next breakpoint *)

    | Next
    (** Stop at next statement and skip function calls *)

    | Step
    (** Step into function calls  *)

    | Finish
    (** Finish current function *)

    | Evaluate of string
  (* Evaluate given expression and send to the client's REPL
        Currently supported expressions : "state" for sending current abstract state
                                          "point" for sending current program point *)

  (** The last entered command *)
  let last_command = ref None

  (** Read a command from input *)
  let rec read_command () =
    let req = read_json_DAP () in
    let c = match extract_command req with
      | "initialize" -> Initialize
      | "launch" -> Launch
      | "setExceptionBreakpoints" -> ExceptBreaks
      | "setFunctionBreakpoints" -> FuncBreaks (function_breakpoints_from_request req)
      | "setBreakpoints" -> Breaks (breakpoints_from_request req)
      | "threads" -> Threads
      | "stackTrace" -> StackTrace
      | "scopes" -> Scopes
      | "variables" -> Vars (extract_varref req)
      | "continue" -> Continue
      | "next" -> Next
      | "stepIn" -> Step
      | "stepOut" -> Finish
      | "evaluate" -> Evaluate (extract_expression req)
      | _ -> read_command ()
    in
    last_request := req;
    last_command := Some c;
    c

  (** {2 Actions on the abstract domain} *)
  (** ********************************** *)

  (* FIXME: Merge with interactive *)
  (** Actions on abstract domain *)
  type _ action =
    | Exec : stmt * route -> Toplevel.t post action
    | Eval : expr * route * semantic * (semantic*(expr->bool)) list -> Toplevel.t eval action


  (** Get the program location related to an action *)
  let action_range : type a. a action -> range = function
    | Exec(stmt,_) -> stmt.srange
    | Eval(exp,_,_,_)  -> exp.erange

  (** Print an action *)
  let pp_action : type a. Toplevel.t flow -> formatter -> a action -> unit = fun flow fmt action ->
    fprintf fmt "%a@." Debug.(color fushia pp_range) (action_range action);
    match action with
    | Exec(stmt,route) ->
       fprintf fmt "@[<v 4>S[ %a@] ] in %a@."
         pp_stmt stmt
         pp_route route

    | Eval(exp,route,translate,translate_when) ->
       fprintf fmt "@[<v 4>E[ %a@] : %a ]<%a> in %a@."
         pp_expr exp
         pp_typ (etyp exp)
         pp_semantic translate
         pp_route route

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

      mutable variablesRefs: var_sub_value VariablesRefs.t;

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
      variablesRefs = VariablesRefs.empty;
      command = Step;
      depth = 0;
      command_depth = 0;
      command_callstack = empty_callstack;
      fstack = [];
      callstack = empty_callstack;
    }

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
    else if detect_return action prev_cs cs then
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


  (** {2 DAP engine} *)
  (** ********************** *)

  (** Apply an action on a flow and return its result *)
  let rec apply_action : type a. a action -> Toplevel.t flow -> a =
    fun action flow ->
    match action with
    | Exec(stmt, route) -> Toplevel.exec ~route stmt man flow
    | Eval(exp, route, translate, translate_when)  -> Toplevel.eval ~route ~translate ~translate_when exp man flow

  (** Wait for request and process it *)
  and interact: type a. a action -> Toplevel.t flow -> a = fun action flow ->
    let cmd = try read_command ()
              with Exit -> exit 0
    in
    state.command <- cmd;
    state.command_depth <- state.depth;
    state.command_callstack <- (Flow.get_callstack flow);
    match cmd with
    | Initialize ->
       write_json_DAP (create_response !last_request body_initilize);
       write_json_DAP (create_event "initialized" `Null);
       interact action flow

    | Launch ->
       write_json_DAP (create_response !last_request `Null);
       interact action flow

    | ExceptBreaks ->
       write_json_DAP (create_response !last_request `Null);
       interact action flow

    | Breaks breakpoints_info ->
       let path = fst breakpoints_info in
       let breakpoints_tmp = BreakpointSet.filter (function b -> match b with | B_function _-> true | B_line (path2, _) -> not (path=path2) ) state.breakpoints in
       state.breakpoints <- List.fold_right (fun bp set -> BreakpointSet.add bp set) (snd breakpoints_info) breakpoints_tmp ;
       write_json_DAP (create_response !last_request (body_breakpoints (List.length (snd breakpoints_info))));
       interact action flow

    | FuncBreaks breakpoints_info ->
       let breakpoints_tmp = BreakpointSet.filter (function b -> match b with | B_function _-> false | B_line _ -> true ) state.breakpoints in
       state.breakpoints <- List.fold_right (fun bp set -> BreakpointSet.add bp set) breakpoints_info breakpoints_tmp ;
       write_json_DAP (create_response !last_request (body_breakpoints (List.length breakpoints_info)));
       interact action flow

    | Threads ->
       if !initialize_mode then
         (
           write_json_DAP (create_response !last_request body_threads);
           write_json_DAP (create_event "stopped" (create_body_stopped "exception"));
           initialize_mode := false
         )
       else
         write_json_DAP (create_response !last_request body_threads);
       interact action flow

    | StackTrace ->
       let file_path_line_column_from_range range =
         let pos = Location.get_range_start range in
         let l = String.split_on_char '/' (Location.get_range_file range) in
         let path = String.concat "/" l
         and file = List.nth l ((List.length l) -1) in
         (file,path,pos.pos_line,pos.pos_column)
       in
       let cs = Flow.get_callstack flow and i = ref 0 in
       let call_stack_json =
         List.map (function callsite ->
                     i := !i+1;
                     let (file,path,line,column) = file_path_line_column_from_range callsite.call_range
                     in createFrame  !i (callsite.call_fun_orig_name) line column file path)
           cs
       in
       let (file,path,line,column) = file_path_line_column_from_range (action_range action) in
       let stack_trace_info = (createFrame  0 "Current pointer" line column file path)::call_stack_json in
       write_json_DAP (create_response !last_request (body_stackTrace stack_trace_info));
       interact action flow

    | Scopes ->
       write_json_DAP (create_response !last_request body_scopes);
       interact action flow

    | Vars varref ->
       let varref_cpt = ref 1001 in
       let info_variable name valeur =
         let varval = Some (Format.asprintf "%a" pp_var_value valeur)
         and vartype = Format.asprintf "%a" pp_typ valeur.var_value_type in
           (
             state.variablesRefs <- VariablesRefs.add !varref_cpt
                                      (match valeur.var_sub_value with
                                         Some valeur -> valeur
                                       | _ -> raise Not_found)
                                      state.variablesRefs;
             varref_cpt := !varref_cpt +1;
             (name, vartype, varval, !varref_cpt -1)
           )
       in
       let vars_info =
         if varref = 1000 then
           let vars = man.ask Q_defined_variables flow in
           List.map (function v ->
                       let valeur = man.ask (Q_debug_variable_value v) flow
                       and varname = Format.asprintf "%a" pp_var v
                       in info_variable varname valeur)
             vars
         else
           let sub_value = VariablesRefs.find varref state.variablesRefs in
           match sub_value with
           |  Named_sub_value list -> List.map (function (varname, valeur) -> info_variable varname valeur) list
           | Indexed_sub_value list -> List.mapi (fun i valeur -> info_variable (string_of_int i) valeur) list
       in
       write_json_DAP (create_response !last_request (body_variables vars_info));
       interact action flow

    | Continue ->
       write_json_DAP (create_response !last_request (`Assoc[]));
       write_json_DAP (create_event "stopped" (create_body_stopped "step"));
       apply_action action flow

    | Next | Step | Finish ->
       write_json_DAP (create_response !last_request `Null);
       write_json_DAP (create_event "stopped" (create_body_stopped "step"));
       apply_action action flow

    | Evaluate expr ->
       let result = match expr with
         | "state" -> asprintf "%a@." (format @@ Flow.print man.lattice.print) flow;
         | "point" -> asprintf "%a" (pp_action flow) action;
         | _ -> "List of supported expressions:\nstate - get current abstract state\npoint - get current program point"
       in
       write_json_DAP (create_response !last_request (body_evaluate result));
       interact action flow

  (** Interact with the user input or apply the action *)
  and interact_or_apply_action : type a. a action -> Location.range -> Toplevel.t flow -> a =
    fun action range flow ->
    on_pre_action action flow;
    let ret =
      if is_interaction_point action then (
        interact action flow
      ) else
        apply_action action flow
    in
    on_post_action action;
    ret

  and init prog =
    Toplevel.init prog man

  and exec ?(route=toplevel) stmt flow =
    interact_or_apply_action (Exec (stmt, route)) stmt.srange flow

  and eval ?(route=toplevel) ?(translate=any_semantic) ?(translate_when=[]) exp flow =
    interact_or_apply_action (Eval (exp, route, translate, translate_when)) exp.erange flow

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
    get_effects = (fun log -> log);
    set_effects = (fun log _ -> log);
    exec = exec;
    eval = eval;
    ask = ask;
    print_expr = print_expr;
  }

end
