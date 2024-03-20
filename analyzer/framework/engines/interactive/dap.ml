(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2023 The MOPSA Project.                               *)
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
open Breakpoint
open Interface
open Query
module IntMap = MapExt.IntMap
module StringMap = MapExt.StringMap
open Toplevel
open Action
open Envdb

module Make(Toplevel : TOPLEVEL) =
struct

  (*************************)
  (** Variables references *)
  (*************************)

  type value =
    | Leaf of string
    | Compound of int

  let vref_counter = ref 0

  let compute_vrefs pobj =
    let vrefs = ref IntMap.empty in
    let rec iter = function
      | Map(m, _) ->
        incr vref_counter;
        let vref = !vref_counter in
        let children =
          MapExtPoly.fold
            (fun k v acc ->
               let name = Format.asprintf "%a" pp_print_object k in
               let vv =
                 match k with
                 | Var _ -> Leaf (Format.asprintf "%a" pp_print_object v)
                 | _     -> iter v
               in
               (name, vv) :: acc 
            ) m []
        in
        vrefs := IntMap.add vref (List.rev children) !vrefs;
        Compound vref
      | List(l, _) ->
        incr vref_counter;
        let vref = !vref_counter in
        let children =
          List.mapi
            (fun i v ->
               string_of_int i, iter v
            ) l
        in
        vrefs := IntMap.add vref children !vrefs;
        Compound vref
      | Set(s, _) ->
        incr vref_counter;
        let vref = !vref_counter in
        let children =
          SetExtPoly.elements s |>
          List.mapi (fun i v -> string_of_int i, iter v)
        in
        vrefs := IntMap.add vref children !vrefs;
        Compound vref
      | pobj ->
        let s = Format.asprintf "%a" pp_print_object pobj in
        Leaf s
    in
    let _ = iter pobj in
    !vrefs

  let compute_scopes_vrefs pobj =
    let initial_vref = !vref_counter in
    let vrefs = ref (compute_vrefs pobj) in
    if IntMap.is_empty !vrefs then
      IntMap.empty, []
    else
      let scopes = IntMap.find (initial_vref + 1) !vrefs in
      let scopes =
        scopes |> List.map
          (fun (name, value) ->
             let vref = match value with
               | Compound vref -> vref
               | Leaf _ ->
                 incr vref_counter;
                 let vref = !vref_counter in
                 vrefs := IntMap.add vref ["", value] !vrefs;
                 vref
             in
             (name, vref)
          )
      in
      !vrefs, scopes

  let vrefs = ref IntMap.empty

  (********************)
  (** JSON processing *)
  (********************)

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
  let extract_file request = request |> member "arguments" |> member "file" |> to_string
  let extract_line request = request |> member "arguments" |> member "line" |> to_int

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

  let createScope name vref :Yojson.Basic.t =
    `Assoc [
      ("name", `String name);
      ("variablesReference", `Int vref);
      ("expensive", `Bool true);
    ]

  let createVariable name = function
    | Leaf value ->
      `Assoc [
        ("name", `String name);
        ("value", `String value);
        ("variablesReference", `Int 0)
      ]
    | Compound vref ->
      `Assoc [
        ("name", `String name);
        ("value", `String "");
        ("variablesReference", `Int vref)
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

  let body_scopes scopes :Yojson.Basic.t =
    `Assoc [
      ("scopes",
       `List (
         List.map (fun (name, vref) -> createScope name vref) scopes
       )
      )
    ]

  let body_variables vars : Yojson.Basic.t =
    `Assoc [
      ("variables", `List (List.map (fun (name, value) -> createVariable name value) vars))
    ]

  let body_breakpoints nb_bps :Yojson.Basic.t=
    `Assoc [
      ("breakpoints", `List (List.init nb_bps (fun _ -> `Assoc [("verified", `Bool true)])))
    ]

  let body_empty_evaluate :Yojson.Basic.t=
    `Assoc [
      ("result", `Null);
      ("variablesReference", `Int 0)
    ]

  let body_evaluate vref :Yojson.Basic.t=
    `Assoc [
      ("result", `Null);
      ("variablesReference", `Int vref)
    ]

  let report_of_alarms alarms =
    alarms |> List.fold_left
      (fun report alarm -> add_alarm alarm report)
      empty_report

  let create_body_alarms_output alarms : Yojson.Basic.t =
    `Assoc [
      ("category", `String "important");
      ("output", `String (Format.asprintf "%d new alarm%a" (List.length alarms) Debug.plurial_list alarms));
      ("data", `Assoc [
          ("kind", `String "alarms");
          ("alarms", `List (Output.Json.render_alarms (report_of_alarms alarms)));
        ])
    ]

  let create_body_environment_output file line env : Yojson.Basic.t =
    `Assoc [
      ("category", `String "important");
      ("output", `Null);
      ("data", `Assoc [
          ("kind", `String "environment");
          ("file", `String file);
          ("line", `Int line);
          ("envrionment", env);
        ])
    ]

  let last_request = ref `Null

  (** Extract breakpoints from request *)
  let breakpoints_from_request request =
    let path = extract_path request
    and breakpoints_json = extract_breakpoints request
    in (path, List.map (function bp_json -> B_line (path, (extract_line_breakpoint bp_json))) breakpoints_json)

  (** Extract functional breakpoints from request *)
  let function_breakpoints_from_request request =
    let breakpoints_json = extract_breakpoints request in
    List.map (function bp_json -> B_function (extract_name bp_json)) breakpoints_json


  (** Commands *)
  type dap_command =
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
    (* Evaluate given expression and send to the client's REPL *)

    | Environment of string * int

    | Disconnect

  (** The last entered command *)
  let last_command = ref None

  (** Read a command from input *)
  let rec read_dap_command () =
    let req = read_json_DAP () in
    let cmd = extract_command req in
    let c = match cmd with 
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
      | "disconnect" -> Disconnect
      | "environment" -> Environment (extract_file req, extract_line req)
      | _ -> read_dap_command ()
    in
    last_request := req;
    last_command := Some c;
    c


  let init () =
    let cmd = try read_dap_command ()
      with Exit -> exit 0
    in
    match cmd with
    | Initialize ->
      write_json_DAP (create_response !last_request body_initilize);
      write_json_DAP (create_event "initialized" `Null)

    | _ ->
      assert false

  let reach action man flow =
    let range = action_range action in
    if is_orig_range range then (
      vrefs := IntMap.empty;
      vref_counter := 0;
      write_json_DAP (create_event "stopped" (create_body_stopped "step"));
    )

  let alarm alarms action man flow =
    write_json_DAP (create_event "output" (create_body_alarms_output alarms))

  let dummy_range = mk_fresh_range ()

  let rec read_command action envdb man flow =
    if not (is_orig_range (action_range action)) then
      Interface.Step
    else
      let cmd = try read_dap_command ()
        with Exit -> exit 0
      in
      match cmd with
      | Initialize ->
        assert false

      | Launch ->
        write_json_DAP (create_response !last_request `Null);
        read_command action envdb man flow

      | ExceptBreaks ->
        write_json_DAP (create_response !last_request `Null);
        read_command action envdb man flow

      | Breaks breakpoints_info ->
        let path = fst breakpoints_info in
        let breakpoints_tmp =
          BreakpointSet.filter
            (fun b -> match b with
               | B_function _-> true
               | B_line (path2, _) -> not (path=path2)
               | _ -> false
            ) !breakpoints
        in
        breakpoints := List.fold_left (fun set bp -> BreakpointSet.add bp set) breakpoints_tmp (snd breakpoints_info);
        write_json_DAP (create_response !last_request (body_breakpoints (List.length (snd breakpoints_info))));
        read_command action envdb man flow

      | FuncBreaks breakpoints_info ->
        let breakpoints_tmp =
          BreakpointSet.filter
            (function b ->
             match b with
             | B_function _-> false
             | B_line _ -> true
             | _ -> false
            ) !breakpoints in
        breakpoints := List.fold_right (fun bp set -> BreakpointSet.add bp set) breakpoints_info breakpoints_tmp ;
        write_json_DAP (create_response !last_request (body_breakpoints (List.length breakpoints_info)));
        read_command action envdb man flow

      | Threads ->
        write_json_DAP (create_response !last_request body_threads);
        read_command action envdb man flow

      | StackTrace ->
        let file_path_line_column_from_range range =
          let pos = Location.get_range_start range in
          let l = String.split_on_char '/' (Location.get_range_file range) in
          let path = String.concat "/" l in
          let path = if Filename.is_relative path then Sys.getcwd () ^ "/" ^ path else path in
          let file = List.nth l ((List.length l) -1) in
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
        read_command action envdb man flow

      | Scopes ->
        if is_orig_range (action_range action) then
          let printer = Print.empty_printer () in
          let vars = action_vars action in
          List.iter
            (fun v ->
               try man.print_expr flow printer (mk_var v dummy_range)
               with Not_found -> ()
            ) vars;
          let pobj = get_printed_object printer in
          let map, scopes = compute_scopes_vrefs pobj in
          vrefs := IntMap.fold (fun vref v acc -> IntMap.add vref v acc) map !vrefs;
          write_json_DAP (create_response !last_request (body_scopes scopes))
        else
          write_json_DAP (create_response !last_request (body_scopes []));
        read_command action envdb man flow

      | Vars vref ->
        let children = IntMap.find vref !vrefs in
        write_json_DAP (create_response !last_request (body_variables children));
        read_command action envdb man flow

      | Continue ->
        write_json_DAP (create_response !last_request `Null  );
        Continue

      | Next ->
        write_json_DAP (create_response !last_request `Null);
        Next

      | Step ->
        write_json_DAP (create_response !last_request `Null);
        Step

      | Finish ->
        write_json_DAP (create_response !last_request `Null);
        Finish

      | Evaluate vars ->
        let vars = String.split_on_char ' ' vars |>
                   List.map String.trim |>
                   List.filter (function "" -> false | _ -> true)
        in
        let vars =
          List.fold_left
            (fun acc s ->
               let parts = String.split_on_char ',' s |>
                           List.filter (function "" -> false | _ -> true)
               in
               SetExt.StringSet.union acc (SetExt.StringSet.of_list parts)
            ) SetExt.StringSet.empty vars
        in
        let vars =
          SetExt.StringSet.elements vars |> ListExt.map_filter
            (fun name ->
               try Some (find_var_by_name name man flow)
               with Not_found -> None
            )
        in
        let printer = Print.empty_printer () in
        List.iter
          (fun v ->
             try man.print_expr flow printer (mk_var v dummy_range)
             with Not_found -> ()
          ) vars;
        let pobj = get_printed_object printer in
        let initial_vref = !vref_counter in
        let map = compute_vrefs pobj in
        vrefs := IntMap.fold (fun vref v acc -> IntMap.add vref v acc) map !vrefs;
        if IntMap.is_empty map then
          write_json_DAP (create_response !last_request body_empty_evaluate)
        else
          write_json_DAP (create_response !last_request (body_evaluate (initial_vref + 1)));
        read_command action envdb man flow

      | Environment(file, line) ->
        let envs, vars =
          match find_envdb_opt file line envdb with
          | None -> CallstackMap.empty, []
          | Some(action, envs) ->
            let vars = action_line_vars action in
            envs, vars
        in
        let ctx = Flow.get_ctx flow in
        let env =
          CallstackMap.fold
            (fun _ -> man.lattice.join ctx)
            envs man.lattice.bottom
        in
        let flow' = Flow.singleton ctx T_cur env in
        let printer = Print.empty_printer () in
        vars |> List.iter
          (fun v ->
             try man.print_expr flow' printer (mk_var v dummy_range)
             with Not_found -> ()
          );
        let body = print_object_to_json (get_printed_object printer) in
        write_json_DAP (create_response !last_request body);
        read_command action envdb man flow

      | Disconnect ->
        write_json_DAP (create_response !last_request `Null);
        raise Exit

  let rec wait_disconnect () =
    let cmd =
      try read_dap_command ()
      with Exit -> exit 0
    in
    match cmd with
    | Disconnect ->
      write_json_DAP (create_response !last_request `Null);
      exit 0
    | _ -> wait_disconnect ()


  let finish man flow =
    write_json_DAP (create_event "terminated" `Null);
    wait_disconnect ()


  let error e =
    write_json_DAP (create_event "output" (
        `Assoc [
          ("category", `String "stderr");
          ("output", `String (Format.asprintf "Exception: %s" (Printexc.to_string e)))
        ]
      ));
    wait_disconnect ()
end
