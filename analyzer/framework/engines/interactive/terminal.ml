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

open Core.All
open Mopsa_utils
open Location
open Callstack
open Interface
open Format
open Breakpoint
open Toplevel
open Action
open Envdb
open Trace

module Make(Toplevel : TOPLEVEL) =
struct

  let opt_show_var_scope = ref true 

  (** Commands *)
  type terminal_command_kind =
    | Break of string
    (** Add a breakpoint *)

    | Continue
    (** Stop at next breakpoint *)

    | MopsaBackTrace
    (** Returns the current backtrace of Mopsa *)

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

    | Print of (string * string option) list * (string * int) option
    (** Print the abstract state or the value of variables *)
    (* string * string option: variable name, potential function name *)

    | Env of string list
    (** Print the current abstract environment, associated to token T_cur,
        eventually projected on a list of domains *)

    | State
    (** Print the current abstract state *)

    | Where
    (** Show current program point *)

    | Info of info_command
    (** Print extra information *)

    | Enable of enable_command
    (** Enable an option *)

    | Disable of enable_command
    (** Disable an option *)

    | Set of set_command * string
    (** Set an option *)

    | Unset of set_command
    (** Unset an option *)

    | BackTrace
    (** Print the callstack *)

    | LoadScript of string

    | Trace

    | Backward


  (** Information sub-commands *)
  and info_command =
    | Alarms
    | Breakpoints
    | Tokens
    | Variables
    | Context

  (** Enable/Disable sub-commands *)
  and enable_command =
    | Hook of string

  (** Set/Unset sub-commands *)
  and set_command =
    | Debug
    | Script
    | ShowVarScope

  type terminal_command_redirection =
    | Pipe of string
    | File of string

  (** Commands *)
  type terminal_command = {
    (** Kind of the command *)
    kind: terminal_command_kind;
    (** Optional flag *)
    redirection: terminal_command_redirection option;
  }

  (** Print a command *)
  let pp_terminal_command_kind fmt = function
    | Break loc   -> Format.fprintf fmt "break %s" loc
    | MopsaBackTrace -> Format.fprintf fmt "mopsa_bt"
    | Continue    -> Format.pp_print_string fmt "continue"
    | Next        -> Format.pp_print_string fmt "next"
    | Finish      -> Format.pp_print_string fmt "finish"
    | NextI       -> Format.pp_print_string fmt "nexti"
    | Step        -> Format.pp_print_string fmt "step"
    | StepI       -> Format.pp_print_string fmt "stepi"
    | Print([], None)    -> Format.pp_print_string fmt "print"
    | Print(vars, None)  ->
      Format.fprintf fmt "print %a"
        (pp_print_list
           ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
           (fun fmt (v, o_f) ->
              match o_f with
              | None -> fprintf fmt "%s" v
              | Some f -> fprintf fmt "%s:%s" v f)
        ) vars
    | Print(vars, Some(file, line))  ->
      Format.fprintf fmt "print@%s:%d %a"
        file line
        (pp_print_list
           ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
           (fun fmt (v, o_f) ->
              match o_f with
              | None -> fprintf fmt "%s" v
              | Some f -> fprintf fmt "%s:%s" v f)
        ) vars
    | Env []           -> Format.pp_print_string fmt "env"
    | Env domains      -> Format.fprintf fmt "env %a"
                            (pp_print_list
                               ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
                               pp_print_string
                            ) domains
    | State            -> Format.pp_print_string fmt "state"
    | Where            -> Format.pp_print_string fmt "where"
    | Info Alarms      -> Format.pp_print_string fmt "info alarms"
    | Info Breakpoints -> Format.pp_print_string fmt "info breakpoints"
    | Info Tokens      -> Format.pp_print_string fmt "info tokens"
    | Info Variables   -> Format.pp_print_string fmt "info variables"
    | Info Context     -> Format.pp_print_string fmt "info context"
    | Enable (Hook h)  -> Format.fprintf fmt "enable hook %s" h
    | Disable (Hook h) -> Format.fprintf fmt "disable hook %s" h
    | Set (Debug, d)   -> Format.fprintf fmt "set debug %s" d
    | Set (Script, d)  -> Format.fprintf fmt "set script %s" d
    | Set (ShowVarScope, d) -> Format.fprintf fmt "set showvarscope %s" d
    | LoadScript s     -> Format.fprintf fmt "load script %s" s
    | Unset Debug      -> Format.pp_print_string fmt "unset debug"
    | Unset Script     -> Format.pp_print_string fmt "unset script"
    | Unset ShowVarScope -> Format.pp_print_string fmt "unset showvarscope"
    | BackTrace        -> Format.pp_print_string fmt "backtrace"
    | Trace            -> Format.pp_print_string fmt "trace"
    | Backward         -> Format.pp_print_string fmt "backward"

  let pp_terminal_command_redirection fmt = function
    | Pipe shell -> Format.fprintf fmt " | %s" shell
    | File file  -> Format.fprintf fmt " > %s" file

  let pp_terminal_command fmt c =
    Format.fprintf fmt "%a%a"
      pp_terminal_command_kind c.kind
      (Format.pp_print_option pp_terminal_command_redirection) c.redirection

  (** Print help message *)
  let print_usage () =
    printf "Available commands:@.";
    printf "  b[reak] <[file:]line>     add a breakpoint at a line@.";
    printf "  b[reak] <function>        add a breakpoint at a function@.";
    printf "  b[reak] @name             add a named breakpoint (will break when the analysis executes an S_break name)@.";
    printf "  b[reak] #a[larm]          break at the next alarm (and go back at the statement generating the alarm)@.";
    printf "  c[ontinue]                run until next breakpoint@.";
    printf "  n[ext]                    stop at next statement and skip function calls.@.";
    printf "  n[ext]i                   stop at next statement and skip nodes in the interpretation sub-tree@.";
    printf "  s[tep]                    step into function calls@.";
    printf "  s[tep]i                   step into interpretation sub-tree@.";
    printf "  f[inish]                  finish current function@.";
    printf "  b[ack]w[ard]              go backward to the calling site@.";
    printf "  e[nable] h[hook] <h>      enable a hook@.";
    printf "  d[isable] h[hook] <h>     disable a hook@.";
    printf "  s[et] d[ebug] <d>         set debug channels@.";
    printf "  u[nset] d[ebug]           unset debug channels@.";
    printf "  s[et] script <file>       store commands into a file@.";
    printf "                            To be used in combination with load script <file>@.";
    printf "  u[nset] script            do not store commands in file anymore@.";
    printf "  load script <file>        reads script command from <file>@.";
    printf "  help                      print this message@.";
    printf "The commands below support shell commands (`env | grep foo`, `mopsa_bt | tac`, ...):@.";
    printf "  p[rint]                   print the abstract state@.";
    printf "  p[rint] <vars>            print the value of selected variables@.";
    printf "                            For example, `p x,y:f,z:*` prints x in the current scope, y in the scope of f, and z in all scopes@.";
    printf "  p[rint] <vars> #<f>:<l>   print the value of selected variables at the given program location@.";
    printf "  e[nv]                     print the current abstract environment@.";
    printf "  e[nv] <domain>,...        print the current abstract environment of selected domains@.";
    printf "  state                     print the full abstract state (map from flow token to environment)@.";
    printf "  b[ack]t[race]             print the current call stack@.";
    printf "  t[race]                   print the analysis trace@.";
    printf "  w[here]                   show current program point@.";
    printf "  i[info] a[larms]          print the list of detected alarms@.";
    printf "  i[info] c[hecks]          print the list of performed checks@.";
    printf "  i[info] b[reakpoints]     print the list of registered breakpoints@.";
    printf "  i[info] t[okens]          print the list of flow tokens@.";
    printf "  i[info] v[ariables]       print the list of variables@.";
    printf "  i[info] c[on]t[e]x[t]     print the flow-insensitive context@.";
    printf "  mopsa_bt                  shows the current backtrace of the analyzer@.";
    ()


  (** Print input prompt *)
  let print_prompt () =
    printf "%a %a @?"
      Debug.(color_str teal) "mopsa"
      Debug.(color_str green) ">>"

  (** Context of LineEdit library *)
  let linedit_ctx = LineEdit.create_ctx ()

  (** Reference to the last commands read from the prompt *)
  let last_prompt_commands = ref []

  (** Buffer of upcoming commands to execute *)
  let commands_buffer = Queue.create ()

  let script : out_channel option ref = ref None

  (* Read the next command as a string *)
  let rec read_terminal_command_string () =
    (* Check the commands buffer *)
    if not (Queue.is_empty commands_buffer) then
      Queue.pop commands_buffer
    else
      (* Buffer is empty, so ask the user *)
      let () = print_prompt () in
      let input = LineEdit.read_line linedit_ctx |>
                  String.trim in
      (* Split the input into multiple commands with delimeter ';' *)
      let parts = String.split_on_char ';' input |>
                  List.map String.trim |>
                  List.filter (function "" -> false | _ -> true) in
      match parts, !last_prompt_commands with
      | [], [] ->
        (* No command entered and no command in history, so as user again *)
        read_terminal_command_string ()

      | [], _ ->
        (* No command entered, but we know the last command, so replay it *)
        let () = List.iter (fun c -> Queue.add c commands_buffer) !last_prompt_commands in
        read_terminal_command_string ()

      | _ ->
        (* Some commands entered. Save them in [last_prompt_commands],
           initialize [commands_buffer] and iterate again *)
        last_prompt_commands := parts;
        List.iter (fun c -> Queue.add c commands_buffer) parts;
        read_terminal_command_string ()

  exception ReadNewCommand

  let parse_command_kind s flow =
    let parts = String.split_on_char ' ' s |>
                List.map String.trim |>
                List.filter (function "" -> false | _ -> true)
    in
    match parts with
    | ["continue" | "c"]   -> Continue
    | ["mopsa_bt"]         -> MopsaBackTrace
    | ["next"     | "n"]   -> Next
    | ["step"     | "s"]   -> Step
    | ["finish"   | "f"]   -> Finish
    | ["nexti"    |"ni"]   -> NextI
    | ["stepi"    |"si"]   -> StepI
    | ["where"    | "w"]   -> Where
    | ["backtrace"|"bt"]   -> BackTrace
    | ["break"    | "b"; l]-> Break l
    | ["backward" | "back" | "bw"] -> Backward

    | ("env"      | "e") :: domains ->
      let domains =
        List.fold_left
          (fun acc s ->
             let parts = String.split_on_char ',' s |>
                         List.filter (function "" -> false | _ -> true)
             in
             SetExt.StringSet.union acc (SetExt.StringSet.of_list parts)
          ) SetExt.StringSet.empty domains
      in
      Env (SetExt.StringSet.elements domains)

    | ["state" ] -> State 

    | ["help" | "h"]   ->
      print_usage ();
      raise ReadNewCommand

    | ["info" |"i"; "tokens"      | "t"] | ["it"] -> Info Tokens
    | ["info" |"i"; "breakpoints" | "b"] | ["ib"] -> Info Breakpoints
    | ["info" |"i"; "alarms"      | "a"] | ["ia"] -> Info Alarms
    | ["info" |"i"; "variables"   | "vars" | "var"  | "v"] | ["iv"] -> Info Variables
    | ["info" |"i"; "context"     | "ctx"] | ["ictx"] -> Info Context

    | ["enable" | "en"; "hook" | "h"; h] | ["eh"; h] -> Enable (Hook h)
    | ["disable"|"d";   "hook" | "h"; h] | ["dh"; h] -> Disable (Hook h)

    | ["set"  |"s";  "debug"| "d"; d] | ["sd"; d] -> Set (Debug, d)
    | ["unset"|"u";  "debug"| "d"] | ["ud"] -> Unset Debug

    | ["set"  |"s";  "script"| "s"; d] | ["sc"; d] -> Set (Script, d)
    | ["unset"|"u";  "script"| "s"] | ["uc"] -> Unset Script

    | ["load"; "script"; s] | ["ls"; s] -> LoadScript s

    | ("print"    | "p") :: vars ->
      let cs = Flow.get_callstack flow in
      let vars =
        List.fold_left
          (fun acc s ->
             let parts = String.split_on_char ',' s |>
                         List.filter (function "" -> false | _ -> true) in
             let parts = List.map (fun s ->
                 let l = String.split_on_char ':' s in
                 if List.length l = 1 then
                   if cs = [] then List.hd l, None
                   else List.hd l, Some (List.hd cs).call_fun_orig_name
                 else
                   let f = List.hd @@ List.tl l in
                   if String.compare f "*" = 0 then List.hd l, None
                   else (List.hd l, Some f)
               ) parts
             in
             parts @ acc
          ) [] vars
      in
      Print (vars, None)

    | print::vars when Str.string_match (Str.regexp {|\(p\|print\)@\(.+\):\([0-9]+\)|}) print 0 ->
      let file = Str.matched_group 2 print in
      let line = Str.matched_group 3 print |> int_of_string in
      let vars =
        List.fold_left
          (fun acc s ->
             let parts = String.split_on_char ',' s |>
                         List.filter (function "" -> false | _ -> true)
             in
             let parts = List.map (fun s ->
                 let l = String.split_on_char ':' s in
                 if List.length l = 1 then List.hd l, None
                 else (List.hd l, Some (List.hd @@ List.tl l))) parts
             in
             parts @ acc)
          [] vars 
      in
      Print(vars, Some(file, line))

    | ["trace"|"t"]   -> Trace

    | _ ->
      printf "Unknown command %s@." s;
      print_usage ();
      raise ReadNewCommand

  let parse_command_redirection s =
    if Str.(string_match (regexp {|\([^|]+\)|\(.*\)|}) s 0) then
      let cmd = Str.matched_group 1 s |> String.trim in
      let redirect = Pipe (Str.matched_group 2 s |> String.trim) in
      cmd, Some redirect
    else
    if Str.(string_match (regexp {|\([^>]+\)>\([^>]+\)|}) s 0) then
      let cmd = Str.matched_group 1 s |> String.trim in
      let redirect = File (Str.matched_group 2 s |> String.trim) in
      cmd, Some redirect
    else
      s, None

  (** Read the next command *)
  let rec read_terminal_command logger flow =
    let s = read_terminal_command_string () in
    logger s;
    let s, redirection = parse_command_redirection s in
    try
      let kind = parse_command_kind s flow in
      { kind; redirection }
    with ReadNewCommand ->
      read_terminal_command logger flow


  (** {2 Pretty printers} *)
  (** ******************* *)

  module Addr =
  struct
    type t = addr
    let compare = compare_addr
    let print = unformat pp_addr
    let from_expr e =
      match ekind e with
      | E_addr (addr, _) -> addr
      | _ -> assert false
  end

  module AddrSet =
  struct
    include SetExt.Make(Addr)
    let print printer s =
      pp_list Addr.print printer (elements s) ~lopen:"{" ~lsep:"," ~lclose:"}"
  end


  let dummy_range = mk_fresh_range ()

  (** Print value of variables *)
  let pp_vars fmt action names man flow =
    if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then
      fprintf fmt "⊥@."
    else
      let () = Ast.Var.print_uniq_with_uid := false in 
      let names =
        match names with
        | [] ->
          List.map (fun v -> asprintf "%a" pp_var v, None) (action_line_vars action)
        | _ -> names in
      let names_global, names_by_func = 
        List.fold_left (fun (global_acc, func_acc) (name, o_f) ->
            match o_f with
            | None -> (name :: global_acc, func_acc)
            | Some f -> (global_acc,
                         if MapExt.StringMap.mem f func_acc then
                           MapExt.StringMap.add f (name :: MapExt.StringMap.find f func_acc) func_acc
                         else
                           MapExt.StringMap.add f [name] func_acc)
          ) ([], MapExt.StringMap.empty) names in 
      let vars = ask_and_reduce man.ask
          (Q_defined_variables None) flow in
      let vmap =
        List.fold_left
          (fun acc v ->
             let vname = asprintf "%a" pp_var v in
             let old = OptionExt.default VarSet.empty
                 (MapExt.StringMap.find_opt vname acc) in
             MapExt.StringMap.add vname (VarSet.add v old) acc)
          MapExt.StringMap.empty vars
      in
      let addrs = ask_and_reduce man.ask Q_allocated_addresses flow in
      let amap =
        List.fold_left
          (fun acc a ->
             let aname = asprintf "%a" pp_addr a in
             let old = OptionExt.default AddrSet.empty
                 (MapExt.StringMap.find_opt aname acc) in
             MapExt.StringMap.add aname (AddrSet.add a old) acc)
          MapExt.StringMap.empty addrs
      in
      let vfound, afound, not_found = List.fold_left
          (fun (vfound,afound,not_found) name ->
             match MapExt.StringMap.find_opt name vmap with
             | Some vars -> (VarSet.elements vars)@vfound,afound,not_found
             | None     ->
               match MapExt.StringMap.find_opt name amap with
               | Some addrs -> vfound,(AddrSet.elements addrs)@afound,not_found
               | None      -> vfound,afound,name::not_found
          ) ([],[],[]) names_global in
      let vfound, afound, not_found = MapExt.StringMap.fold
          (fun func vs (vfound, afound, not_found) ->
             let vars = ask_and_reduce man.ask (Q_defined_variables (Some func)) flow in
             let vmap = List.fold_left
               (fun acc v ->
                  let vname = asprintf "%a" pp_var v in
                  let old = OptionExt.default VarSet.empty
                      (MapExt.StringMap.find_opt vname acc) in
                  MapExt.StringMap.add vname (VarSet.add v old) acc)
               MapExt.StringMap.empty vars in
             List.fold_left (fun (vfound, afound, not_found) name -> 
                 match MapExt.StringMap.find_opt name vmap with
                 | Some vars -> (VarSet.elements vars)@vfound,afound,not_found
                 | None     ->
                   match MapExt.StringMap.find_opt name amap with
                   | Some addrs -> vfound,(AddrSet.elements addrs)@afound,not_found
                   | None      -> vfound,afound,name::not_found
               ) (vfound, afound, not_found) vs 
          )
          names_by_func (vfound, afound, not_found) in
      let protect_print print_thunk =
        if not @@ !opt_show_var_scope then 
          let oldv = !Core.Ast.Var.print_uniq_with_uid in
          (
            Core.Ast.Var.print_uniq_with_uid := false;
            print_thunk ();
            Core.Ast.Var.print_uniq_with_uid := oldv;
          )
        else
          print_thunk ()
      in
      let () = Ast.Var.print_uniq_with_uid := true in 
      let not_found' =
        let printer = empty_printer () in
        let found,not_found =
          List.fold_left
            (fun (found,not_found) v ->
               try
                 let () = 
                   if VarSet.cardinal @@ OptionExt.default VarSet.empty (MapExt.StringMap.find_opt (asprintf "%a" pp_var v) vmap) > 1 then
                     protect_print (fun () -> man.print_expr flow printer (mk_var v dummy_range))
                   else 
                     man.print_expr flow printer (mk_var v dummy_range) in
                 true,not_found
               with Not_found ->
                 let vname = asprintf "%a" pp_var v in
                 found,vname::not_found
            ) (false,[]) vfound
        in
        let found,not_found =
          List.fold_left
            (fun (found,not_found) a ->
               try
                 let () = man.print_expr flow printer (mk_addr a dummy_range) in
                 true,not_found
               with Not_found ->
                 let aname = asprintf "%a" pp_addr a in
                 found,aname::not_found
            ) (found,not_found) afound
        in
        if found then fprintf fmt "%a@." pflush printer;
        not_found
      in
      ( match not_found@not_found' with
        | [] -> ()
        | l  ->
          fprintf fmt "Variable%a %a not found@."
            Debug.plurial_list not_found
            (pp_print_list
               ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
               (fun fmt vname -> fprintf fmt "'%s'" vname)
            ) l
      )

  let init () =
    printf "@.%a@.Type '%a' to get the list of commands.@.@."
      (Debug.bold pp_print_string) ("Welcome to Mopsa " ^ Version.version ^ "!")
      (Debug.bold pp_print_string) "help"

  let reach action man flow =
    (* Print the range of the next action *)
    printf "%a@." Debug.(color fushia pp_relative_range) (action_range action);
    (* Print location in the source code *)
    pp_action_source_code std_formatter action;
    (* Print interpreter action *)
    printf "%a@." (pp_action ~truncate:true ~indent:0) action

  let pp_alarms fmt alarms =
    fprintf fmt "%d new alarm%a detected: @[<v>@."
      (List.length alarms)
      Debug.plurial_list alarms;
    ignore(Output.Text.construct_checks_summary ~print:true (Alarm.alarms_to_report alarms) None);
    fprintf fmt "@]@."

  let alarm alarms action man flow =
    pp_alarms Format.std_formatter alarms

  let logger cmd_str =
    if List.mem cmd_str ["us"; "unset script"; "unset s"] then
      ()
    else
      (* todo: currently logs unset script, whoops *)
      match !script with
      | None -> ()
      | Some ch ->
        let file_fmt = formatter_of_out_channel ch in
        Format.fprintf file_fmt "%s@." cmd_str
  (* todo: remove last @. ... *)

  let pp_output_command action envdb man flow fmt = function
    | BackTrace ->
      let cs = Flow.get_callstack flow in
      fprintf fmt "%a@." pp_callstack cs

    | MopsaBackTrace ->
      let (in_file_descr, out_file_descr) = Unix.pipe () in
      let in_channel = Unix.in_channel_of_descr in_file_descr in
      let out_channel = Unix.out_channel_of_descr out_file_descr in
      Printexc.print_raw_backtrace out_channel (Printexc.get_callstack Int.max_int);
      let buffer = Buffer.create 100 in
      close_out out_channel;
      let rec loop () =
        match input_line in_channel with
        | l -> Buffer.add_string buffer (l ^ "\n"); loop ()
        | exception End_of_file -> Buffer.contents buffer
      in
      let r = loop () in
      close_in in_channel;
      fprintf fmt "%s@." r

    | Print(names, loc) ->
      let ctx = Flow.get_ctx flow in
      let env =
        match loc with
        | None -> Some(action, flow)
        | Some(file, line) ->
          match find_envdb_opt file line envdb with
          | None -> None
          | Some(action', envs) ->
            fprintf fmt "%a@." (pp_action ~truncate:false ~indent:0) action';
            let env =
              CallstackMap.fold
                (fun _ -> man.lattice.join ctx)
                envs man.lattice.bottom
            in
            Some(action', Flow.singleton ctx T_cur env)
      in
      ( match env with
        | None -> ()
        | Some(action', flow') -> pp_vars fmt action' names man flow'
      )

    | Env [] ->
      let env = Flow.get T_cur man.lattice flow in
      fprintf fmt "%a@." (Print.format man.lattice.print) env

    | Env domains ->
      let env = Flow.get T_cur man.lattice flow in
      let re =
        List.map
          (fun d ->
             (* Replace wildcard shortcut '_' *)
             let d' = Str.global_replace (Str.regexp_string "_") ".*" d in
             (* Accept any domain name containing the given string *)
             ".*" ^ d' ^ ".*"
          ) domains |>
        (* Add alternative operator between domains names *)
        String.concat "\\|" |>
        Str.regexp
      in
      let pobj = pbox man.lattice.print env in
      let pobj' = match_print_object_keys re pobj in
      fprintf fmt "%a@." pp_print_object pobj'

    | State ->
      fprintf fmt "%a@." (Print.format (Flow.print man.lattice.print)) flow

    | Where ->
      pp_action_source_code std_formatter action;
      fprintf fmt "%a@." Debug.(color fushia pp_relative_range) (action_range action);
      fprintf fmt "%a@." (pp_action ~truncate:false ~indent:0) action

    | Info Tokens ->
      let tokens = Flow.fold (fun acc tk _ -> tk::acc) [] flow in
      fprintf fmt "%a@." (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_token) tokens

    | Info Alarms ->
      let report = Flow.get_report flow in
      ( if is_safe_report report
        then printf "%a No alarm@." Debug.(color_str green) "✔";
        let _ = Output.Text.construct_checks_summary ~print:true report None in
        ()
      )

    | Info Breakpoints ->
      fprintf fmt "%a@." Breakpoint.pp_breakpoint_set !breakpoints

    | Info Variables ->
      let vars = ask_and_reduce man.ask (Q_defined_variables None) flow in
      fprintf fmt "@[<v>%a@]@."
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt v -> Query.pp_var_with_type fmt (v,v.vtyp))
        ) vars

    | Info Context ->
      let ctx = Flow.get_ctx flow in
      fprintf fmt "%a@." (pp_ctx man.lattice.print) ctx

    | Trace ->
      fprintf fmt "%a@." pp_trace state.trace

   | _ -> assert false

  let process_output_command cmd action envdb man flow =
    match cmd.redirection with
    | None ->
      pp_output_command action envdb man flow Format.std_formatter cmd.kind 
    
    | Some(File file) ->
      let old_print_color = !Debug.print_color in
      Debug.print_color := false;
      let output = asprintf "%a" (pp_output_command action envdb man flow) cmd.kind in
      Debug.print_color := old_print_color;
      let ch = open_out file in
      output_string ch output;
      flush ch;
      close_out ch
    
    | Some(Pipe shell) ->
      let old_print_color = !Debug.print_color in
      let output = asprintf "%a" (pp_output_command action envdb man flow) cmd.kind in
      Debug.print_color := old_print_color;
      let in_ch, out_ch = Unix.open_process shell in
      output_string out_ch output;
      flush out_ch;
      close_out out_ch;
      let rec iter () =
        try
          printf "%s@." (input_line in_ch);
          iter ()
        with End_of_file -> ()
      in
      iter ();
      close_in in_ch



  let rec read_command action envdb man flow =
    let cmd =
      try read_terminal_command logger flow
      with Exit -> exit 0
    in
    match cmd.kind with
    | Break loc ->
      let () =
        try
          let default_file =
            try get_range_file (action_range action)
            with _ -> "" (* FIXME: use first file in list of analyzed files *) in
          let bp = parse_breakpoint default_file loc in 
          breakpoints := BreakpointSet.add bp !breakpoints;
        with Invalid_breakpoint_syntax -> printf "Invalid breakpoint syntax@." in
      read_command action envdb man flow

    | Continue ->
      Interface.Continue

    | Next ->
      Next

    | NextI ->
      NextI

    | Step ->
      Step

    | StepI ->
      StepI

    | Finish ->
      Finish

    | Backward ->
      Backward

    | Enable (Hook hook) ->
      if not (Hook.mem_hook hook) then (
        printf "Hook '%s' not found@." hook;
        read_command action envdb man flow
      ) else (
        Hook.activate_hook hook;
        let ctx = Hook.init_hook hook (Flow.get_ctx flow) in
        let flow = Flow.set_ctx ctx flow in
        read_command action envdb man flow
      )

    | Disable (Hook hook) ->
      if not (Hook.mem_hook hook) then (
        printf "Hook '%s' not found@." hook
      ) else (
        Hook.deactivate_hook hook man flow
      );
      read_command action envdb man flow

    | Set (Debug, channel) ->
      Debug.set_channels channel;
      read_command action envdb man flow

    | Unset Debug ->
      Debug.set_channels "";
      read_command action envdb man flow

    | Set (Script, filename) ->
      let ch = open_out filename in 
      script := Some ch;
      read_command action envdb man flow

    | Unset Script ->
      let () = match !script with
        | None -> ()
        | Some ch ->
          close_out ch;
          script := None in
      read_command action envdb man flow

    | LoadScript s ->
      let ch = open_in s in
      let lines =
        let rec process res =
          try
            process ((input_line ch) :: res)
          with End_of_file ->
            List.rev res
        in process []
      in
      List.iter (fun l -> Queue.add l commands_buffer) lines;
      close_in ch;
      read_command action envdb man flow

    | Set (ShowVarScope, _) ->
      opt_show_var_scope := true;
      read_command action envdb man flow

    | Unset ShowVarScope ->
      opt_show_var_scope := false;
      read_command action envdb man flow

    | BackTrace
    | MopsaBackTrace
    | Print _
    | Env _
    | State
    | Where
    | Info _
    | Trace ->
      process_output_command cmd action envdb man flow;
      read_command action envdb man flow


  let finish man flow =
    ()

  let error ex =
    printf "%a@\n%a@."
      (Debug.color_str Debug.red) "Analysis aborted"
      (fun fmt -> function
         | Exceptions.Panic (msg, "") -> fprintf fmt "panic: %s@." msg
         | Exceptions.Panic (msg, loc) -> fprintf fmt "panic raised in %s: %s@." loc msg

         | Exceptions.PanicAtLocation (range, msg, "") -> fprintf fmt "panic in %a: %s@." Location.pp_range range msg
         | Exceptions.PanicAtLocation (range, msg, loc) -> fprintf fmt "%a: panic raised in %s: %s@." Location.pp_range range loc msg

         | Exceptions.PanicAtFrame (range, cs, msg, "") -> fprintf fmt "panic in %a: %s@\nTrace:@\n%a@." Location.pp_range range msg pp_callstack cs
         | Exceptions.PanicAtFrame (range, cs, msg, loc) -> fprintf fmt "%a: panic raised in %s: %s@\nTrace:@\n%a@." Location.pp_range range loc msg pp_callstack cs

         | Exceptions.SyntaxError (range, msg) -> fprintf fmt "%a: syntax error: %s@." Location.pp_range range msg
         | Exceptions.UnnamedSyntaxError range -> fprintf fmt "%a: syntax error@." Location.pp_range range

         | Exceptions.SyntaxErrorList l ->
           fprintf fmt "Syntax errors:@\n  @[%a@]@."
             (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
                (fun fmt (range, msg) -> fprintf fmt "%a: %s" Location.pp_range range msg
                )
             ) l

         | Exceptions.UnnamedSyntaxErrorList l ->
           fprintf fmt "Syntax errors:@\n  @[%a@]@."
             (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") Location.pp_range)
             l

         | ex -> fprintf fmt "Uncaught exception: %s@." (Printexc.to_string ex)
      ) ex
end
