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
open Command



(** {2 Interactive engine} *)
(** ********************** *)

module Make(Toplevel : TOPLEVEL) =
struct

  type t = Toplevel.t

  let debug fmt = Debug.debug ~channel:"framework.engines.interactive" fmt

  (** Catch Ctrl+C interrupts as a Break exception*)
  let () = Sys.catch_break true


  (** {2 Actions on the abstract domain} *)
  (** ********************************** *)

  (** Actions on abstract domain *)
  type _ action =
    | Exec : stmt * route -> Toplevel.t post action
    | Eval : expr * route * semantic * (semantic*(expr->bool)) list -> Toplevel.t eval action

  (* Actions with hidden return type *)
  type xaction = Action : 'a action -> xaction


  (** Get the program location related to an action *)
  let action_range : type a. a action -> range = function
    | Exec(stmt,_)    -> stmt.srange
    | Eval(exp,_,_,_) -> exp.erange


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

    mutable callstack : callstack;
    (** Current call-stack *)

    mutable loc : range option;
    (** Last analyzed line of code *)

    mutable locstack : range option list;
    (** Stack of lines of codes *)

    mutable trace: xaction list;
    (** Trace of executed transfer functions *)

    mutable call_preamble : bool;
    (** Flag set when calling a function and reset when reaching its first loc *)

    mutable print_welcome : bool;
    (** Flag to print welcome message at the beginning *)

    mutable script: out_channel option;
    (** Optional file where all recorded commands will be written down *)
  }


  (** Global state *)
  let state = {
    breakpoints = BreakpointSet.empty;
    command = StepI;
    depth = 0;
    command_depth = 0;
    command_callstack = empty_callstack;
    callstack = empty_callstack;
    loc = None;
    locstack = [];
    trace = [];
    call_preamble = false;
    print_welcome = true;
    script = None;
  }

  (* Copy a state *)
  let copy_state () =
    { breakpoints = state.breakpoints;
      command = state.command;
      depth = state.depth;
      command_depth = state.command_depth;
      command_callstack = state.command_callstack;
      callstack = state.callstack;
      loc = state.loc;
      locstack = state.locstack;
      trace = state.trace;
      call_preamble = state.call_preamble;
      print_welcome = state.print_welcome;
      script = state.script
    }


  (** {2 Interaction detection} *)
  (** ************************* *)

  (* Test if the currest state corresponds to a function call *)
  let is_call old =
    callstack_begins_with state.callstack old

  (* Test if the currest state corresponds to a function return *)
  let is_return old =
    callstack_begins_with old state.callstack

  (* Test if an action corresponds to a new line of code *)
  let is_new_loc_action : type a. state -> a action -> bool = fun old action ->
    match action with
    | Eval _ -> false
    | Exec({skind = S_block _},_) -> false
    | Exec(stmt,_) ->
      let range = stmt.srange in
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
          | B_function _ -> false
        ) state.breakpoints


  (** Test if there is a breakpoint at a given function *)
  let is_function_breakpoint () =
    not (is_empty_callstack state.callstack)
    && ( let call = callstack_top state.callstack in
         BreakpointSet.exists
           (function
             | B_function f -> f = call.call_fun_orig_name
             | B_line _ -> false
           ) state.breakpoints )


  (* Check if the analyzer reached an interaction point *)
  let is_interaction_point old action =
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
      ( not (is_call state.command_callstack) ||
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
      ( is_return state.command_callstack ||
        is_range_breakpoint () ||
        is_function_breakpoint () )

    | _ -> false


  (** {2 Pretty printers} *)
  (** ******************* *)


  (* Get the number of digits of an integer *)
  let nb_digits n =
    int_of_float (log10 (float_of_int n)) + 1

  (* Right align an integer *)
  let pp_right_align_int width fmt i =
    let digits = nb_digits i in
    fprintf fmt "%s%d"
      (String.init (width - digits) (fun _ -> ' '))
      i

  let pp_right_align width pp fmt x =
    let s = asprintf "%a" pp x in
    let len = String.length s in
    fprintf fmt "%s%s"
      (String.init (width - len) (fun _ -> ' '))
      s

  (* Format has issues when identing in presence of unicode characters. So we
       do it manually. *)
  let fix_string_indentation indent s =
    let lines = String.split_on_char '\n' s in
    match lines with
    | [] -> ""
    | [_] -> s
    | hd::tl ->
      let lines' = hd :: List.map (fun l -> (String.make indent ' ') ^ "    " ^ l) tl in
      String.concat "\n" lines'

  let truncate_string s =
    let lines = String.split_on_char '\n' s in
    match lines with
    | [] | [_] -> s
    | hd::tl -> hd ^ " ..."

  let pp_exec ~truncate ~indent fmt stmt =
    let s = asprintf "@[<v>%a@]" pp_stmt stmt in
    fprintf fmt "%a %a %s %a"
      Debug.(color 45 pp_print_string) "𝕊"
      Debug.(color 45 pp_print_string) "⟦"
      (if truncate then truncate_string s else fix_string_indentation indent s)
      Debug.(color 45 pp_print_string) "⟧"

  let pp_eval ~truncate ~indent fmt exp =
    let s = asprintf "@[<v>%a@]" pp_expr exp in
    fprintf fmt "%a %a %s : %a %a"
      Debug.(color 209 pp_print_string) "𝔼"
      Debug.(color 209 pp_print_string) "⟦"
      (if truncate then truncate_string s else fix_string_indentation indent s)
      pp_typ exp.etyp
      Debug.(color 209 pp_print_string) "⟧"

  let pp_action : type a. ?truncate:bool -> ?indent:int -> (formatter -> a action -> unit)
  = fun ?(truncate=false) ?(indent=0) fmt action ->
    match action with
    | Exec(stmt,_) -> pp_exec ~truncate ~indent fmt stmt
    | Eval(exp,_,_,_) -> pp_eval ~truncate ~indent fmt exp

  let pp_trace fmt trace =
    let remove_new_lines s =
      Bytes.of_string s |>
      Bytes.map (function '\n' -> ' ' | x -> x) |>
      Bytes.to_string
    in
    let trace',n = List.fold_left (fun (acc,i) a -> (i,a)::acc,(i+1)) ([],0) trace in
    let max_digits = nb_digits n in
    fprintf fmt "@[<v>%a@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
         (fun fmt (i,Action a) ->
            let s = asprintf "@[<h>%a%a  @[%a@] at %a@]"
                (fun fmt () -> if i = 0 then () else fprintf fmt "@,") ()
                (pp_right_align (max_digits+1) pp_print_string) ("#" ^ (string_of_int (n-i-1)))
                (pp_action ~truncate:true ~indent:0) a
                pp_relative_range (action_range a) in
            let s' = remove_new_lines s in
            pp_print_string fmt s'
         )
      ) trace'
  (** Print source code of an action *)
  let pp_action_source_code fmt action =
    (* Entry point *)
    let rec doit () =
      let range = action_range action in
      if not (is_orig_range (untag_range range)) then () else
        let start = get_range_start range in
        let file = start.pos_file in
        let line = start.pos_line in
        if not (Sys.file_exists file) then ()
        else
          let ch = open_in file in
          let before,at,after = read_lines_around ch line in
          let max_line = line + List.length after in
          let max_digits = nb_digits max_line in
          List.iter (pp_surrounding_line max_digits std_formatter) before;
          pp_target_line max_digits std_formatter at;
          List.iter (pp_surrounding_line max_digits std_formatter) after;
          close_in ch
    (* Read lines before and after a target line *)
    and read_lines_around ch line =
      let rec iter before at after i =
        let lo = try Some (input_line ch) with End_of_file -> None in
        match lo with
        | Some l ->
          if i < line - 5 then iter before at after (i+1) else
          if i > line + 5 then (before,at,after)
          else
          if i < line then iter ((i,l)::before) at after (i+1) else
          if i = line then iter before (i,l) after (i+1)
          else iter before at ((i,l)::after) (i+1)
        | None -> (before,at,after) in
      let before, at, after = iter [] (0,"") [] 1 in
      List.rev before, at, List.rev after
    (* Print a surrounding line *)
    and pp_surrounding_line max_line fmt (i,l) =
      fprintf fmt "   %a  %s@."
        (pp_right_align_int max_line) i
        l
    (* Print the target line *)
    and pp_target_line max_line fmt (i,l) =
      fprintf fmt " %a %a  %a@."
        Debug.(color 118 pp_print_string) "►"
        Debug.(color 118 (pp_right_align_int max_line)) i
        Debug.(color 118 pp_print_string) l
    in
    doit ()


  (** Print the next action of the analyzer before asking user what to do *)
  let pp_interaction fmt action =
    (* First interaction => print welcome message *)
    if state.print_welcome then (
      state.print_welcome <- false;
      fprintf fmt "@.%a@.Type '%a' to get the list of commands.@.@."
        (Debug.bold pp_print_string) ("Welcome to Mopsa " ^ Version.version ^ "!")
        (Debug.bold pp_print_string) "help"
    )
    else (
      (* Print the range of the next action *)
      fprintf fmt "%a@." Debug.(color fushia pp_relative_range) (action_range action);
      (* Print location in the source code *)
      pp_action_source_code fmt action;
      (* Print interpreter action *)
      fprintf fmt "%a@." (pp_action ~truncate:true ~indent:0) action
    )

  (** Unique range for expressions/statements constructed by the interactive engine *)
  let interactive_range = tag_range (mk_fresh_range ()) "interactive"

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


  (** Get the variables appearing in an action *)
  let action_line_vars : type a. a action -> var list = fun action ->
    let range = action_range action in
    if not (is_orig_range range) then []
    else
      let line = get_range_line range in
      let visit_expr acc e =
        if not (is_orig_range e.erange) then VisitParts acc
        else
          let line' = get_range_line e.erange in
          if line = line' then Keep (acc@expr_vars e) else VisitParts acc
      in
      let visit_stmt acc s = VisitParts acc in
      match action with
      | Exec (stmt, _) ->
        fold_stmt visit_expr visit_stmt [] stmt

      | Eval (expr, _, _, _) ->
        fold_expr visit_expr visit_stmt [] expr


  (** Print value of variables *)
  let pp_vars action names man flow =
    if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then
      printf "⊥@."
    else
      let names =
        match names with
        | [] ->
          List.map (fun v -> asprintf "%a" pp_var v) (action_line_vars action)
        | _ -> names in 
      let vars = man.ask Q_defined_variables flow in
      let addrs = man.ask Q_allocated_addresses flow in
      let vmap =
        List.fold_left
              (fun acc v ->
                 let vname = asprintf "%a" pp_var v in
                 let old = OptionExt.default VarSet.empty
                     (MapExt.StringMap.find_opt vname acc) in
                 MapExt.StringMap.add vname (VarSet.add v old) acc)
          MapExt.StringMap.empty vars
      in
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
        ) ([],[],[]) names in
      let protect_print print_thunk =
        let oldv = !Core.Ast.Var.print_uniq_with_uid in
        Core.Ast.Var.print_uniq_with_uid := true;
        print_thunk ();
        Core.Ast.Var.print_uniq_with_uid := oldv;
      in
      let not_found' =
        let printer = empty_printer () in
        let found,not_found =
          List.fold_left
            (fun (found,not_found) v ->
               try
                 let () = 
                   if VarSet.cardinal @@ OptionExt.default VarSet.empty (MapExt.StringMap.find_opt (asprintf "%a" pp_var v) vmap) > 1 then
                     protect_print (fun () -> man.print_expr flow printer (mk_var v interactive_range))
                   else 
                     man.print_expr flow printer (mk_var v interactive_range) in
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
                 let () = man.print_expr flow printer (mk_addr a interactive_range) in
                 true,not_found
               with Not_found ->
                 let aname = asprintf "%a" pp_addr a in
                 found,aname::not_found
            ) (found,not_found) afound
        in
        if found then printf "%a@." pflush printer;
        not_found
      in
      ( match not_found@not_found' with
        | [] -> ()
        | l  ->
          printf "Variable%a %a not found@."
            Debug.plurial_list not_found
            (pp_print_list
               ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
               (fun fmt vname -> fprintf fmt "'%s'" vname)
            ) l
      )


  (** {2 Interactive engine} *)
  (** ********************** *)

  (** Apply an action on a flow and return its result *)
  let rec apply_action : type a. a action -> t flow -> a =
    fun action flow ->
    match action with
    | Exec(stmt, route) ->
      state.depth <- state.depth + 1;
      let ret = Toplevel.exec ~route stmt man flow in
      state.depth <- state.depth - 1;
      ret
    | Eval(exp, route, translate, translate_when)  ->
      state.depth <- state.depth + 1;
      let ret = Toplevel.eval ~route ~translate ~translate_when exp man flow in
      state.depth <- state.depth - 1;
      ret

(** Wait for user input and process it *)
  and interact: type a. a action -> t flow -> a = fun action flow ->
    let logger = fun cmd_str ->
      if List.mem cmd_str ["us"; "unset script"; "unset s"] then () else
      (* todo: currently logs unset script, whoops *)
        match state.script with
        | None -> ()
        | Some ch ->
          let file_fmt = formatter_of_out_channel ch in
          Format.fprintf file_fmt "%s@." cmd_str
          (* todo: remove last @. ... *)
    in
    let cmd = try read_command logger
              with Exit -> exit 0
    in
    state.command <- cmd;
    state.command_depth <- state.depth;
    state.command_callstack <- (Flow.get_callstack flow);
    let range = action_range action in
    match cmd with
    | MopsaBackTrace ->
       Printexc.print_raw_backtrace Stdlib.stdout (Printexc.get_callstack Int.max_int);
       interact action flow

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

    | BackTrace ->
      let cs = Flow.get_callstack flow in
      printf "%a@." pp_callstack cs;
      interact action flow

    | Trace ->
      printf "@[<v>%a@]@." pp_trace state.trace;
      interact action flow

    | Print names ->
      pp_vars action names man flow;
      interact action flow

    | Env [] ->
      let env = Flow.get T_cur man.lattice flow in
      printf "%a@." (Print.format man.lattice.print) env;
      interact action flow

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
      printf "%a@." pp_print_object pobj';
      interact action flow

    | Where ->
      pp_action_source_code std_formatter action;
      printf "%a@." Debug.(color fushia pp_relative_range) (action_range action);
      printf "%a@." (pp_action ~truncate:false ~indent:0) action;
      interact action flow

    | Enable (Hook hook) ->
      if not (Hook.mem_hook hook) then (
        printf "Hook '%s' not found@." hook;
        interact action flow
      ) else (
        Hook.activate_hook hook;
        let ctx = Hook.init_hook hook (Flow.get_ctx flow) in
        let flow = Flow.set_ctx ctx flow in
        interact action flow
      )

    | Disable (Hook hook) ->
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
      ( if is_safe_report report
        then printf "%a No alarm@." Debug.(color_str green) "✔";
        let _ = Output.Text.construct_checks_summary ~print:true report None in
        ()
      );
      interact action flow

    | Info Checks ->
      let report = Flow.get_report flow in
      let total, safe, error, warning, checks_map = Output.Text.construct_checks_summary report None in
      Output.Text.print_checks_summary checks_map total safe error warning None;
      interact action flow

    | Info Breakpoints ->
      printf "%a@." pp_breakpoint_set state.breakpoints;
      interact action flow

    | Info Variables ->
      let vars = man.ask Q_defined_variables flow in
      printf "@[<v>%a@]@."
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt v -> pp_var_with_type fmt (v,v.vtyp))
        ) vars
      ;
      interact action flow

    | Info Context ->
      let ctx = Flow.get_ctx flow in
      printf "%a@." (pp_ctx man.lattice.print) ctx;
      interact action flow

    | Set (Debug, channel) ->
      Debug.set_channels channel;
      interact action flow

    | Unset Debug ->
      Debug.set_channels "";
      interact action flow

    | Set (Script, filename) ->
      let ch = open_out filename in 
      state.script <- Some ch;
      interact action flow

    | Unset Script ->
      let () = match state.script with
      | None -> ()
      | Some ch ->
        close_out ch;
        state.script <- None in
      interact action flow

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
      List.iter (fun l -> Queue.add l Command.commands_buffer) lines;
      close_in ch;
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
      let old = copy_state () in
      state.depth <- state.depth + 1;
      state.callstack <- Flow.get_callstack flow;
      let trace = state.trace in
      state.trace <- (Action action) :: trace;
      (* When entering a function, we push the old loc to locstack, so that we
         can retrieve it when returning from the function to check if we
         encounter a new loc after the call. *)
      ( if is_call old.callstack then
          ( state.call_preamble <- true;
            state.locstack <- old.loc :: old.locstack )
        else
        (* When returning from a function, we pop the loc from the stack,
           and we consider it as the old loc *)
        if is_return old.callstack then
          ( state.call_preamble <- false;
            match old.locstack with
            | []     ->
              old.loc <- None;
              state.loc <- None
            | hd::tl ->
              old.loc <- hd;
              state.loc <- hd;
              state.locstack <- tl ) );
      (* Check if we reached a new loc *)
      let new_loc = is_new_loc_action old action in
      ( if new_loc then
          let range = action_range action in
          state.loc <- Some range
      );
      (* Check if we reached an interaction point *)
      let interaction = is_interaction_point old action in
      (* Reset call flag if we reached a new loc *)
      if new_loc then state.call_preamble <- false;
      let ret =
        if interaction then (
          pp_interaction std_formatter action;
          interact action flow
        ) else
          apply_action action flow
      in
      state.depth <- state.depth - 1;
      state.trace <- trace;
      ret
    with Sys.Break ->
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
    get_effects = (fun e -> e);
    set_effects = (fun e _ -> e);
    exec = exec;
    eval = eval;
    ask = ask;
    print_expr = print_expr;
  }


end
