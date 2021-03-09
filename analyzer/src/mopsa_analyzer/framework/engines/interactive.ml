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
type ('a,_) query += Q_debug_variables : ('a,var list) query


(** Query to retrieve the value of a given variable *)
type ('a,_) query += Q_debug_variable_value : var -> ('a,var_value) query


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
  pp_print_option (Debug.color_str Debug.blue) fmt v.var_value;
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

    | WhereI
    (** Show current interpreter transfer function *)

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

    | Trace
    (** Print the analysis trace *)

    | Save of string
    (** Save the environment in a file *)


  (** Information sub-commands *)
  and info_command =
    | Alarms
    | Breakpoints
    | Tokens
    | Variables

  (** Enable/Disable sub-commands *)
  and enable_command =
    | Hook of string

  (** Set/Unset sub-commands *)
  and set_command =
    | Debug


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
    | WhereI       -> Format.pp_print_string fmt "wherei"
    | Info Alarms      -> Format.fprintf fmt "info alarms"
    | Info Breakpoints -> Format.fprintf fmt "info breakpoints"
    | Info Tokens      -> Format.fprintf fmt "info tokens"
    | Info Variables   -> Format.fprintf fmt "info variables"
    | Enable (Hook h)  -> Format. fprintf fmt "enable hook %s" h
    | Disable (Hook h) -> Format. fprintf fmt "disable hook %s" h
    | Set (Debug, d)  -> Format. fprintf fmt "set debug %s" d
    | Unset Debug -> Format. fprintf fmt "unset debug"
    | BackTrace   -> Format.fprintf fmt "backtrace"
    | Save file   -> Format.fprintf fmt "save %s" file
    | Trace   -> Format.fprintf fmt "trace"


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
    printf "  t[race]               print the analysis trace@.";
    printf "  w[here]               show current program point@.";
    printf "  w[here]i              show current interpreter transfer function@.";
    printf "  i[info] a[larms]      print the list of alarms@.";
    printf "  i[info] b[reakpoints] print the list of breakpoints@.";
    printf "  i[info] t[okens]      print the list of flow tokens@.";
    printf "  i[info] v[ariables]   print the list of variables@.";
    printf "  e[nable] h[hook] <h>  enable a hook@.";
    printf "  d[isable] h[hook] <h> disable a hook@.";
    printf "  s[et] d[ebug] <d>     set debug channels@.";
    printf "  u[nset] d[ebug]       unset debug channels@.";
    printf "  save <file>           save the abstract state in a file@.";
    printf "  help                  print this message@.";
    ()


  (** Print input prompt *)
  let print_prompt () =
    printf "%a %a @?"
      Debug.(color_str teal) "mopsa"
      Debug.(color_str green) ">>"


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
      | ["wherei"   |"wi"]   -> WhereI
      | ["backtrace"|"bt"]   -> BackTrace
      | ["trace"    | "t"]   -> Trace
      | ["break"    | "b"; loc] -> Break loc
      | ["print"    | "p"; var] -> PrintVar var

      | ["help" | "h"]   ->
        print_usage ();
        read_command ()


      | ["info" |"i"; "tokens"      | "t"] | ["it"] -> Info Tokens
      | ["info" |"i"; "breakpoints" | "b"] | ["ib"] -> Info Breakpoints
      | ["info" |"i"; "alarms"      | "a"] | ["ia"] -> Info Alarms
      | ["info" |"i"; "variables"   | "vars" | "var"  | "v"] | ["iv"] -> Info Variables

      | ["enable" |"e";  "hook" | "h"; h] | ["eh"; h] -> Enable (Hook h)
      | ["disable"|"d";  "hook" | "h"; h] | ["dh"; h] -> Disable (Hook h)

      | ["set"  |"s";  "debug"| "d"; d] | ["sd"; d] -> Set (Debug, d)
      | ["unset"|"u";  "debug"| "d"] | ["ud"] -> Unset Debug

      | [""] ->
        ( match !last_command with
          | None ->  read_command ()
          | Some c -> c )

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
      print_welcome = state.print_welcome; }


  (** {2 Interaction detection} *)
  (** ************************* *)

  (* Test if the currest state corresponds to a function call *)
  let is_call old =
    callstack_begins_with state.callstack old

  (* Test if the currest state corresponds to a function return *)
  let is_return old =
    callstack_begins_with old state.callstack

  (* Test if an action corresponds to a new line of code *)
  let is_new_loc : type a. state -> a action -> bool = fun old action ->
    match action with
    | Eval _ -> false
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

  (** Test there is a breakpoint at a given program location *)
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
    | NextI -> state.depth <= state.command_depth

    (* [Step] stops at any new line of codes *)
    | Step ->
      is_new_loc old action

    (* [Next] stops at new lines of codes that are not in inner calls (unless if
       there is a breakpoint) *)
    | Next ->
      is_new_loc old action &&
      ( not (is_call state.command_callstack) ||
        is_range_breakpoint () ||
        ( state.call_preamble && is_function_breakpoint () ) )

    (* [Continue] stops at new lines of code with attached breakpoints *)
    | Continue ->
      is_new_loc old action &&
      ( is_range_breakpoint () ||
        ( state.call_preamble && is_function_breakpoint () ) )

    (* [Finish] stops at new lines of code after function return or at breakpoints *)
    | Finish ->
      is_new_loc old action &&
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

  let pp_action : type a. ?truncate:bool -> (formatter -> a action -> unit) = fun ?(truncate=false) fmt action ->
    (* Format has issues when identing in presence of unicode characters. So we
       do it manually. *)
    let fix_string_indentation s =
      let lines = String.split_on_char '\n' s in
      match lines with
      | [] | [_] -> s
      | hd::tl ->
        let lines' = hd :: List.map (fun l -> "    " ^ l) tl in
        String.concat "\n" lines'
    in
    let truncate_string s =
      let lines = String.split_on_char '\n' s in
      match lines with
      | [] | [_] -> s
      | hd::tl -> hd ^ " ..."
    in
    match action with
    | Exec(stmt,route) ->
      let s = asprintf "@[<v>%a@]" pp_stmt stmt in
      fprintf fmt "%a %a %s %a@."
        Debug.(color 45 pp_print_string) "𝕊"
        Debug.(color 45 pp_print_string) "⟦"
        (if truncate then truncate_string s else fix_string_indentation s)
        Debug.(color 45 pp_print_string) "⟧"

    | Eval(exp,route,translate,translate_when) ->
      let s = asprintf "@[<v>%a@]" pp_expr exp in
      fprintf fmt "%a %a %s : %a %a@."
        Debug.(color 209 pp_print_string) "𝔼"
        Debug.(color 209 pp_print_string) "⟦"
        (if truncate then truncate_string s else fix_string_indentation s)
        pp_typ exp.etyp
        Debug.(color 209 pp_print_string) "⟧"

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
                (pp_action ~truncate:true) a
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
        try
          let l = input_line ch in
          if i < line - 5 then iter before at after (i+1) else
          if i > line + 5 then (before,at,after)
          else
            if i < line then iter ((i,l)::before) at after (i+1) else
            if i = line then iter before (i,l) after (i+1)
            else iter before at ((i,l)::after) (i+1)
        with End_of_file -> (before,at,after)
      in
      let before,at,after = iter [] (0,"") [] 1 in
      List.rev before, at, List.rev after
    (* Print a surrounding line *)
    and pp_surrounding_line max_line fmt (i,l) =
      fprintf fmt "   %a  %s@."
        (pp_right_align_int max_line) i
        l
    (* Print the target line *)
    and pp_target_line max_line fmt (i,l) =
      fprintf fmt " %a %a  %a@."
        Debug.(color 78 pp_print_string) "►"
        Debug.(color 78 (pp_right_align_int max_line)) i
        Debug.(color 78 pp_print_string) l
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
      match state.command with
      (* For source-level navigation, show a listing of the program *)
      | Next | Step | Continue | Finish | Where ->
        pp_action_source_code fmt action

      (* For interpreter-level navigation, show next transfer function *)
      | NextI | StepI | WhereI ->
        pp_action fmt action

      | _ -> assert false
    )


  (** {2 Interactive engine} *)
  (** ********************** *)

  (** Apply an action on a flow and return its result *)
  let rec apply_action : type a. a action -> Toplevel.t flow -> a =
    fun action flow ->
    match action with
    | Exec(stmt, route) -> Toplevel.exec ~route stmt man flow
    | Eval(exp, route, translate, translate_when)  -> Toplevel.eval ~route ~translate ~translate_when exp man flow


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

    | BackTrace ->
      let cs = Flow.get_callstack flow in
      printf "%a@." pp_callstack cs;
      interact action flow

    | Trace ->
      printf "@[<v>%a@]@." pp_trace state.trace;
      interact action flow

    | Print ->
      printf "%a@." (format (Flow.print man.lattice.print)) flow;
      interact action flow

    | Env ->
      let env = Flow.get T_cur man.lattice flow in
      printf "%a@." (Print.format man.lattice.print) env;
      interact action flow

    | Where ->
      fprintf std_formatter "%a@." Debug.(color fushia pp_relative_range) (action_range action);
      pp_action_source_code std_formatter action;
      interact action flow

    | WhereI ->
      fprintf std_formatter "%a@." Debug.(color fushia pp_relative_range) (action_range action);
      pp_action std_formatter action;
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
      let vars = man.ask Q_debug_variables flow in
      printf "@[<v>%a@]@."
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt v -> pp_var_with_type fmt (v,v.vtyp))
        ) vars
      ;
      interact action flow

    | PrintVar vname ->
      let vars = man.ask Q_debug_variables flow in
      begin try
          let var = List.find
              (fun var' ->
                 let vname' = Format.asprintf "%a" pp_var var' in
                 vname = vname'
              ) vars
          in
          let value = man.ask (Q_debug_variable_value var) flow in
          printf "%a = %a@." pp_var_with_type (var,value.var_value_type) pp_var_value value;
          interact action flow
        with Not_found ->
          printf "Variable '%s' not found@." vname;
          interact action flow
      end

    | Set (Debug, channel) ->
      Debug.set_channels channel;
      interact action flow

    | Unset Debug ->
      Debug.set_channels "";
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
      (* When entering a functio, we pusth the old loc to locstack, so that we
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
      let new_loc = is_new_loc old action in
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
