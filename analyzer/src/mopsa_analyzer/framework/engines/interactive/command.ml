(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2021 The MOPSA Project.                               *)
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

(** Commands of the interactive engine *)

open Mopsa_utils
open Format


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

  | Print of string list
  (** Print the current abstract state or the value of variables *)

  | Env of string list
  (** Print the current abstract environment, associated to token T_cur,
      eventually projected on a list of domains *)

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

  | Trace
  (** Print the analysis trace *)

  | Save of string
  (** Save the environment in a file *)


(** Information sub-commands *)
and info_command =
  | Alarms
  | Checks
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


(** Print a command *)
let pp_command fmt = function
  | Break loc   -> Format.fprintf fmt "break %s" loc
  | Continue    -> Format.pp_print_string fmt "continue"
  | Next        -> Format.pp_print_string fmt "next"
  | Finish      -> Format.pp_print_string fmt "finish"
  | NextI       -> Format.pp_print_string fmt "nexti"
  | Step        -> Format.pp_print_string fmt "step"
  | StepI       -> Format.pp_print_string fmt "stepi"
  | Print []    -> Format.pp_print_string fmt "print"
  | Print vars  -> Format.fprintf fmt "print %a"
                     (pp_print_list
                        ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
                        pp_print_string
                     ) vars
  | Env []           -> Format.pp_print_string fmt "env"
  | Env domains      -> Format.fprintf fmt "env %a"
                          (pp_print_list
                             ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
                             pp_print_string
                          ) domains
  | Where            -> Format.pp_print_string fmt "where"
  | Info Alarms      -> Format.pp_print_string fmt "info alarms"
  | Info Checks      -> Format.pp_print_string fmt "info checks"
  | Info Breakpoints -> Format.pp_print_string fmt "info breakpoints"
  | Info Tokens      -> Format.pp_print_string fmt "info tokens"
  | Info Variables   -> Format.pp_print_string fmt "info variables"
  | Info Context     -> Format.pp_print_string fmt "info context"
  | Enable (Hook h)  -> Format.fprintf fmt "enable hook %s" h
  | Disable (Hook h) -> Format.fprintf fmt "disable hook %s" h
  | Set (Debug, d)   -> Format.fprintf fmt "set debug %s" d
  | Unset Debug      -> Format.pp_print_string fmt "unset debug"
  | BackTrace        -> Format.pp_print_string fmt "backtrace"
  | Save file        -> Format.fprintf fmt "save %s" file
  | Trace            -> Format.pp_print_string fmt "trace"


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
  printf "  p[rint]               print the abstract state@.";
  printf "  p[rint] <var>,...     print the value of selected variables@.";
  printf "  e[nv]                 print the current abstract environment@.";
  printf "  e[nv] <domain>,...    print the current abstract environment of selected domains@.";
  printf "  b[ack]t[race]         print the current call stack@.";
  printf "  t[race]               print the analysis trace@.";
  printf "  w[here]               show current program point@.";
  printf "  w[here]i              show current interpreter transfer function@.";
  printf "  i[info] a[larms]      print the list of detected alarms@.";
  printf "  i[info] c[hecks]      print the list of performed checks@.";
  printf "  i[info] b[reakpoints] print the list of registered breakpoints@.";
  printf "  i[info] t[okens]      print the list of flow tokens@.";
  printf "  i[info] v[ariables]   print the list of variables@.";
  printf "  i[info] c[on]t[e]x[t] print the flow-insensitive context@.";
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


(** Reference to the last commands read from the prompt *)
let last_prompt_commands = ref []

(** Buffer of upcoming commands to execute *)
let commands_buffer = Queue.create ()

(* Read the next command as a string *)
let rec read_command_string () =
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
      read_command_string ()

    | [], _ ->
      (* No command entered, but we know the last command, so replay it *)
      let () = List.iter (fun c -> Queue.add c commands_buffer) !last_prompt_commands in
      read_command_string ()

    | _ ->
      (* Some commands entered. Save them in [last_prompt_commands],
         initialize [commands_buffer] and iterate again *)
      last_prompt_commands := parts;
      List.iter (fun c -> Queue.add c commands_buffer) parts;
      read_command_string ()

(** Read the next command *)
let rec read_command () =
  let s = read_command_string () in
  (* Get command's parts *)
  let parts = String.split_on_char ' ' s |>
              List.map String.trim |>
              List.filter (function "" -> false | _ -> true)
  in
  match parts with
  | ["continue" | "c"]   -> Continue
  | ["next"     | "n"]   -> Next
  | ["step"     | "s"]   -> Step
  | ["finish"   | "f"]   -> Finish
  | ["nexti"    |"ni"]   -> NextI
  | ["stepi"    |"si"]   -> StepI
  | ["where"    | "w"]   -> Where
  | ["backtrace"|"bt"]   -> BackTrace
  | ["trace"    | "t"]   -> Trace
  | ["break"    | "b"; l]-> Break l

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

  | ("print"    | "p") :: vars ->
    let vars =
      List.fold_left
        (fun acc s ->
           let parts = String.split_on_char ',' s |>
                       List.filter (function "" -> false | _ -> true)
           in
           SetExt.StringSet.union acc (SetExt.StringSet.of_list parts)
        ) SetExt.StringSet.empty vars
    in
    Print (SetExt.StringSet.elements vars)

  | ["help" | "h"]   ->
    print_usage ();
    read_command ()

  | ["info" |"i"; "tokens"      | "t"] | ["it"] -> Info Tokens
  | ["info" |"i"; "breakpoints" | "b"] | ["ib"] -> Info Breakpoints
  | ["info" |"i"; "alarms"      | "a"] | ["ia"] -> Info Alarms
  | ["info" |"i"; "checks"      | "c"] | ["ic"] -> Info Checks
  | ["info" |"i"; "variables"   | "vars" | "var"  | "v"] | ["iv"] -> Info Variables
  | ["info" |"i"; "context"     | "ctx"] | ["ictx"] -> Info Context

  | ["enable" | "en"; "hook" | "h"; h] | ["eh"; h] -> Enable (Hook h)
  | ["disable"|"d";   "hook" | "h"; h] | ["dh"; h] -> Disable (Hook h)

  | ["set"  |"s";  "debug"| "d"; d] | ["sd"; d] -> Set (Debug, d)
  | ["unset"|"u";  "debug"| "d"] | ["ud"] -> Unset Debug

  | ["save"; file] -> Save file

  | _ ->
    printf "Unknown command %s@." s;
    print_usage ();
    read_command ()
