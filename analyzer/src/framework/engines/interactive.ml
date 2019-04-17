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

(** Engine of an interactive analysis *)

open Ast
open Ast.Stmt
open Ast.Expr
open Core
open Lattice
open Token
open Flow
open Manager
open Eval
open Post
open Query
open Zone
open Abstraction
open Engine
open Format

(** Query for printing variables *)
type _ query += Q_print_var : (Format.formatter -> string -> unit) query

let () =
  register_query {
    query_join = (
      let doit : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_print_var ->
            fun fmt var ->
              Format.fprintf fmt "%a@,%a" a var b var
          | _ -> next.join query a b
      in
      doit
    );

    query_meet = (
      let doit : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_print_var ->
            fun fmt var ->
              Format.fprintf fmt "%a@,%a" a var b var
          | _ -> next.join query a b
      in
      doit
    );
  }



(** Create an interactive analysis engine over an abstraction. *)
module Make(Abstraction : ABSTRACTION) : ENGINE with type t = Abstraction.t =
struct

  type t = Abstraction.t

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
    | Value of string (** Print the value of a variable *)
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
    printf "  e[nv] <var>      print the value of a variable in the current abstract environment@.";
    printf "  w[here]          show current program point@.";
    printf "  l[og] {on|off}   activate/deactivate short logging@.";
    printf "  llog {on|off}   activate/deactivate complete logging@.";
    printf "  h[elp]           print this message@.";
    ()

  let print_prompt range () =
    printf "%a %a @?"
      (Debug.color "lightblue" Location.pp_range) range
      (Debug.color "green" pp_print_string) ">>"

  let linedit_ctx = LineEdit.create_ctx ()

  (** Read a debug command *)
  let rec read_command range () =
    print_prompt range ();
    let l =  LineEdit.read_line linedit_ctx |> String.trim in
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

      | "log on" | "l on" ->
        Core.Debug_tree.opt_short_log := true;
        read_command range ()

      | "log off" | "l off" ->
        Core.Debug_tree.opt_short_log := false;
        read_command range ()

      | "llog on" ->
        Core.Debug_tree.opt_log := true;
        read_command range ()

      | "llog off" ->
        Core.Debug_tree.opt_log := false;
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

        else
        if Str.string_match (Str.regexp "\\(e\\|env\\) \\(.*\\)") l 0
        then (
          let var = Str.matched_group 2 l in
          Value var
        )

        else (
          printf "Unknown command %s@." l;
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

  (** Catch Ctrl+C interrupts as a Break exception*)
  let () = Sys.catch_break true


  (** Interpreter actions *)
  type _ action =
    | Exec : stmt * zone -> Abstraction.t flow action
    | Post : stmt * zone -> Abstraction.t post action
    | Eval : expr * (zone * zone) * zone -> (expr, Abstraction.t) eval action


  (** Print the current analysis action *)
  let pp_action : type a. formatter -> a action * Abstraction.t flow -> unit = fun fmt (action, flow) ->
    match action with
    | Exec(stmt,zone) ->
      fprintf fmt "@[<v 4>S[ %a@] ] in zone %a@."
        pp_stmt stmt
        pp_zone zone

    | Post(stmt,zone) ->
      fprintf fmt "@[<v 4>P[ %a@] ] in zone %a@."
        pp_stmt stmt
        pp_zone zone

    | Eval(exp,zone,_) ->
      fprintf fmt "@[<v 4>E[ %a@] ] in zone %a@."
        pp_expr exp
        pp_zone2 zone


  (** Apply an action on a flow and return its result *)
  let rec apply_action : type a. a action -> Abstraction.t flow -> a =
    fun action flow ->
      match action with
      | Exec(stmt, zone) -> Abstraction.exec ~zone stmt man flow
      | Post(stmt, zone) -> Abstraction.post ~zone stmt man flow
      | Eval(exp, zone, via) -> Abstraction.eval ~zone ~via exp man flow


  (** Interact with the user input *)
  and interact : type a. ?where:bool -> a action -> Location.range -> Abstraction.t flow -> a =
    fun ?(where=true) action range flow ->
      if not !break then (
        (* Search for a breakpoint at current location *)
        match find_breakpoint_at range with
        | None ->
          (* No breakpoint here *)
          ()

        | Some brk ->
          (* Check if brk is different than the current breakpoint *)
          if compare_breakpoint brk !last_breakpoint <> 0 then (
            break := true;
            last_breakpoint := brk;
          )
      );

      try

        if not !break
        then apply_action action flow

        else (
          if where then pp_action std_formatter (action, flow);

          let cmd =
            try read_command range ()
            with Exit -> exit 0
          in

          match cmd with
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
            printf "%a@." (Flow.print man.lattice) flow;
            interact ~where:false action range flow

          | Env ->
            let env = Flow.get T_cur man.lattice flow in
            printf "%a@." man.lattice.print env;
            interact ~where:false action range flow

          | Value(var) ->
            let old_break = !break in
            break := false;
            printf "%a@." (man.ask Q_print_var flow) var;
            break := old_break;
            interact ~where:false action range flow

          | Where ->
            pp_action std_formatter (action, flow);
            interact ~where:false action range flow
        )

      with Sys.Break ->
        break := true;
        interact ~where:true action range flow

  and init prog =
    Abstraction.init prog man

  and exec ?(zone=any_zone) stmt flow =
    interact (Exec (stmt, zone)) stmt.srange flow

  and post ?(zone=any_zone) stmt flow =
    interact (Post (stmt,zone)) stmt.srange flow

  and eval ?(zone=(any_zone, any_zone)) ?(via=any_zone) exp flow =
    interact (Eval (exp, zone, via)) exp.erange flow

  and ask : type r. r query -> Abstraction.t flow -> r =
    fun query flow ->
      Abstraction.ask query man flow

  and lattice : Abstraction.t lattice = {
    bottom = Abstraction.bottom;
    top = Abstraction.top;
    is_bottom = (fun a -> Abstraction.is_bottom man a);
    subset = (fun a a' -> Abstraction.subset man a a');
    join = (fun a a' -> Abstraction.join man a a');
    meet = (fun a a' -> Abstraction.meet man a a');
    widen = (fun ctx a a' -> Abstraction.widen man ctx a a');
    print = (fun fmt a -> Abstraction.print man fmt a);
  }

  and man : (Abstraction.t, Abstraction.t) man = {
    lattice;
    get = (fun flow -> flow);
    set = (fun flow _ -> flow);
    get_log = (fun log -> log);
    set_log = (fun log _ -> log);
    exec = exec;
    post = post;
    eval = eval;
    ask = ask;
  }

end
