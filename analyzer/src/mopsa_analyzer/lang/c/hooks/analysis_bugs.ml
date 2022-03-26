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

(** Hook to track some bug patterns in C analysis

    The hook captures assignments and calls to stubbed functions. It
    checks that if such statement is reachable its post-state should
    not be empty. Otherwise, this seems to be a soundness bug in some
    transfer function.
*)

open Mopsa
open Format
open Ast
open Stubs.Ast


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "c-analysis-bugs"
  let debug fmt = Debug.debug ~channel:name fmt


  (** {2 Command-line options} *)
  (** ************************ *)

  (** Whitelist of functions that do not return *)
  let default_whitelist = [
      "exit";
      "_exit";
      "quick_exit";
      "_Exit";
      "abort";
      "_exit";
      "__builtin_abort";
      "__builtin_unreachable";
      "__assert_fail";
      "__assert_perror_fail";
      "__assert";
      "_mopsa_error";
      "_mopsa_error_at_line";
      "sigreturn";
      "error";
      "error_at_line";
    ]

  let opt_whitelist = ref []

  let () = register_language_option "C" {
      key = "-c-analysis-bugs-whitelist";
      doc = "whitelist of non-terminating functions";
      spec = ArgExt.Set_string_list opt_whitelist;
      category = "C Hooks";
      default = "";
    }

  let mem_whitelist f =
    List.mem f default_whitelist ||
    List.mem f !opt_whitelist


  (** {2 Analyzer bugs} *)
  (** ***************** *)

  type bug = {
    bug_kind : bug_kind;
    bug_range : range;
    bug_callstack : callstack;
  }

  and bug_kind =
    | Exec of stmt
    | Call of expr


  (** Compare two bug kinds *)
  let compare_bug_kind bk1 bk2 =
    match bk1, bk2 with
    | Exec s1, Exec s2 -> compare_stmt s1 s2
    | Call e1, Call e2 -> compare_expr e1 e2
    | _ -> compare bk1 bk2


  (** Compare two bugs *)
  let compare_bug b1 b2 =
    Compare.triple compare_range compare_callstack compare_bug_kind
      (b1.bug_range,b1.bug_callstack,b1.bug_kind)
      (b2.bug_range,b2.bug_callstack,b2.bug_kind)


  (** Print bug kinds *)
  let pp_bug_kind fmt = function
    | Exec stmt -> pp_stmt fmt stmt
    | Call e -> pp_expr fmt e


  (** Print a bug *)
  let pp_bug fmt bug =
    fprintf fmt "%a %a: %a@\n  Trace:@\n%a"
      Debug.(color_str orange) "⚠"
      pp_relative_range bug.bug_range
      pp_bug_kind bug.bug_kind
      pp_callstack bug.bug_callstack


  (** Set of bugs *)
  module BugSet = SetExt.Make
      (struct
        type t = bug
        let compare = compare_bug
      end)


  let bugs = ref BugSet.empty

  (** Add a bug at a statement *)
  let add_exec_bug stmt cs =
    let bug = {bug_range = stmt.srange; bug_kind = Exec stmt; bug_callstack = cs} in
    debug "%a" pp_bug bug;
    bugs := BugSet.add bug !bugs


  (** Add a bug at a function call *)
  let add_call_bug exp cs =
    let bug = {bug_range = exp.erange; bug_kind = Call exp; bug_callstack = cs} in
    debug "%a" pp_bug bug;
    bugs := BugSet.add bug !bugs


  (** Remove redundant bug locations *)
  let remove_redundant_bugs () =
    bugs := BugSet.filter
        (fun b ->
           BugSet.for_all
             (fun b' -> b.bug_range == b'.bug_range
                        || (not (subset_range b'.bug_range b.bug_range)
                            &&  not (callstack_begins_with b'.bug_callstack b.bug_callstack))
             ) !bugs
        ) !bugs


  (** {2 Initialization} *)
  (** ****************** *)

  let init ctx = ()


  (** {2 Utility functions} *)
  (** ********************* *)

  (** Check that T_cur is bottom in a flow *)
  let is_cur_bottom_in_flow man flow =
    man.lattice.is_bottom (Flow.get T_cur man.lattice flow)

  (** Check that T_cur is bottom in a cases set *)
  let is_cur_bottom_in_cases man cases =
    Cases.for_all (fun _ flow -> is_cur_bottom_in_flow man flow) cases


  (** {2 Events handlers} *)
  (** ******************* *)

  let on_before_exec route stmt man flow = ()

  let on_after_exec route stmt man flow post =
    match skind stmt with
    | S_c_declaration _
    | S_assign _
    | S_stub_directive _
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man post
      ->
      add_exec_bug stmt (Flow.get_callstack flow)

    | _ -> ()


  let on_before_eval route semantic exp man flow = ()

  let on_after_eval route semantic exp man flow evl =
    match ekind exp with
    | E_stub_call (f,_)
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man evl
        && not (mem_whitelist f.stub_func_name)
      ->
      add_call_bug exp (Flow.get_callstack flow)

    | E_c_builtin_call (f,_)
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man evl
        && not (mem_whitelist f)
      ->
      add_call_bug exp (Flow.get_callstack flow)

    | _ -> ()
    

  let on_finish man flow =
    let () = remove_redundant_bugs () in
    if BugSet.is_empty !bugs then
      printf "No potential analyzer bug detected@\n@."
    else
     let nb = BugSet.cardinal !bugs in
      printf "%d potential analyzer bug%a detected:@.  @[<v>%a@]@\n@."
        nb
        Debug.plurial_int nb
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,@,") pp_bug) (BugSet.elements !bugs)

end

let () =
  Core.Hook.register_stateless_hook (module Hook)
