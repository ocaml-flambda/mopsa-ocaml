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

open Location
open Mopsa
open Framework.Core.Sig.Domain.Manager
open Format
open Ast
open Stubs.Zone
open Stubs.Ast
open Zone


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "c-analysis-bugs"
  let exec_zones = [Z_c]
  let eval_zones = [Z_stubs,Z_any]


  (** {2 Analyzer bugs} *)
  (** ***************** *)

  type bug = {
    bug_kind : bug_kind;
    bug_range : range;
    bug_callstack : callstack;
  }

  and bug_kind =
    | Assign of stmt
    | Call   of string


  (** Compare two bug kinds *)
  let compare_bug_kind bk1 bk2 =
    match bk1, bk2 with
    | Assign s1, Assign s2 -> compare_stmt s1 s2
    | Call f1, Call f2 -> compare f1 f2
    | _ -> compare bk1 bk2


  (** Compare two bugs *)
  let compare_bug b1 b2 =
    Compare.triple compare_range compare_callstack compare_bug_kind
      (b1.bug_range,b1.bug_callstack,b1.bug_kind)
      (b2.bug_range,b2.bug_callstack,b2.bug_kind)


  (** Print bug kinds *)
  let pp_bug_kind fmt = function
    | Assign stmt -> pp_stmt fmt stmt
    | Call f -> pp_print_string fmt f


  (** Print a bug *)
  let pp_bug fmt bug =
    fprintf fmt "%a %a: %a@,  Trace:@,%a"
      (Debug.color_str "orange") "⚠"
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

  (** Whitelist of stub functions that do not return *)
  let whitelist = [ "exit";
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
                  ]


  (** Add a bug at an assignment *)
  let add_assign_bug stmt range cs =
    warn_at range "potential bug detected in %a" pp_stmt stmt;
    bugs := BugSet.add {bug_range = range; bug_kind = Assign stmt; bug_callstack = cs} !bugs


  (** Add a bug at a function call *)
  let add_call_bug f range cs =
    if not (List.mem f whitelist) then warn_at range "potential bug detected in %s" f;
    bugs := BugSet.add {bug_range = range; bug_kind = Call f; bug_callstack = cs} !bugs


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


  (** Remove calls to whitelist functions *)
  let remove_whitelist_bugs () =
    bugs := BugSet.filter
        (fun b ->
           match b.bug_kind with
           | Call f when List.mem f whitelist -> false
           | _ -> true
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

  let on_before_exec zone stmt man flow = ()

  let on_after_exec zone stmt man flow post =
    match skind stmt with
    | S_assign _
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man post
      ->
      add_assign_bug stmt stmt.srange (Flow.get_callstack flow)

    | _ -> ()


  let on_before_eval zone exp man flow = ()

  let on_after_eval zone exp man flow evl =
    match ekind exp with
    | E_stub_call (f,_)
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man evl
      ->
      add_call_bug f.stub_func_name exp.erange (Flow.get_callstack flow)

    | E_c_builtin_call (f,_)
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man evl
      ->
      add_call_bug f exp.erange (Flow.get_callstack flow)

    | _ -> ()
    

  let on_finish man flow =
    let () = remove_redundant_bugs () in
    let () = remove_whitelist_bugs () in
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
