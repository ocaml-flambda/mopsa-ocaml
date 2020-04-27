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
    bug_range : range;
    bug_callstack : Callstack.cs;
  }


  let compare_bug b1 b2 =
    Compare.pair compare_range Callstack.compare
      (b1.bug_range,b1.bug_callstack)
      (b2.bug_range,b2.bug_callstack)


  module BugSet = SetExt.Make
      (struct
        type t = bug
        let compare = compare_bug
      end)


  let bugs = ref BugSet.empty


  (** Add range to the set of bug locations *)
  let add_bug range cs =
    bugs := BugSet.add {bug_range = range; bug_callstack = cs} !bugs


  (** Remove redundant bug locations *)
  let remove_redundant_bugs () =
    bugs := BugSet.filter
        (fun b ->
           BugSet.for_all
             (fun b' -> b.bug_range == b'.bug_range
                          || (not (subset_range b'.bug_range b.bug_range)
                              &&  not (Callstack.is_after b'.bug_callstack b.bug_callstack))
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

  let on_before_exec zone stmt man flow = ()

  let on_after_exec zone stmt man flow post =
    match skind stmt with
    | S_assign _
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man post
      ->
      add_bug stmt.srange (Flow.get_callstack flow)

    | _ -> ()


  let on_before_eval zone exp man flow = ()

  let on_after_eval zone exp man flow evl =
    match ekind exp with
    | E_stub_call (f,_)
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man evl
      ->
      add_bug exp.erange (Flow.get_callstack flow)

    | _ -> ()
    

  let on_finish man flow =
    if BugSet.is_empty !bugs then
      printf "No potential analyzer bug detected@."
    else
      let () = remove_redundant_bugs () in
      let nb = BugSet.cardinal !bugs in
      printf "%d potential analyzer bug%a found:@.  @[<v>%a@]@."
        nb
        Debug.plurial_int nb
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           (fun fmt bug -> fprintf fmt "%a %a@,  Trace:@,%a"
               (Debug.color_str "orange") "⚠"
               pp_relative_range bug.bug_range
               (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
                  (fun fmt c ->
                     fprintf fmt "\tfrom %a: %s"
                       pp_relative_range c.Callstack.call_site
                       c.Callstack.call_fun_orig_name
                  )
               ) bug.bug_callstack
           )
        ) (BugSet.elements !bugs)

end

let () =
  Core.Hook.register_stateless_hook (module Hook)
