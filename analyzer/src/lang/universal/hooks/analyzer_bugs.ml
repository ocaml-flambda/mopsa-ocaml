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

(** Hook to track some bug patterns in the analyzer

    The hook captures assignments and function calls. It checks that
    if such statement is reachable its post-state should not be empty.
    Otherwise, this seems to be a soundness bug in some transfer
    function.
*)

open Location
open Mopsa
open Framework.Core.Sig.Domain.Manager
open Format
open Ast
open Zone


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "analyzer-bugs"
  let exec_zones = [Z_any]
  let eval_zones = [Z_any,Z_any]


  (** {2 Hook state} *)
  (** ************** *)

  (** Set of ranges where bugs were detected *)
  module RangeSet = SetExt.Make
      (struct
        type t = range
        let compare = compare_range
      end)

  (** State of the hook keeping the set of detected bug locations. We
      also need the set of encountered call sites in order to detected
      call expression in [on_after_eval] *)
  type state = {
    mutable bugs : RangeSet.t;
    mutable call_sites : RangeSet.t;
  }

  let state = {
    bugs = RangeSet.empty;
    call_sites = RangeSet.empty;
  }


  (** Add range to the set of bug locations *)
  let add_bug range =
    state.bugs <- RangeSet.add (untag_range range) state.bugs


  (** Update the set of call sites *)
  let update_call_sites cs =
    if Callstack.is_empty cs then
      ()
    else
      let call = Callstack.top cs in
      state.call_sites <- RangeSet.add (untag_range call.call_site) state.call_sites


  (** Check if a range corresponds to a call site *)
  let is_call_site range =
    RangeSet.mem (untag_range range) state.call_sites


  (** Remove redundant bug locations *)
  let remove_redundant_bugs () =
    state.bugs <- RangeSet.filter
        (fun range ->
           RangeSet.for_all
             (fun range' -> range == range'
                            || not (subset_range range' range)
             ) state.bugs
        ) state.bugs


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

  let on_before_exec zone stmt man flow =
    update_call_sites (Flow.get_callstack flow);
    ()

  let on_after_exec zone stmt man flow post =
    match skind stmt with
    | S_assign _
      when not (is_cur_bottom_in_flow man flow)
        && is_cur_bottom_in_cases man post
      ->
      add_bug stmt.srange

    | _ -> ()


  let on_before_eval zone exp man flow = ()

  let on_after_eval zone exp man flow evl =
    if is_call_site exp.erange
    && not (is_cur_bottom_in_flow man flow)
    && is_cur_bottom_in_cases man evl then
      add_bug exp.erange
    else
      ()

  let on_finish man flow =
    if RangeSet.is_empty state.bugs then
      printf "No potential analyzer bug detected@."
    else
      let () = remove_redundant_bugs () in
      let nb = RangeSet.cardinal state.bugs in
      printf "%d potential analyzer bug%a found:@.  @[<v>%a@]@."
        nb
        Debug.plurial_int nb
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           (fun fmt range -> fprintf fmt "%a %a" (Debug.color_str "orange") "⚠" pp_range range)
        ) (RangeSet.elements state.bugs)

end

let () =
  Core.Hook.register_stateless_hook (module Hook)
