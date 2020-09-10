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

(** Unit tests iterator. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast


let name = "universal.iterators.unittest"

(* Command line options
   ====================

   - The flag -unittest activates the mode of unit testing. In this
   mode, every function starting with test_* is called. At the end,
   statistics about failed/safe assertions are collected.

   - The option -unittest-filter = f1,f2,... selects the functions
   to be tested.
*)

let unittest_flag = ref false
let unittest_filter = ref []

let () =
  register_domain_option name {
    key = "-unittest";
    category = "Unit tests";
    spec = ArgExt.Set unittest_flag;
    doc = " activate unittest mode";
    default = "";
  };
  register_domain_option name {
    key = "-unittest-filter";
    category = "Unit tests";
    doc = " list of test functions (separated by comma) to analyze";
    spec = ArgExt.String(fun s ->
        unittest_filter := Str.split (Str.regexp "[ ]*,[ ]*") s
      );
    default = "";
  };
  ()


(* Flow tokens
   ===========

   Safe assertions are saved in the flow so that we can compute
   statistics at the end of the analysis. Note that failed assertions
   are kept in the T_alarm token.
*)

type token +=
  | T_safe_assert of range (* location of the assert statement *)

let () =
  register_token {
    compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_safe_assert(r1), T_safe_assert(r2) -> compare_range r1 r2
        | _ -> next tk1 tk2
      );
    print = (fun next fmt tk ->
        match tk with
        | T_safe_assert(r) -> Format.fprintf fmt "safe@%a" pp_range r
        | _ -> next fmt tk
      );
  };
  ()


(* Analysis alarms *)
type alarm_class   += A_assert_fail
type alarm_message += A_assert_fail_msg of expr (** condition *)


let () =
  register_alarm_class (fun next fmt -> function
      | A_assert_fail -> Format.fprintf fmt "Assertion fail"
      | a -> next fmt a
    );
  register_alarm_message {
    classifier = (fun next -> function
        | A_assert_fail_msg _ -> A_assert_fail
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_assert_fail_msg(c1), A_assert_fail_msg(c2) -> compare_expr c1 c2
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_assert_fail_msg(cond) -> Format.fprintf fmt "Assertion '%a' violated" (Debug.bold pp_expr) cond
        | a -> next fmt a
      );
  };
  ()


let raise_assert_fail ?(force=false) cond man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_assert_fail_msg cond) cs cond.erange in
  Flow.raise_alarm alarm ~bottom:true ~force man.lattice flow



module Domain =
struct


  (* Domain identification *)
  (* ===================== *)

  include GenStatelessDomainId(struct
      let name = name
    end)
  let summary fmt = Debug.debug ~channel:"unittest" fmt


  let alarms = []

  (* Initialization *)
  (* ============== *)

  let init prog man flow = flow


  (* Computation of post-conditions *)
  (* ============================== *)

  let execute_test_functions tests man flow =
    let tests = match !unittest_filter with
      | [] | ["all"] -> tests
      | _ -> List.filter (fun (t, _) -> List.mem t !unittest_filter) tests
    in
    let ctx = Flow.get_ctx flow in
    let alarms = Flow.get_alarms flow in
    List.fold_left (fun acc (name, test) ->
        debug "Executing %s" name;
        (* Fold the context *)
        let flow = Flow.copy_ctx acc flow in
        (* Call the function *)
        let flow1 = man.exec test flow |> post_to_flow man in
        Flow.join man.lattice acc flow1
      )
      (Flow.bottom ctx alarms)
      tests

  let rec exec stmt man flow  =
    match skind stmt with
    | S_unit_tests(tests) ->
      debug "Starting tests";
      let flow1 = execute_test_functions tests man flow in
      Post.return flow1 |>
      OptionExt.return

    | S_assert(cond) ->
      assume
        cond
        ~fthen:(fun safe_flow -> Post.return safe_flow)
        ~felse:(fun fail_flow ->
            raise_assert_fail cond man fail_flow |>
            Post.return
          )
        man flow
      |> OptionExt.return

    | S_satisfy(cond) ->
      man.exec (mk_assume cond stmt.srange) flow >>%? fun flow' -> 
      if not @@ man.lattice.is_bottom @@ Flow.get T_cur man.lattice flow' then
        Post.return flow |>
        OptionExt.return
      else
        raise_assert_fail cond man flow |>
        Post.return |>
        OptionExt.return


    | _ -> None


  let eval exp man flow = None

  let ask query man flow = None

end

let () =
  register_stateless_domain (module Domain)
