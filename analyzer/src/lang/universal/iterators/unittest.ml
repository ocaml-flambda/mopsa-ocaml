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
open Framework.Core.Sig.Domain.Stateless
open Ast
open Zone


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


(* Analysis alarms
   ===============

   Three kinds of alarms are handled here:
   (a) failed assertions,
   (b) potentially failed assertions, and
   (c) panic situations.
*)

type alarm_kind +=
  | A_fail_assert of expr (** condition *)
  | A_may_assert of expr (** condition *)
  | A_panic_test of string (** panic message *) * string (* test function *) * string (** OCaml loc *)

let () =
  register_alarm {
    compare = (fun next a1 a2 ->
        match a1.alarm_kind, a2.alarm_kind with
        | A_fail_assert(c1), A_fail_assert(c2)
        | A_may_assert(c1), A_may_assert(c2) ->
          compare_expr c1 c2

        | A_panic_test(msg1, f1, loc1), A_panic_test(msg2, f2, loc2) ->
          Compare.compose [
            (fun () -> compare msg1 msg2);
            (fun () -> compare f1 f2)
          ]
        | _ -> next a1 a2
      );
    pp_token = (fun next fmt a ->
        match a.alarm_kind with
        | A_fail_assert (c) -> Format.fprintf fmt "fail"
        | A_may_assert  (c) -> Format.fprintf fmt "may"
        | A_panic_test (msg, f, loc) -> Format.fprintf fmt "panic@%s" f
        | _ -> next fmt a
      );
    pp_title = (fun next fmt a ->
        match a.alarm_kind with
        | A_fail_assert(cond) -> Format.fprintf fmt "Assertion fail"
        | A_may_assert(cond) -> Format.fprintf fmt "Assertion unproven"
        | A_panic_test(msg, f, loc) -> Format.fprintf fmt "Panic in test %s: %s" f msg
        | _ -> next fmt a
      );
    pp_report = (fun next fmt a ->
        match a.alarm_kind with
        | A_fail_assert(cond) -> Format.fprintf fmt "Condition %a fails" pp_expr cond
        | A_may_assert(cond) -> Format.fprintf fmt "Condition %a may fail" pp_expr cond
        | A_panic_test(msg, f, "") -> Format.fprintf fmt "%s: %s" f msg
        | A_panic_test(msg, f, loc) -> Format.fprintf fmt "%s: %s" loc msg
        | _ -> next fmt a
      );
  };
  ()


module Domain =
struct


  (* Domain identification *)
  (* ===================== *)

  include GenStatelessDomainId(struct
      let name = name
    end)
  let summary fmt = Debug.debug ~channel:"unittest" fmt


  (* Zoning interface *)
  (* ================ *)

  let interface = {
    iexec = {provides = [Z_u]; uses = []};
    ieval = {provides = []; uses = []};
  }


  (* Initialization *)
  (* ============== *)

  let init prog man flow = flow


  (* Computation of post-conditions *)
  (* ============================== *)

  let rec exec zone stmt man flow  =
    match skind stmt with
    | S_unit_tests(tests) ->
      debug "Starting tests";
      let flow1, ok, fail, may_fail, panic = execute_test_functions tests man flow in
      summary
        "Analysis done@\n %a  %a assertion%a passed@\n %a  %a assertion%a failed@\n %a  %a assertion%a unproven\n %a  %a test%a skipped"
        ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok Debug.plurial_int ok
        ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail Debug.plurial_int fail
        ((Debug.color "orange") Format.pp_print_string) "⚠" ((Debug.color "orange") Format.pp_print_int) may_fail Debug.plurial_int may_fail
        ((Debug.color "fushia") Format.pp_print_string) "⎇" ((Debug.color "fushia") Format.pp_print_int) panic Debug.plurial_int panic
      ;
      Post.return flow1 |>
      Option.return

    | S_assert(cond) ->
      let range = srange stmt in
      let cs = Callstack.get flow in
      assume_post
        cond
        ~fthen:(fun safe_flow ->
            Flow.add (T_safe_assert range) (Flow.get T_cur man.lattice safe_flow) man.lattice safe_flow |>
            Post.return
        )
        ~felse:(fun fail_flow ->
            let a = mk_alarm (A_fail_assert cond) range ~cs ~level:ERROR in
            Flow.add
              (T_alarm a) (Flow.get T_cur man.lattice fail_flow)
              man.lattice fail_flow |>
            Flow.set T_cur man.lattice.bottom man.lattice |>
            Post.return
        )
        ~fboth:(fun safe_flow fail_flow ->
            let a = mk_alarm (A_may_assert cond) range ~cs ~level:WARNING in
            let flow = Flow.join man.lattice safe_flow fail_flow in
            Flow.add
              (T_alarm a) (Flow.get T_cur man.lattice flow)
              man.lattice flow |>
            Flow.set T_cur (Flow.get T_cur man.lattice safe_flow) man.lattice |>
            Post.return
          )
        ~fnone:(fun flow ->
            let a = mk_alarm (A_fail_assert cond) range ~cs ~level:ERROR in
            Flow.add
              (T_alarm a) man.lattice.top
              man.lattice flow |>
            Flow.set T_cur man.lattice.bottom man.lattice |>
            Post.return
          )
        man flow
      |> Option.return

    | S_simple_assert(cond, b1, b2) ->
      let range = srange stmt in

      let cond' = if b2 then cond else mk_not cond (tag_range cond.erange "neg") in

      let f' = man.exec (mk_assume cond' (tag_range stmt.srange "assume")) flow in
      let b = Flow.get T_cur man.lattice f' |> man.lattice.is_bottom in

      let cur =
        let env = Flow.get T_cur man.lattice flow in
        if man.lattice.is_bottom env then man.lattice.top else env
      in

      let nflow =
        if b = b1 then
          Flow.add (T_safe_assert range) cur man.lattice flow
        else
          let cs = Callstack.get flow in
          let a = mk_alarm (A_fail_assert cond) range ~cs ~level:ERROR in
          Flow.add (T_alarm a) cur man.lattice flow
      in
      Post.return nflow |>
      Option.return


    | _ -> None


  and execute_test_functions tests man flow =
    let annot = Flow.get_ctx flow in
    (
      match !unittest_filter with
      | []
      | ["all"] -> tests
      | _ -> List.filter (fun (t, _) -> List.mem t !unittest_filter) tests
    )
    |>
    List.fold_left (fun (acc, nb_ok, nb_fail, nb_may_fail, nb_panic) (name, test) ->
        debug "Executing %s" name;
        (* Fold the context *)
        let flow = Flow.copy_ctx acc flow in
        (* Call the function *)
        let flow1 = man.exec test flow in
        let ok, fail, may_fail = Flow.fold (fun (ok, fail, may_fail) tk env ->
            match tk with
            | T_safe_assert _ -> (ok + 1, fail, may_fail)
            | T_alarm {alarm_kind = A_fail_assert _} -> (ok, fail + 1, may_fail)
            | T_alarm {alarm_kind = A_may_assert _} -> (ok, fail, may_fail + 1)
            | _ -> (ok, fail, may_fail)
          ) (0, 0, 0) flow1
        in
        debug "Execution of %s done@\n %a  %a assertion%a passed@\n %a  %a assertion%a failed@\n %a  %a assertion%a unproven"
          name
          ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok Debug.plurial_int ok
          ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail Debug.plurial_int fail
          ((Debug.color "orange") Format.pp_print_string) "⚠" ((Debug.color "orange") Format.pp_print_int) may_fail Debug.plurial_int may_fail
        ;
        Flow.join man.lattice acc flow1,
        nb_ok + ok,
        nb_fail + fail,
        nb_may_fail + may_fail,
        nb_panic


      ) (Flow.bottom annot, 0, 0, 0, 0)


  let eval zone exp man flow = None

  let ask query man flow = None

end

let () =
  register_domain (module Domain)
