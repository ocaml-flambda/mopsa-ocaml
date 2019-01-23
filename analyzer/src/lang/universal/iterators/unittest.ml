(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Unit tests iterator. *)

open Mopsa
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
    spec = Arg.Set unittest_flag;
    doc = " activate unittest mode";
  };
  register_domain_option name {
    key = "-unittest-filter";
    doc = " list of test functions (separated by comma) to analyze";
    spec = Arg.String(fun s ->
        unittest_filter := Str.split (Str.regexp "[ ]*,[ ]*") s
      );
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
        | A_panic_test(msg, f, loc) -> Format.fprintf fmt "Panic"
        | _ -> next fmt a
      );
    pp_report = (fun next fmt a ->
        match a.alarm_kind with
        | A_fail_assert(cond) -> Format.fprintf fmt "Condition %a fails" pp_expr cond
        | A_may_assert(cond) -> Format.fprintf fmt "Condition %a may fail" pp_expr cond
        | A_panic_test(msg, f, "") -> Format.fprintf fmt "%s: %s" f msg
        | A_panic_test(msg, f, loc) -> Format.fprintf fmt "%s: %s raised in %s" f msg loc
        | _ -> next fmt a
      );
  };
  ()


module Domain =
struct


  (* Domain identification *)
  (* ===================== *)

  type _ domain += D_universal_unittest : unit domain
  let id = D_universal_unittest
  let name = name
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_universal_unittest -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt
  let summary fmt = Debug.debug ~channel:"unittest" fmt


  (* Zoning interface *)
  (* ================ *)

  let exec_interface = {export = [Z_u]; import = []}
  let eval_interface = {export = []; import = []}


  (* Initialization *)
  (* ============== *)

  let init prog man flow = None


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
      Post.of_flow flow1 |>
      OptionExt.return

    | S_assert(cond) ->
      let range = srange stmt in
      let cs = Callstack.get flow in
      debug "Expression is %a" pp_expr cond;
      Post.assume
        cond
        ~fthen:(fun safe_flow ->
            Flow.add (T_safe_assert range) (Flow.get T_cur man safe_flow) man safe_flow |>
            Post.of_flow
        )
        ~felse:(fun fail_flow ->
            let a = mk_alarm (A_fail_assert cond) range ~cs ~level:ERROR in
            Flow.add
              (T_alarm a) (Flow.get T_cur man fail_flow)
              man fail_flow |>
            Flow.set T_cur man.bottom man |>
            Post.of_flow
        )
        ~fboth:(fun safe_flow fail_flow ->
            let a = mk_alarm (A_may_assert cond) range ~cs ~level:WARNING in
            let flow = Flow.join man safe_flow fail_flow in
            Flow.add
              (T_alarm a) (Flow.get T_cur man flow)
              man flow |>
            Flow.set T_cur (Flow.get T_cur man safe_flow) man |>
            Post.of_flow
          )
        ~fnone:(fun flow ->
            let a = mk_alarm (A_fail_assert cond) range ~cs ~level:ERROR in
            Flow.add
              (T_alarm a) man.top
              man flow |>
            Flow.set T_cur man.bottom man |>
            Post.of_flow
          )
        man flow
      |> OptionExt.return

    | S_simple_assert(cond, b1, b2) ->
      let range = srange stmt in

      let cond' = if b2 then cond else mk_not cond (tag_range cond.erange "neg") in

      let f' = man.exec (mk_assume cond' (tag_range stmt.srange "assume")) flow in
      let b = Flow.get T_cur man f' |> man.is_bottom in

      let cur =
        let env = Flow.get T_cur man flow in
        if man.is_bottom env then man.top else env
      in

      let nflow =
        if b = b1 then
          Flow.add (T_safe_assert range) cur man flow
        else
          let cs = Callstack.get flow in
          let a = mk_alarm (A_fail_assert cond) range ~cs ~level:ERROR in
          Flow.add (T_alarm a) cur man flow
      in
      Post.of_flow nflow |>
      OptionExt.return


    | _ -> None


  and execute_test_functions tests man flow =
    let annot = Flow.get_all_annot flow in
    (
      match !unittest_filter with
      | []
      | ["all"] -> tests
      | _ -> List.filter (fun (t, _) -> List.mem t !unittest_filter) tests
    )
    |>
    List.fold_left (fun (acc, nb_ok, nb_fail, nb_may_fail, nb_panic) (name, test) ->
        debug "Executing %s" name;
        try
          (* Call the function *)
          let flow1 = man.exec test flow in
          let ok, fail, may_fail = Flow.fold (fun (ok, fail, may_fail) tk env ->
              match tk with
              | T_safe_assert _ -> (ok + 1, fail, may_fail)
              | T_alarm {alarm_kind = A_fail_assert _} -> (ok, fail + 1, may_fail)
              | T_alarm {alarm_kind = A_may_assert _} -> (ok, fail, may_fail + 1)
              | _ -> (ok, fail, may_fail)
            ) (0, 0, 0) man flow1
          in
          debug "Execution of %s done@\n %a  %a assertion%a passed@\n %a  %a assertion%a failed@\n %a  %a assertion%a unproven"
            name
            ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok Debug.plurial_int ok
            ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail Debug.plurial_int fail
            ((Debug.color "orange") Format.pp_print_string) "⚠" ((Debug.color "orange") Format.pp_print_int) may_fail Debug.plurial_int may_fail
          ;
          Flow.join man acc flow1,
          nb_ok + ok,
          nb_fail + fail,
          nb_may_fail + may_fail,
          nb_panic
        with
        | Exceptions.Panic (msg, loc) ->
          let a = mk_alarm (A_panic_test (msg, name, loc)) test.srange ~level:PANIC in
          let flow1 = Flow.add (T_alarm a) (Flow.get T_cur man flow) man flow in
          Flow.join man acc flow1, nb_ok, nb_fail, nb_may_fail, nb_panic + 1

        | Exceptions.PanicAt (range, msg, loc) ->
          let a = mk_alarm (A_panic_test (msg, name, loc)) range ~level:PANIC in
          let flow1 = Flow.add (T_alarm a) (Flow.get T_cur man flow) man flow in
          Flow.join man acc flow1, nb_ok, nb_fail, nb_may_fail, nb_panic + 1

        (* | exn ->
         *   let msg = Printexc.to_string exn in
         *   let a = mk_alarm (A_panic_test (msg, name)) test.srange ~level:PANIC in
         *   let flow1 = Flow.add (T_alarm a) (Flow.get T_cur man flow) man flow in
         *   Flow.join man acc flow1, nb_ok, nb_fail, nb_may_fail, nb_panic + 1 *)


      ) (Flow.bottom annot, 0, 0, 0, 0)


  let eval zone exp man flow = None

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain);
  ()
