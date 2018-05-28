(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Unit tests iterator. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Alarm
open Framework.Context
open Framework.Lattice
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Framework.Eval
open Ast

let name = "universal.unit_tests"
let debug fmt = Debug.debug ~channel:name fmt

(*==========================================================================*)
(**                          {2 Test context }                              *)
(*==========================================================================*)

type _ Framework.Context.key +=
  | KCurTestName: string Framework.Context.key


(*==========================================================================*)
(**                        {2 Assertions tokens }                           *)
(*==========================================================================*)

type token +=
  | TSafeAssert of string (** test function *) * range
  | TFailAssert of expr (** condition *) * string (** test function *) * range
  | TMayAssert of expr (** condition *) * string (** test function *) * range
  | TPanic of string (** message *) * string (** test function *) * range


(*==========================================================================*)
(**                        {2 Analysis alarms }                             *)
(*==========================================================================*)

type alarm_kind +=
  | AFailTest of expr (** condition *)* string (** test function *)
  | AMayTest of expr (** condition *)* string (** test function *)
  | APanic of string (** panic message *) * string (** test function *)

(*==========================================================================*)
(**                        {2 Abstract domain }                             *)
(*==========================================================================*)

module Domain =
struct


  let execute_test_functions man ctx tests flow =
    tests |> List.fold_left (fun (acc, nb_ok, nb_fail, nb_may_fail, nb_panic) (name, test) ->
        debug "Executing %s" name;
        let ctx = Framework.Context.add KCurTestName name ctx in
        try
          (* Call the function *)
          let flow1 = man.exec ctx test flow in
          let ok, fail, may_fail = man.flow.fold (fun (ok, fail, may_fail) env -> function
              | TSafeAssert _ -> (ok + 1, fail, may_fail)
              | TFailAssert _ -> (ok, fail + 1, may_fail)
              | TMayAssert _ -> (ok, fail, may_fail + 1)
              | _ -> (ok, fail, may_fail)
            ) (0, 0, 0) flow1 in
          debug "Execution of %s done@\n %a  %a assertion%a passed@\n %a  %a assertion%a failed@\n %a  %a assertion%a unproven"
            name
            ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok Debug.plurial_int ok
            ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail Debug.plurial_int fail
            ((Debug.color "orange") Format.pp_print_string) "⚠" ((Debug.color "orange") Format.pp_print_int) may_fail Debug.plurial_int may_fail
          ;
          man.flow.join acc flow1, nb_ok + ok, nb_fail + fail, nb_may_fail + may_fail, nb_panic
        with
        | Framework.Exceptions.Panic (msg) ->
          Debug.warn "Panic: @[%s@]" msg;
          let flow1 = man.flow.add (TPanic (msg, name, test.srange)) (man.flow.get TCur flow) flow in
          man.flow.join acc flow1, nb_ok, nb_fail, nb_may_fail, nb_panic + 1

        | Framework.Exceptions.PanicAt (range, msg) ->
          Debug.warn "Panic: @[%s@] at %a" msg Framework.Pp.pp_range range;
          let flow1 = man.flow.add (TPanic (msg, name, range)) (man.flow.get TCur flow) flow in
          man.flow.join acc flow1, nb_ok, nb_fail, nb_may_fail, nb_panic + 1

      ) (man.flow.bottom, 0, 0, 0, 0)


  let init man ctx prog flow = ctx, flow

  let eval man ctx exp flow = None

  let exec man ctx stmt flow  =
    match skind stmt with
    | S_unit_tests(file, tests) ->
      debug "Starting tests";
      let flow1, ok, fail, may_fail, panic = execute_test_functions man ctx tests flow in
      Debug.debug ~channel:(name ^ ".summary")
        "Analysis of %s done@\n %a assertion%a passed@\n %a assertion%a failed@\n %a assertion%a unproven\n %a test%a panicked"
        file
        ((Debug.color "green") Format.pp_print_int) ok Debug.plurial_int ok
        ((Debug.color "red") Format.pp_print_int) fail Debug.plurial_int fail
        ((Debug.color "orange") Format.pp_print_int) may_fail Debug.plurial_int may_fail
        ((Debug.color "red") Format.pp_print_int) panic Debug.plurial_int panic
      ;

      return flow1

    | S_simple_assert(cond, b1, b2) ->
      let range = srange stmt in
      let name = Framework.Context.find KCurTestName ctx in
      let cond' = if b2 then cond else mk_not cond (tag_range cond.erange "neg") in
      let f' = man.exec ctx (mk_assume cond' (tag_range stmt.srange "assume")) flow in
      let b = man.flow.is_cur_bottom f' in
      let cur =
        let env = man.flow.get TCur flow in
        if man.env.is_bottom env then man.env.top else env
      in
      let nflow =
        if b = b1 then
          man.flow.add (TSafeAssert (name,range)) cur flow
        else
          man.flow.add (TFailAssert (cond', name,range)) cur flow
      in
      return nflow

    | S_assert(cond) ->
      let range = srange stmt in
      let name = Framework.Context.find KCurTestName ctx in
      Utils.assume_to_exec
        cond
        (fun safe_flow ->
           man.flow.add (TSafeAssert ( name, range)) (man.flow.get TCur safe_flow) safe_flow
        )
        (fun fail_flow ->
           man.flow.add (TFailAssert (cond, name, range)) (man.flow.get TCur fail_flow) fail_flow |>
           man.flow.set TCur man.env.bottom
        )
        ~merge_case:(fun safe_flow fail_flow ->
            man.flow.join safe_flow fail_flow |>
            man.flow.add (TMayAssert (cond, name, range)) (man.flow.get TCur flow) |>
            man.flow.set TCur (man.flow.get TCur safe_flow)
          )
        ~bottom_case:(fun () ->
            man.flow.add (TFailAssert (cond, name, range)) (man.env.top) flow |>
            man.flow.set TCur man.env.bottom
          )
        man ctx flow () |>
      return

    | _ -> None

  let ask : type r. ('a, unit) manager -> Framework.Context.context -> r Framework.Query.query -> 'a flow -> r option =
    fun man ctx query flow ->
      match query with
      | Framework.Alarm.QGetAlarms ->
        let alarms = man.flow.fold (fun acc env -> function
            | TFailAssert(cond, test, range) ->
              let alarm = {
                alarm_kind = AFailTest (cond, test);
                alarm_range = range;
                alarm_level = ERROR;
              } in
              alarm :: acc

            | TMayAssert(cond, test, range) ->
              let alarm = {
                alarm_kind = AMayTest (cond, test);
                alarm_range = range;
                alarm_level = WARNING;
              } in
              alarm :: acc

            | TPanic(msg, test, range) ->
              let alarm = {
                alarm_kind = APanic (msg, test);
                alarm_range = range;
                alarm_level = PANIC;
              } in
              alarm :: acc

            | _ -> acc
          ) [] flow
        in
        let alarms = List.sort (fun a1 a2 ->
            match a1.alarm_kind, a2.alarm_kind with
            | AFailTest _, _ -> 2
            | APanic _, _ -> 1
            | AMayTest _, _ -> 0
            | _ -> compare_range a1.alarm_range a2.alarm_range
          ) alarms
        in
        Some alarms
      | _ -> None

end

let setup () =
  Stateless.register_domain name (module Domain);
  register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TSafeAssert (_, r1), TSafeAssert (_, r2)
      | TFailAssert (_, _, r1), TFailAssert (_, _, r2)
      | TMayAssert (_, _, r1), TMayAssert (_, _, r2) ->
        compare_range r1 r2
      | TPanic(msg1, t1, r1), TPanic(msg2, t2, r2) ->
        compare_composer [
          (fun () -> compare msg1 msg2);
          (fun () -> compare t1 t2);
          (fun () -> compare_range r1 r2);
        ]
      | _ -> next tk1 tk2
    );
  register_pp_token (fun next fmt -> function
      | TSafeAssert (test, r) -> Format.fprintf fmt "safe@%s:%a" test Framework.Pp.pp_range r
      | TFailAssert (_, test, r) -> Format.fprintf fmt "fail@%s:%a" test Framework.Pp.pp_range r
      | TMayAssert (_, test, r) -> Format.fprintf fmt "may@%s:%a" test Framework.Pp.pp_range r
      | TPanic (msg, test, r) -> Format.fprintf fmt "panic@%s:%s" msg test
      | tk -> next fmt tk
    );
  register_alarm_compare (fun next a1 a2 ->
      match a1.alarm_kind, a2.alarm_kind with
      | AFailTest (cond1, t1), AFailTest (cond2, t2)
      | AMayTest (cond1, t1), AMayTest (cond2, t2) ->
        compare_composer [
          (fun () -> compare t1 t2);
          (fun () -> compare_range cond1.erange cond2.erange)
        ]
      | APanic (msg1, test1), APanic (msg2, test2) ->
        compare_composer [
          (fun () -> compare msg1 msg2);
          (fun () -> compare test1 test2);
        ]
      | _ -> next a1 a2
    );
  register_pp_alarm (fun next fmt alarm ->
      match alarm.alarm_kind with
      | AFailTest(cond, t) -> Format.fprintf fmt "Assertion %a in %s fails" Framework.Pp.pp_expr cond t
      | AMayTest(cond, t) -> Format.fprintf fmt "Assertion %a in %s may fail" Framework.Pp.pp_expr cond t
      | APanic(msg, t) -> Format.fprintf fmt "Panic in %s: %s" t msg
      | _ -> next fmt alarm
    );
  register_key_equality {
    case = (let f : type a b. chain -> a key -> b key -> (a, b) eq option =
              fun chain k1 k2 ->
                match k1, k2 with
                | KCurTestName, KCurTestName -> Some Eq
                | _ -> chain.check k1 k2
            in
            f);
  }
