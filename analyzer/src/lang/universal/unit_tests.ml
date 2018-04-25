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
  | TSafeAssert of string * range
  | TFailAssert of string * range
  | TMayAssert of string * range
  | TPanic of string * range


(*==========================================================================*)
(**                        {2 Analysis alarms }                             *)
(*==========================================================================*)

type alarm_kind +=
  | AFailTest of string
  | AMayTest of string
  | APanic of string

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
          let flow1 = man.flow.add (TPanic (name, test.srange)) (man.flow.get TCur flow) flow in
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
        "Analysis of %s done@\n %a  %a assertion%a passed@\n %a  %a assertion%a failed@\n %a  %a assertion%a unproven\n %a  %a test%a skipped"
        file
        ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok Debug.plurial_int ok
        ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail Debug.plurial_int fail
        ((Debug.color "orange") Format.pp_print_string) "⚠" ((Debug.color "orange") Format.pp_print_int) may_fail Debug.plurial_int may_fail
        ((Debug.color "fushia") Format.pp_print_string) "⎇" ((Debug.color "fushia") Format.pp_print_int) panic Debug.plurial_int panic
      ;

      return flow1

    | S_simple_assert(cond, b1, b2) ->
      let cur = man.flow.get TCur flow in
      let range = srange stmt in
      let name = Framework.Context.find KCurTestName ctx in
      let cond' = if b2 then cond else mk_not cond (tag_range cond.erange "neg") in
      let f' = man.exec ctx (mk_assume cond' (tag_range stmt.srange "assume")) flow in
      let b = man.env.leq (man.flow.get TCur f') man.env.bottom in
      let nflow =
        if b = b1 then
          man.flow.add (TSafeAssert (name,range)) cur flow
        else
          man.flow.add (TFailAssert (name,range)) cur flow
      in
      return nflow

    | S_assert(cond) ->
      let range = srange stmt in
      let name = Framework.Context.find KCurTestName ctx in
      Utils.assume_to_exec
        cond
        (fun safe_flow ->
           man.flow.add (TSafeAssert (name, range)) (man.flow.get TCur safe_flow) safe_flow
        )
        (fun fail_flow ->
           man.flow.add (TFailAssert (name, range)) (man.flow.get TCur fail_flow) fail_flow |>
           man.flow.set TCur man.env.bottom
        )
        ~merge_case:(fun safe_flow fail_flow ->
            man.flow.join safe_flow fail_flow |>
            man.flow.add (TMayAssert (name, range)) (man.flow.get TCur flow) |>
            man.flow.set TCur (man.flow.get TCur safe_flow)
          )
        ~bottom_case:(fun () ->
            man.flow.add (TFailAssert (name, range)) (man.env.top) flow |>
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
            | TFailAssert(test, range) ->
              let alarm = {
                alarm_kind = AFailTest test;
                alarm_range = range;
              } in
              alarm :: acc

            | TMayAssert(test, range) ->
              let alarm = {
                alarm_kind = AMayTest test;
                alarm_range = range;
              } in
              alarm :: acc

            | TPanic(test, range) ->
              let alarm = {
                alarm_kind = APanic (test);
                alarm_range = range;
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
      | TFailAssert (_, r1), TFailAssert (_, r2)
      | TMayAssert (_, r1), TMayAssert (_, r2) ->
        compare_range r1 r2
      | TPanic(s1, r1), TPanic(s2, r2) -> compare s1 s2
      | _ -> next tk1 tk2
    );
  register_pp_token (fun next fmt -> function
      | TSafeAssert (test, r) -> Format.fprintf fmt "safe@%s:%a" test Framework.Pp.pp_range r
      | TFailAssert (test, r) -> Format.fprintf fmt "fail@%s:%a" test Framework.Pp.pp_range r
      | TMayAssert (test, r) -> Format.fprintf fmt "may@%s:%a" test Framework.Pp.pp_range r
      | TPanic (test, r) -> Format.fprintf fmt "panic@%s" test
      | tk -> next fmt tk
    );
  register_alarm_compare (fun next a1 a2 ->
      match a1.alarm_kind, a2.alarm_kind with
      | AFailTest t1, AFailTest t2
      | AMayTest t1, AMayTest t2
      | APanic(t1), APanic(t2) ->
        compare t1 t2
      | _ -> next a1 a2
    );
  register_pp_alarm (fun next fmt alarm ->
      match alarm.alarm_kind with
      | AFailTest(t) -> Format.fprintf fmt "%a  Test %s fails" ((Debug.color "red") Format.pp_print_string) "✘" t
      | AMayTest(t) -> Format.fprintf fmt "%a  Test %s may fail" ((Debug.color "orange") Format.pp_print_string) "⚠" t
      | APanic(t) -> Format.fprintf fmt "%a  Test %s skipped" ((Debug.color "fushia") Format.pp_print_string) "⎇" t
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
