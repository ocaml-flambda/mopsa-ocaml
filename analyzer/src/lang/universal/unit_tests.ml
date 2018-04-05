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
open Framework.Utils
open Ast

let name = "universal.unit_tests"
let debug fmt = Debug.debug ~channel:name fmt

(*==========================================================================*)
(**                          {2 Test context }                              *)
(*==========================================================================*)

type _ Framework.Context.key +=
  | KCurTestName: string Framework.Context.key

let () =
  register_key_equality {
    case = (let f : type a b. chain -> a key -> b key -> (a, b) eq option =
              fun chain k1 k2 ->
                match k1, k2 with
                | KCurTestName, KCurTestName -> Some Eq
                | _ -> chain.check k1 k2
            in
            f);
  }

(*==========================================================================*)
(**                        {2 Assertions tokens }                           *)
(*==========================================================================*)

type token +=
  | TSafeAssert of string * range
  | TFailAssert of string * range
  | TMayAssert of string * range
  | TUnsupportedStmt of string * stmt
  | TUnsupportedExpr of string * expr

let () =
  register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TSafeAssert (_, r1), TSafeAssert (_, r2)
      | TFailAssert (_, r1), TFailAssert (_, r2)
      | TMayAssert (_, r1), TMayAssert (_, r2) ->
        compare_range r1 r2
      | TUnsupportedStmt(_, s1), TUnsupportedStmt(_, s2) ->
        compare_range s1.srange s2.srange
      | TUnsupportedExpr(_, e1), TUnsupportedExpr(_, e2) ->
        compare_range e1.erange e2.erange
      | _ -> next tk1 tk2
    );
  register_pp_token (fun next fmt -> function
      | TSafeAssert (test, r) -> Format.fprintf fmt "safe@%s:%a" test Framework.Pp.pp_range r
      | TFailAssert (test, r) -> Format.fprintf fmt "fail@%s:%a" test Framework.Pp.pp_range r
      | TMayAssert (test, r) -> Format.fprintf fmt "may@%s:%a" test Framework.Pp.pp_range r
      | TUnsupportedStmt (test, s) -> Format.fprintf fmt "unsupported@%s:%a:@[%a@]" test Framework.Pp.pp_stmt s Framework.Pp.pp_range s.srange
      | TUnsupportedExpr (test, e) -> Format.fprintf fmt "unsupported@%s:%a:@[%a@]" test Framework.Pp.pp_expr e Framework.Pp.pp_range e.erange
      | tk -> next fmt tk
    )


(*==========================================================================*)
(**                        {2 Analysis alarms }                             *)
(*==========================================================================*)

type alarm_kind +=
  | AFailTest of string
  | AMayTest of string
  | AUnsupportedStmt of string * stmt
  | AUnsupportedExpr of string * expr

(*==========================================================================*)
(**                        {2 Abstract domain }                             *)
(*==========================================================================*)

module Domain =
struct


  let execute_test_functions tests man ctx flow =
    tests |> List.fold_left (fun (acc, nb_ok, nb_fail, nb_may_fail, nb_panic) (name, test) ->
        debug "Executing %s" name;
        let ctx = Framework.Context.add KCurTestName name ctx in
        try
          (* Call the function *)
          let flow1 = man.exec test ctx flow in
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
        | StmtPanic (stmt) ->
          Debug.warn "Execution of test %s not completed.@\nUnable to analyze stmt %a"
            name
            Framework.Pp.pp_stmt stmt
          ;
          let flow1 = man.flow.add (TUnsupportedStmt (name, stmt)) (man.flow.get TCur flow) flow in
          man.flow.join acc flow1, nb_ok, nb_fail, nb_may_fail, nb_panic + 1

        | ExprPanic (exp) ->
          Debug.warn "Execution of test %s not completed.@\nUnable to evaluate expression %a"
            name
            Framework.Pp.pp_expr exp
          ;
          let flow1 = man.flow.add (TUnsupportedExpr (name, exp)) (man.flow.get TCur flow) flow in
          man.flow.join acc flow1, nb_ok, nb_fail, nb_may_fail, nb_panic + 1

      ) (man.flow.bottom, 0, 0, 0, 0)


  let init prog man flow = flow

  let eval exp man ctx flow = None

  let exec stmt man ctx flow  =
    match skind stmt with
    | S_unit_tests(file, tests) ->
      debug "Starting tests";
      let flow1, ok, fail, may_fail, panic = execute_test_functions tests man ctx flow in
      Debug.debug ~channel:(name ^ ".summary")
        "Analysis of %s done@\n %a  %a assertion%a passed@\n %a  %a assertion%a failed@\n %a  %a assertion%a unproven\n %a  %a test%a skipped"
        file
        ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok Debug.plurial_int ok
        ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail Debug.plurial_int fail
        ((Debug.color "orange") Format.pp_print_string) "⚠" ((Debug.color "orange") Format.pp_print_int) may_fail Debug.plurial_int may_fail
        ((Debug.color "fushia") Format.pp_print_string) "⎇" ((Debug.color "fushia") Format.pp_print_int) panic Debug.plurial_int panic
      ;

      return flow1

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
        man ctx flow () |>
      return

    | _ -> None

  let ask : type r. r Framework.Query.query -> ('a, unit) manager -> Framework.Context.context -> 'a flow -> r option =
    fun query man ctx flow ->
      match query with
      | Framework.Alarm.QGetAlarms ->
        let alarms = man.flow.fold (fun acc env -> function
            | TFailAssert(test, range) ->
              let alarm = {
                alarm_kind = AFailTest test;
                alarm_range = range;
                alarm_level = High;
              } in
              alarm :: acc

            | TMayAssert(test, range) ->
              let alarm = {
                alarm_kind = AMayTest test;
                alarm_range = range;
                alarm_level = Unknown;
              } in
              alarm :: acc

            | TUnsupportedStmt(test, s) ->
              let alarm = {
                alarm_kind = AUnsupportedStmt (test, s);
                alarm_range = s.srange;
                alarm_level = High;
              } in
              alarm :: acc

           | TUnsupportedExpr(test, e) ->
              let alarm = {
                alarm_kind = AUnsupportedExpr (test, e);
                alarm_range = e.erange;
                alarm_level = High;
              } in
              alarm :: acc

            | _ -> acc
          ) [] flow
        in
        let alarms = List.sort (fun a1 a2 ->
            match a1.alarm_kind, a2.alarm_kind with
            | AFailTest _, _ -> 3
            | AUnsupportedStmt _, _ -> 2
            | AUnsupportedExpr _, _ -> 1
            | AMayTest _, _ -> 0
            | _ -> compare_range a1.alarm_range a2.alarm_range
          ) alarms
        in
        Some alarms
      | _ -> None

end

let setup () =
  Stateless.register_domain name (module Domain);

  register_alarm_compare (fun next a1 a2 ->
      match a1.alarm_kind, a2.alarm_kind with
      | AFailTest t1, AFailTest t2 -> compare t1 t2
      | AMayTest t1, AMayTest t2 -> compare t1 t2
      | AUnsupportedStmt(t1, s1), AUnsupportedStmt(t2, s2) ->
        compare_composer [
          (fun () -> compare t1 t2);
          (fun () -> compare_range s1.srange s2.srange);
        ]
      | AUnsupportedExpr(t1, e1), AUnsupportedExpr(t2, e2) ->
        compare_composer [
          (fun () -> compare t1 t2);
          (fun () -> compare_range e1.erange e2.erange);
        ]
      | _ -> next a1 a2
    );

  register_pp_alarm (fun next fmt alarm ->
      match alarm.alarm_kind with
      | AFailTest(t) -> Format.fprintf fmt "%a  Test %s fails" ((Debug.color "red") Format.pp_print_string) "✘" t
      | AMayTest(t) -> Format.fprintf fmt "%a  Test %s may fail" ((Debug.color "orange") Format.pp_print_string) "⚠" t
      | AUnsupportedStmt(t, s) -> Format.fprintf fmt "%a  Test %s skipped, unsupported statement:@\n     @[%a@]" ((Debug.color "fushia") Format.pp_print_string) "⎇" t Framework.Pp.pp_stmt s
      | AUnsupportedExpr(t, e) -> Format.fprintf fmt "%a  Test %s skipped, unsupported expression:@\n     @[%a@]" ((Debug.color "fushia") Format.pp_print_string) "⎇" t Framework.Pp.pp_expr e
      | _ -> next fmt alarm
    )
