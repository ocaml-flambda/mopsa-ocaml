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
open Framework.Lattice
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Ast

let name = "universal.unit_tests"
let debug fmt = Debug.debug ~channel:name fmt
let plurial fmt n = if n <= 1 then () else Format.pp_print_string fmt "s"


(*==========================================================================*)
(**                        {2 Assertions tokens }                           *)
(*==========================================================================*)

type token +=
  | TSafeAssert of range
  | TFailAssert of range
  | TMayAssert of range

let () =
  register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TSafeAssert r1, TSafeAssert r2
      | TFailAssert r1, TFailAssert r2
      | TMayAssert r1, TMayAssert r2 ->
        compare_range r1 r2
      | _ -> next tk1 tk2
    );
  register_pp_token (fun next fmt -> function
      | TSafeAssert r -> Format.fprintf fmt "safe@%a" Framework.Pp.pp_range r
      | TFailAssert r -> Format.fprintf fmt "fail@%a" Framework.Pp.pp_range r
      | TMayAssert r -> Format.fprintf fmt "may@%a" Framework.Pp.pp_range r
      | tk -> next fmt tk
    )


module Domain =
struct


  let execute_test_functions tests man ctx flow =
    tests |> List.fold_left (fun (acc, nb_ok, nb_fail, nb_may_fail, nb_panic) (name, test) ->
        debug "Executing %s" name;
        try
          (* Call the function *)
          let flow1 = man.exec test ctx flow in
          let ok, fail, may_fail = man.flow.fold (fun (ok, fail, may_fail) env -> function
              | TSafeAssert _ -> (ok + 1, fail, may_fail)
              | TFailAssert _ -> (ok, fail + 1, may_fail)
              | TMayAssert _ -> (ok, fail, may_fail + 1)
              | _ -> (ok, fail, may_fail)
            ) (0, 0, 0) flow1 in
          debug "Execution of %s done@\n %a %a assertion%a passed@\n %a %a assertion%a failed@\n %a %a assertion%a unproven"
            name
            ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok plurial ok
            ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail plurial fail
            ((Debug.color "orange") Format.pp_print_string) "⊬" ((Debug.color "orange") Format.pp_print_int) may_fail plurial may_fail
          ;
          man.flow.join acc flow1, nb_ok + ok, nb_fail + fail, nb_may_fail + may_fail, nb_panic
        with
        | StmtPanic stmt ->
          Debug.warn "Execution of test %s not completed.@\nUnable to analyze stmt %a"
            name
            Framework.Pp.pp_stmt stmt;
          acc, nb_ok, nb_fail, nb_may_fail, nb_panic + 1

        | ExprPanic exp ->
          Debug.warn "Execution of test %s not completed.@\nUnable to evaluate expression %a"
            name
            Framework.Pp.pp_expr exp;
          acc, nb_ok, nb_fail, nb_may_fail, nb_panic + 1
      ) (man.flow.bottom, 0, 0, 0, 0)


  let init prog man flow = flow

  let eval exp man ctx flow = None

  let exec stmt man ctx flow  =
    match skind stmt with
    | S_unit_tests(file, tests) ->
      debug "Starting tests";
      let flow1, ok, fail, may_fail, panic = execute_test_functions tests man ctx flow in
      Debug.debug ~channel:(name ^ ".summary")
        "Analysis of %s done@\n %a %a assertion%a passed@\n %a %a assertion%a failed@\n %a %a assertion%a unproven\n %a %a test%a not analyzed"
        file
        ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok plurial ok
        ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail plurial fail
        ((Debug.color "orange") Format.pp_print_string) "⊬" ((Debug.color "orange") Format.pp_print_int) may_fail plurial may_fail
        ((Debug.color "fushia") Format.pp_print_string) "✱" ((Debug.color "fushia") Format.pp_print_int) panic plurial panic
      ;

      Exec.return flow1

    | S_assert(cond) ->
      let range = srange stmt in
      Utils.cond_flow
        cond
        (fun safe_flow ->
           man.flow.add (TSafeAssert range) (man.flow.get TCur safe_flow) safe_flow
        )
        (fun fail_flow ->
           man.flow.add (TFailAssert range) (man.flow.get TCur fail_flow) fail_flow |>
           man.flow.set TCur man.env.bottom
        )
        (fun () -> flow)
        (fun safe_flow fail_flow ->
           man.flow.join safe_flow fail_flow |>
           man.flow.add (TMayAssert range) (man.flow.get TCur flow) |>
           man.flow.set TCur (man.flow.get TCur safe_flow)
        )
        man ctx flow |>
      Exec.return
        
    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
