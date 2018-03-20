(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of Python unit tests. *)


open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.programs.unit_test"
let debug fmt = Debug.debug ~channel:name fmt

let plurial fmt n = if n <= 1 then () else Format.pp_print_string fmt "s"

module Domain =
struct

  let get_function_name fundec = fundec.py_func_var.orgname

  let is_test fundec =
    String.sub (get_function_name fundec) 0 4 = "test"

  let get_test_functions body =
    Framework.Visitor.fold_stmt
        (fun acc exp -> acc)
        (fun acc stmt ->
           match skind stmt with
           | S_py_function(fundec)
             when is_test fundec  ->
             fundec :: acc
           | _ -> acc
        ) [] body


  let execute_test_functions tests manager ctx flow =
    tests |> List.fold_left (fun (acc, nb_ok, nb_fail, nb_may_fail, nb_panic) test ->
        debug "Executing %s" (get_function_name test);
        let range = mk_fresh_range () in
        try
          (* Call the function *)
          let flow1 =
            manager.exec
              (mk_stmt
                 (S_expression (mk_py_call
                                  (mk_var test.py_func_var (tag_range range "function"))
                                  []
                                  (tag_range range "call")
                               ))
                 (tag_range range "stmt")
              ) ctx flow
          in
          let ok, fail, may_fail = manager.flow.fold (fun (ok, fail, may_fail) env -> function
              | Libs.Mopsa.TSafeAssert _ -> (ok + 1, fail, may_fail)
              | Libs.Mopsa.TFailAssert _ -> (ok, fail + 1, may_fail)
              | Libs.Mopsa.TMayAssert _ -> (ok, fail, may_fail + 1)
              | _ -> (ok, fail, may_fail)
            ) (0, 0, 0) flow1 in
          debug "Execution of %s done@\n %a %a assertion%a passed@\n %a %a assertion%a failed@\n %a %a assertion%a unproven"
            (get_function_name test)
            ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok plurial ok
            ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail plurial fail
            ((Debug.color "orange") Format.pp_print_string) "⊬" ((Debug.color "orange") Format.pp_print_int) may_fail plurial may_fail
          ;
          manager.flow.join acc flow1, nb_ok + ok, nb_fail + fail, nb_may_fail + may_fail, nb_panic
        with
        | StmtPanic stmt ->
          Debug.warn "Execution of test %s not completed.@\nUnable to analyze stmt %a in %s"
            (get_function_name test)
            Framework.Pp.pp_stmt stmt (get_function_name test);
          acc, nb_ok, nb_fail, nb_may_fail, nb_panic + 1

        | ExprPanic exp ->
          Debug.warn "Execution of test %s not completed.@\nUnable to evaluate expression %a in %s"
            (get_function_name test)
            Framework.Pp.pp_expr exp (get_function_name test);
          acc, nb_ok, nb_fail, nb_may_fail, nb_panic + 1
      ) (manager.flow.bottom, 0, 0, 0, 0)


  let init prog manger flow = flow

  let eval exp manager ctx flow = None

  let exec stmt manager ctx flow  =
    match skind stmt with
    | S_unit_test({prog_kind = Py_program(globals, body); prog_file}) ->
      (* Initialize global and special variables *)
      let flow1 = Standalone.Domain.init_globals prog_file globals manager ctx flow in

      (* Execute the body *)
      let flow2 = manager.exec body ctx flow1 in

      (* Collect test functions *)
      let tests = get_test_functions body in

      (* Execute test functions *)
      debug "Starting tests";
      let flow3, ok, fail, may_fail, panic = execute_test_functions tests manager ctx flow2 in
      Debug.debug ~channel:(name ^ ".summary")
        "Analysis of %s done@\n %a %a assertion%a passed@\n %a %a assertion%a failed@\n %a %a assertion%a unproven\n %a %a test%a not analyzed"
        prog_file
        ((Debug.color "green") Format.pp_print_string) "✔" ((Debug.color "green") Format.pp_print_int) ok plurial ok
        ((Debug.color "red") Format.pp_print_string) "✘" ((Debug.color "red") Format.pp_print_int) fail plurial fail
        ((Debug.color "orange") Format.pp_print_string) "⊬" ((Debug.color "orange") Format.pp_print_int) may_fail plurial may_fail
        ((Debug.color "fushia") Format.pp_print_string) "✱" ((Debug.color "fushia") Format.pp_print_int) panic plurial panic
      ;

      Exec.return flow3
    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
