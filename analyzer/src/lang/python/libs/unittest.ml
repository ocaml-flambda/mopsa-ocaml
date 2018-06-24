(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Unittest library. *)

open Framework.Domains.Stateless
open Framework.Essentials
open Universal.Ast
open Ast
open Addr

let name = "python.libs.unittest"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                               {2 Domain }                               *)
(*==========================================================================*)


module Domain =
struct

  let get_name addr =
    match addr.addr_kind with
    | A_py_class(C_user cls, _) -> cls.py_cls_var.vname
    | A_py_function (F_user f) -> f.py_func_var.vname
    | _ -> assert false

  let is_test_function name =
    if String.length name < 5 then false
    else String.sub name 0 4 = "test"


  (*==========================================================================*)
  (**                       {2 Transfer functions }                           *)
  (*==========================================================================*)


  let exec_interface = Framework.Domain.{
      import = [];
      export = [];
    }

  let eval_interface = Framework.Domain.{
      import = [Zone.Z_py, Zone.Z_py_object];
      export = [];
    }
  
  let eval zpath exp man ctx flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.main")})}, [], []) ->
      debug "Search for all classes that inherit from TestCase";
      let globals = Framework.Context.find Program.KPyGlobals ctx in
      let test_cases, functions = List.fold_left (fun acc glob ->
          man.eval ~zpath:(Zone.Z_py, Zone.Z_py_object) (mk_var glob range) ctx flow |>
          Eval.fold_ (fun (tests, funs) case ->
              match case.result with
              | Some {ekind = E_py_object ({addr_kind = A_py_class(C_user cls, [({addr_kind = A_py_class (C_builtin "unittest.TestCase", _)}, _)])} as addr, _)} ->
                (addr, cls) :: tests, funs
              | Some {ekind = E_py_object ({addr_kind = A_py_function (F_user f)}, _)} ->
                tests, f :: funs
              | _ -> tests, funs
            ) acc
        ) ([], []) globals
      in
      (* Instantiate the test classes *)
      let selfs, flow =
        List.fold_left (fun (selfs, flow) (addr, cls) ->
            (* Allocate an instance of the test class *)
            man.eval
              ~zpath:(Universal.Zone.Z_heap, Universal.Zone.Z_heap)
              (mk_alloc_instance (addr, mk_py_empty range) range)
              ctx flow |>
            Eval.fold_ (fun (selfs, _) case ->
                match case.result with
                | Some {ekind = E_addr addr} -> ((addr, mk_py_empty range), cls) :: selfs, case.flow
                | _ -> assert false
              ) (selfs, flow)
          ) ([], flow) test_cases
      in

      (* Fold over the class methods and bind them to self *)
      let tests =
        List.fold_left (fun tests (self, cls) ->
            List.fold_left (fun tests v ->
                match is_test_function v.vname, List.find_opt (fun f -> compare_var f.py_func_var v = 0) functions with
                | false, _ | _, None -> tests
                | true, Some f ->
                  let stmt = mk_stmt (S_expression (mk_py_call (mk_py_object_attr self v.vname range) [] range)) range in
                  (v.vname, stmt) :: tests
              ) tests cls.py_cls_static_attributes
          ) [] selfs
      in

      let flow = man.exec (mk_stmt (Universal.Ast.S_unit_tests ("file", tests)) range) ctx flow in
      Eval.singleton (Some (mk_py_none range)) flow


    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertEqual")})}, [test; arg1; arg2], []) ->
      Mopsa.check (mk_binop arg1 O_eq arg2 range) range  man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertTrue")})}, [test; cond], []) ->
      Mopsa.check cond range man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertFalse")})}, [test; cond], []) ->
      Mopsa.check (mk_not cond range) range man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertIs")})}, [test; arg1; arg2], []) ->
      Mopsa.check (mk_binop arg1 O_py_is arg2 range) range man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertIsNot")})}, [test; arg1; arg2], []) ->
      Mopsa.check (mk_binop arg1 O_py_is_not arg2 range) range man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertIn")})}, [test; arg1; arg2], []) ->
      Mopsa.check (mk_binop arg1 O_py_in arg2 range) range man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertNotIn")})}, [test; arg1; arg2], []) ->
      Mopsa.check (mk_binop arg1 O_py_not_in arg2 range) range man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertIsInstance")})}, [test; arg1; arg2], []) ->
      Mopsa.check (Utils.mk_builtin_call "isinstance" [arg1; arg2] range) range man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertNotIsInstance")})}, [test; arg1; arg2], []) ->
      Mopsa.check (mk_not (Utils.mk_builtin_call "isinstance" [arg1; arg2] range) range) range man ctx flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertRaises")})}, test :: exn :: f :: args, []) ->
      let stmt = mk_try
          (mk_block [
              mk_stmt (S_expression (mk_py_call f args range)) range;
              mk_assert_unreachable range
            ] range)
          [
            mk_except
              (Some exn)
              None
              (mk_assert_reachable range);
            mk_except
              None
              None
              (mk_assert_unreachable range)
          ]
          (mk_block [] range)
          (mk_block [] range)
          range
      in
      let flow = man.exec stmt ctx flow in
      Eval.singleton (Some (mk_py_none range)) flow

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertRaises")})}, [test; exn], []) ->
      (* Instantiate ExceptionContext with the given exception exn *)
      let exp' = Utils.mk_builtin_call "unittest.ExceptionContext" [exn] range in
      man.eval exp' ctx flow |>
      Eval.return


    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.ExceptionContext.__exit__")})},[self; typ; exn; trace], []) ->
      Eval.assume
        (mk_binop exn O_eq (mk_py_none range) range) ~zone:Zone.Z_py
        ~fthen:(fun true_flow ->
           (* No exception raised => assertion failed *)
           let flow = man.exec (mk_assert_unreachable range) ctx true_flow in
           Eval.empty flow
        )
        ~felse:(fun false_flow ->
           (* Check that the caught exception is an instance of the expected exception *)
           Eval.assume
             (Utils.mk_builtin_call "isinstance" [exn; (mk_py_attr self "expected" range)] range) ~zone:Zone.Z_py
             ~fthen:(fun true_flow ->
                let flow = man.exec (mk_assert_reachable range) ctx true_flow in
                Eval.singleton (Some (mk_py_true range)) flow
             )
             ~felse:(fun false_flow ->
                let flow = man.exec (mk_assert_unreachable range) ctx false_flow in
                Eval.empty flow
             )
             man ctx false_flow ()
        )
        man ctx flow ()

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin f)})}, _, _)
      when Addr.is_builtin_class_function "unittest.TestCase" f ->
      Framework.Exceptions.panic "unittest.TestCase function %s not implemented" f

    | _ -> None

  let init prog man ctx flow = None
  let exec zone stmt man ctx flow = None
  let ask _ _ _ _ = None

end




(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let setup () =
  register_domain name (module Domain)
