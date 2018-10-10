(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Unittest library. *)


open Framework.Essentials
open Universal.Ast
open Ast
open Addr

module Domain =
  struct

    type _ domain += D_python_libs_unittest : unit domain

    let id = D_python_libs_unittest
    let name = "python.libs.unittest"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_libs_unittest -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = { export = []; import = [] }
    let eval_interface = { export = [Zone.Z_py, Zone.Z_py]; import = []}

    let init _ _ flow = Some flow

    let exec _ _ _ _ = None

    let eval zones exp (man:('a, unit) man) (flow:'a flow) : ('a, Framework.Ast.expr) evl option =
      let range = exp.erange in
      match ekind exp with
      (* | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.main")})}, [], []) ->
        * debug "Search for all classes that inherit from TestCase";
          * let test_cases = man.ask ctx Universal.Heap.Query.QAllocatedAddresses flow |>
          *                    OptionExt.none_to_exn |>
          *                    List.filter (fun addr ->
          *                        match addr.addr_kind with
          *                        | A_py_class(cls, ({addr_kind = A_py_class (C_builtin "unittest.TestCase", _)}, _) :: _) -> true
          *                        | _ -> false
          *                      ) |>
          *                    List.map (fun addr ->
          *                        match addr.addr_kind with
          *                        | A_py_class(C_user cls, _) -> addr, cls
          *                        | _ -> assert false
          *                      )
          * in
          * (\* Instantiate the test classes *\)
          * let selfs, flow =
          *   List.fold_left (fun (selfs, flow) (addr, cls) ->
          *       (\* Allocate an instance of the test class *\)
          *       Addr.eval_alloc_instance man ctx (addr, mk_py_empty range) None range flow |>
          *         oeval_fold (fun (selfs, _) (addr, flow, _) ->
          *             match addr with
          *             | Some addr -> ((addr, mk_py_empty range), cls) :: selfs, flow
          *             | None -> assert false
          *           ) (selfs, flow)
          *     ) ([], flow) test_cases
          * in
          *
          * let functions = man.ask ctx Universal.Heap.Query.QAllocatedAddresses flow |>
          *                   OptionExt.none_to_exn |>
          *                   List.filter (fun addr ->
          *                       match addr.addr_kind with
          *                       | A_py_function(F_user func) -> true
          *                       | _ -> false
          *                     ) |>
          *                   List.map (fun addr ->
          *                       match addr.addr_kind with
          *                       | A_py_function(F_user func) -> func
          *                       | _ -> assert false
          *                     )
          * in
          * (\* Fold over the class methods and bind them to self *\)
          * let tests =
          *   List.fold_left (fun tests (self, cls) ->
          *       List.fold_left (fun tests v ->
          *           match is_test_function v.vname, List.find_opt (fun f -> compare_var f.py_func_var v = 0) functions with
          *           | false, _ | _, None -> tests
          *           | true, Some f ->
          *              let stmt = mk_stmt (S_expression (mk_py_call (mk_py_object_attr self v.vname range) [] range)) range in
          *              (v.vname, stmt) :: tests
          *         ) tests cls.py_cls_static_attributes
          *     ) [] selfs
          * in
          *
          * let flow = man.exec ctx (mk_stmt (Universal.Ast.S_unit_tests ("file", tests)) range) flow in
          * oeval_singleton (Some (mk_py_none range), flow, []) *)


      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertEqual")})}, [test; arg1; arg2], []) ->
         Mopsa.check man (mk_binop arg1 O_eq arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertTrue")})}, [test; cond], []) ->
         Mopsa.check man cond range flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertFalse")})}, [test; cond], []) ->
         Mopsa.check man (mk_not cond range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertIs")})}, [test; arg1; arg2], []) ->
         Mopsa.check man (mk_binop arg1 O_py_is arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertIsNot")})}, [test; arg1; arg2], []) ->
         Mopsa.check man (mk_binop arg1 O_py_is_not arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertIn")})}, [test; arg1; arg2], []) ->
         Mopsa.check man (mk_binop arg1 O_py_in arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertNotIn")})}, [test; arg1; arg2], []) ->
         Mopsa.check man (mk_binop arg1 O_py_not_in arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertIsInstance")})}, [test; arg1; arg2], []) ->
         Mopsa.check man (Utils.mk_builtin_call "isinstance" [arg1; arg2] range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertNotIsInstance")})}, [test; arg1; arg2], []) ->
         Mopsa.check man (mk_not (Utils.mk_builtin_call "isinstance" [arg1; arg2] range) range) range flow
         |> OptionExt.return

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
         let flow = man.exec stmt flow in
         Eval.singleton (mk_py_none range) flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertRaises")})}, [test; exn], []) ->
         (* Instantiate ExceptionContext with the given exception exn *)
         let exp' = Utils.mk_builtin_call "unittest.ExceptionContext" [exn] range in
         man.eval exp' flow |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "unittest.ExceptionContext.__exit__")})},[self; typ; exn; trace], []) ->
         Eval.assume
           (mk_binop exn O_eq (mk_py_none range) range)
           ~fthen:(fun true_flow ->
             (* No exception raised => assertion failed *)
             let flow = man.exec (mk_assert_unreachable range) true_flow in
             Eval.empty_singleton flow
           )
           ~felse:(fun false_flow ->
             (* Check that the caught exception is an instance of the expected exception *)
             Eval.assume
               (Utils.mk_builtin_call "isinstance" [exn; (mk_py_attr self "expected" range)] range)
               ~fthen:(fun true_flow ->
                 let flow = man.exec (mk_assert_reachable range) true_flow in
                 Eval.singleton (mk_py_true range) flow
               )
               ~felse:(fun false_flow ->
                 let flow = man.exec (mk_assert_unreachable range) false_flow in
                 Eval.empty_singleton flow
               )
               man false_flow
           )
           man flow
         |> OptionExt.return

      | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin f)})}, _, _)
           when Addr.is_builtin_class_function "unittest.TestCase" f ->
         Framework.Exceptions.panic "unittest.TestCase function %s not implemented" f

      | _ -> None


    let ask _ _ _ = None

  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
