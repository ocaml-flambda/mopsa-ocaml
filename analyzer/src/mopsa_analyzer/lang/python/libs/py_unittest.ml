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

(** Unittest library. *)

(* Originally written by Abdelraouf Ouadjaout *)

open Mopsa
open Sig.Abstraction.Stateless
open Addr
open Ast
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.libs.unittest"
      end)

    let checks = []

    let init _ _ flow = flow

    let exec _ _ _ = None

    let test_functions_from_cls (cls: Ast.py_clsdec) : Ast.py_fundec list =
      match skind cls.py_cls_body with
      | S_block (stmts, _) ->
        List.fold_left (fun tests stmt ->
            match skind stmt with
            | S_py_function f when String.sub f.py_func_var.vname 0 4 = "test" ->
              f :: tests
            | _ -> tests) [] stmts
      | _ -> assert false

    let eval exp man flow =
      let range = exp.erange in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.main", _))}, _)}, [], []) ->
       debug "Search for all classes that inherit from TestCase";
       let test_cases = man.ask Universal.Heap.Recency.Q_allocated_addresses flow |>
                          List.filter (fun addr ->
                            match addr.addr_kind with
                            | A_py_class(cls, _ :: ({addr_kind = A_py_class (C_user u, _)}, _) :: _) ->
                               get_orig_vname u.py_cls_var = "TestCase"
                            | _ -> false
                          ) |>
                        List.map (fun addr ->
                            match addr.addr_kind with
                            | A_py_class(C_user cls, _) -> addr, cls
                            | _ -> assert false
                          )
       in

       let tests = List.rev @@ List.fold_left (fun other_tests (cls_addr, cls_decl) ->
           (* create tmp, alloc class in tmp, run tests, delete tmp *)
           let tmp = mktmp ~typ:(T_py None) () in
           let tmpvar = mk_var tmp range in
           let assign_alloc = mk_assign tmpvar (mk_py_call (mk_var cls_decl.py_cls_var range) [] range) range in
           let test_calls =
             List.rev @@
             List.map
               (fun func ->
                  mk_stmt (S_expression (mk_py_call
                                           (mk_var func.py_func_var range)
                                           [tmpvar] range)) range )
               (test_functions_from_cls cls_decl)
           in
           let stmt = mk_block
               (assign_alloc :: test_calls @ [mk_remove_var tmp range])
               range in
           debug "stmts for %s =@[@\n%a@]" cls_decl.py_cls_var.vname pp_stmt stmt;
           (cls_decl.py_cls_var.vname ^ "'s tests", stmt) :: other_tests
         ) [] test_cases in
       debug "|tests| = %d" (List.length tests);
       man.exec (mk_stmt (Universal.Ast.S_unit_tests tests) range) flow >>%
       man.eval (mk_py_none range)
       |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertEqual", _))}, _)}, [test; arg1; arg2; _], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertEqual", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_eq arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertGreater", _))}, _)}, [test; arg1; arg2; _], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertGreater", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_ge arg2 range) range flow
         |> OptionExt.return


      (* FIXME: handle message *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertTrue", _))}, _)}, [test; cond;_], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertTrue", _))}, _)}, [test; cond], []) ->
        Py_mopsa.check man (Utils.mk_builtin_call "bool" [cond] range) range flow
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.fail", _))}, _)}, [test; cond], []) ->
        Py_mopsa.check man (mk_false range) range flow
        |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertFalse", _))}, _)}, [test; cond; _], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertFalse", _))}, _)}, [test; cond], []) ->
         Py_mopsa.check man (mk_py_not cond range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertIs", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_py_is arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertIsNot", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_py_is_not arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertIn", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_py_in arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertNotIn", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_py_not_in arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertIsInstance", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (Utils.mk_builtin_call "isinstance" [arg1; arg2] range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertNotIsInstance", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_not (Utils.mk_builtin_call "isinstance" [arg1; arg2] range) range) range flow
         |> OptionExt.return

      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "unittest.TestCase.assertRaises")}, _)}, test :: exn :: f :: args, []) ->
       *    let stmt = mk_try
       *                 (mk_block [
       *                      mk_stmt (S_expression (mk_py_call f args range)) range;
       *                      mk_assert_unreachable range
       *                    ] range)
       *                 [
       *                   mk_except
       *                     (Some exn)
       *                     None
       *                     (mk_assert_reachable range);
       *                   mk_except
       *                     None
       *                     None
       *                     (mk_assert_unreachable range)
       *                 ]
       *                 (mk_block [] range)
       *                 (mk_block [] range)
       *                 range
       *    in
       *    let flow = man.exec stmt flow in
       *    Eval.singleton (mk_py_none range) flow
       *    |> OptionExt.return *)

      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertRaises", _))}, _)}, [test; exn], []) ->
       *    (\* Instantiate ExceptionContext with the given exception exn *\)
       *    let exp' = mk_call "unittest.ExceptionContext" [exn] range in
       *    man.eval exp' flow |> OptionExt.return *)

      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertRaises", _))}, _)}, _, _) ->
       *    Py_mopsa.check man (mk_py_top T_py_bool range) range flow
       *    |> OptionExt.return *)

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.ExceptionContext.__exit__", _))}, _)},[self; typ; exn; trace], []) ->
         assume
           (mk_binop ~etyp:(T_py None) exn O_eq (mk_py_none range) range)
           ~fthen:(fun true_flow ->
             debug "Assertion failed!";
             (* No exception raised => assertion failed *)
             Py_mopsa.check man (mk_py_false range) range flow
           )
           ~felse:(fun false_flow ->
             (* Check that the caught exception is an instance of the expected exception *)
             assume
               (Utils.mk_builtin_call "isinstance" [exn; (mk_py_attr self "expected" range)] range)
               ~fthen:(fun true_flow ->
                 debug "Good exception!";
                 man.eval (mk_py_true range) flow
               )
               ~felse:(fun false_flow ->
                 debug "Bad exception!";
                 Py_mopsa.check man (mk_py_false range) range flow
               )
               man false_flow
           )
           man flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, args, _)
           when is_builtin_class_function "unittest.TestCase" f ->
         panic "unittest.TestCase function %s not implemented (with |args| = %d)" f (List.length args)

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.skipUnless", _))}, _)}, [], _) ->
         failwith "todo"


      | _ -> None


    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()

  end

let () = register_stateless_domain (module Domain)
