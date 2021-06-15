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

let () = Universal.Heap.Policies.register_mk_addr
           (fun default ak -> match ak with
                              | A_py_instance {addr_kind = A_py_class (C_user c, _)} when get_orig_vname c.py_cls_var = "ExceptionContext" && Filename.basename (Location.get_range_file c.py_cls_range) = "unittest.py" ->
                                 Universal.Heap.Policies.mk_addr_range ak
                              | _ -> default ak)


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.libs.unittest"
      end)

    let checks = []

    let init _ _ flow = flow

    let exec _ _ _ = None

    let test_functions_and_setup_from_cls (cls: Ast.py_clsdec) (osetup, test_functions) : (Ast.py_fundec option * Ast.py_fundec list) =
      match skind cls.py_cls_body with
      | S_block (stmts, _) ->
        List.fold_left (fun (osetup, tests) stmt ->
            match skind stmt with
            | S_py_function f ->
               if String.sub f.py_func_var.vname 0 4 = "test" && not (List.exists (fun f' -> get_orig_vname f.py_func_var = get_orig_vname f'.py_func_var) test_functions) then
                 (osetup, f :: tests)
               else if get_orig_vname f.py_func_var = "setUp" && osetup = None then
                 (Some f, tests)
               else
                 (osetup, tests)
            | _ -> (osetup, tests)) (osetup, test_functions) stmts
      | _ -> assert false

    let eval exp man flow =
      let range = exp.erange in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.main", _))}, _)}, [], []) ->
       (* FIXME: tearDown *)
       debug "Search for all classes that inherit from TestCase";
       let test_cases = man.ask Q_allocated_addresses flow |>
                          List.filter (fun addr ->
                            match addr.addr_kind with
                            | A_py_class(cls, _ :: mro) ->
                               List.exists (fun (addr, _) ->
                                   match akind addr with
                                   | A_py_class (C_user u, _) ->
                                      get_orig_vname u.py_cls_var = "TestCase"
                                   | _ -> false
                                 ) mro
                            | _ -> false
                          ) |>
                        List.map (fun addr ->
                            match addr.addr_kind with
                            | A_py_class(C_user cls, _) -> addr, cls
                            | _ -> assert false
                          ) |>
                          List.sort (fun (a1, _) (a2, _) ->
                              match akind a1, akind a2 with
                              | A_py_class(C_user cls1, _),
                                A_py_class(C_user cls2, _) ->
                                 compare_var cls1.py_cls_var cls2.py_cls_var
                              | _ -> assert false)
       in
       debug "|tests classes| = %d" (List.length test_cases);
       let init_flow = flow in
       let flow =
         List.fold_left (fun acc (cls_addr, cls_decl) ->
           (* create tmp, alloc class in tmp, run tests, delete tmp *)
             debug "%a, cur is bottom?%b" pp_addr cls_addr (man.lattice.is_bottom (Flow.get T_cur man.lattice flow));
             let tmp = mktmp ~typ:(T_py None) () in
             let tmpvar = mk_var tmp range in
             let assign_alloc = mk_assign tmpvar (mk_py_call (mk_var cls_decl.py_cls_var range) [] range) range in
             let osetup, test_functions =
               let mro = match akind cls_addr with
                 | A_py_class(C_user cls, mro) -> mro
                 | _ -> assert false in
               List.fold_left (fun (osetup, test_functions) cls ->
                   match akind @@ fst cls with
                   | A_py_class (C_user cls, _) ->
                      debug "cls %a" pp_var cls.py_cls_var;
                      test_functions_and_setup_from_cls cls (osetup, test_functions)
                   | _ -> (osetup, test_functions)) (None, []) mro
             in
             if List.length test_functions > 0 then
               let () = debug "osetup = %a, |test_functions|=%d" (OptionExt.print pp_var) (OptionExt.lift (fun fdec -> fdec.py_func_var) osetup) (List.length test_functions) in
               let test_calls =
                 List.rev @@
                   List.map
                     (fun func ->
                       get_orig_vname func.py_func_var,
                       mk_stmt (S_expression (mk_py_call
                                                (mk_var func.py_func_var range)
                                                [tmpvar] range)) range )
                     test_functions
               in
               let flow = Flow.copy_ctx acc init_flow in
               Flow.join man.lattice acc
               (post_to_flow man
                 (
                     man.exec assign_alloc flow >>%
                     fun flow ->
                     (
                       match osetup with
                       | None -> Post.return flow
                       | Some setup ->
                          man.exec (mk_stmt (S_expression (mk_py_call (mk_var setup.py_func_var range) [tmpvar] range)) range) flow
                     ) >>%
                       man.exec (mk_stmt (Universal.Ast.S_unit_tests test_calls) cls_decl.py_cls_range) >>%
                       man.exec (mk_remove_var tmp range)
                 )
               )
             else acc
           ) flow test_cases in
       Post.return flow >>%
       man.eval (mk_py_none range)
       |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertEqual", _))}, _)}, [test; arg1; arg2; _], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertEqual", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_eq arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertNotEqual", _))}, _)}, [test; arg1; arg2; _], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertNotEqual", _))}, _)}, [test; arg1; arg2], []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_ne arg2 range) range flow
         |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertGreater", _))}, _)}, test :: arg1 :: arg2 :: _, []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_gt arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertGreaterEqual", _))}, _)}, test :: arg1 :: arg2 :: _, []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_ge arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertLess", _))}, _)}, test :: arg1 :: arg2 :: _, []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_lt arg2 range) range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertLessEqual", _))}, _)}, test :: arg1 :: arg2 :: _, []) ->
         Py_mopsa.check man (mk_binop ~etyp:(T_py None) arg1 O_le arg2 range) range flow
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

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertRaises", _))}, _)} as caller, test :: exn :: f :: args, []) ->
         (* rewriting into: with exn: f(args) *)
         let stmt = mk_stmt (S_py_with ({exp with ekind = E_py_call(caller, [test;exn], [])}, None,
                      mk_stmt (S_expression (mk_py_call f args range)) range)) range
         in
         man.exec stmt flow >>%
         Eval.singleton (mk_py_none range)
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.TestCase.assertRaises", _))}, _)}, [test; exn], []) ->
         (* Instantiate ExceptionContext with the given exception exn *)
         let unittest = OptionExt.none_to_exn @@ man.ask (Desugar.Import.Q_python_addr_of_module "unittest") flow in
         let exp' = mk_py_call (mk_py_attr (mk_py_object (unittest, None) range) "ExceptionContext" range) [exn] range in
         man.eval exp' flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("unittest.ExceptionContext.__exit__", _))}, _)},[self; typ; exn; trace], []) ->
         let r = man.eval exn flow >>$ (fun exn flow ->
          debug "ExceptionContext: self = %a, exn = %a" pp_expr self pp_expr exn;
         assume
           (mk_binop ~etyp:(T_py None) exn O_eq (mk_py_none range) range)
           ~fthen:(fun flow ->
             (* No exception raised => assertion failed *)
             Py_mopsa.check man (mk_py_false range) range flow >>$ fun _ flow ->
             man.eval (mk_py_false range) flow
           )
           ~felse:(fun flow ->
             (* Check that the caught exception is an instance of the expected exception *)
             assume
               (Utils.mk_builtin_call "isinstance" [exn; (mk_py_attr self "expected" range)] range)
               ~fthen:(fun true_flow ->
                 man.eval (mk_py_attr self "regex" range) flow >>$
                   fun reg flow ->
                   debug "now!";
                   assume (eq ~etyp:(T_py None) reg (mk_py_none range) range)
                     man flow
                     ~fthen:(fun flow ->
                       Py_mopsa.check man (mk_py_true range) range flow >>$
                         fun _ flow ->
                         man.eval (mk_py_true range) flow
                     )
                     ~felse:(fun flow ->
                       let regex = Universal.Strings.Powerset.StringPower.choose @@
                                     man.ask (Universal.Strings.Powerset.mk_strings_powerset_query (Utils.extract_oobject reg)) flow in
                       man.eval (mk_py_index_subscript (mk_py_attr exn "args" range) (mk_zero ~typ:(T_py None) range) range) flow >>$
                         fun exc_msg flow ->
                         let exc_powerset = man.ask (Universal.Strings.Powerset.mk_strings_powerset_query (Utils.extract_oobject exc_msg)) flow in
                         if Universal.Strings.Powerset.StringPower.is_top exc_powerset then
                             Py_mopsa.check man (mk_py_top T_bool range) range flow >>$
                               fun _ flow ->
                               man.eval (mk_py_true range) flow
                         else
                           let exc_msg = Universal.Strings.Powerset.StringPower.choose exc_powerset in
                           let quoted_regex =
                             if String.length regex >= 2 && String.sub regex (String.length regex - 2) 2 = ".*" then
                               Str.quote (String.sub regex 0 (String.length regex - 2)) ^ ".*"
                             else
                               Str.quote regex in
                           let re = Str.regexp quoted_regex in
                           let () = warn_at range "assertRaisesRegex hackish implementation, only .* at the end is supported" in
                           let () = debug "assertRaisesRegex regex=%s exc_msg=%s" quoted_regex exc_msg in
                           if Str.string_match re exc_msg 0 then
                             Py_mopsa.check man (mk_py_true range) range flow >>$
                               fun _ flow ->
                               man.eval (mk_py_true range) flow
                           else
                             Py_mopsa.check man (mk_py_false range) range flow >>$ fun _ flow ->
                             man.eval (mk_py_false range) flow

                     )
               )
               ~felse:(fun flow ->
                 debug "not the good type of exn";
                 man.eval (mk_py_false range) flow
               )
               man flow
           )
           man flow
           ) in
         (* let _ = r >>$ fun r flow ->
          *                debug "r = %a@.flow=%a" pp_expr r (format man.lattice.print) (Flow.get T_cur man.lattice flow);
          *                Cases.empty flow in *)
         r |> OptionExt.return

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
