(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python standard library. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr


module Domain =
  struct

    type _ domain += D_python_libs_stdlib : unit domain

    let id = D_python_libs_stdlib
    let name = "python.libs.stdlib"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_libs_stdlib -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = { export = []; import = [] }
    let eval_interface = { export = [any_zone, any_zone]; import = [] }

    let init _ _ flow = Some flow

    let exec _ _ _ _ = None

    let eval zones exp man flow =
      let range = exp.erange in
      match ekind exp with
      (* Calls to iter built-in function *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "iter")}, _)},
                  [obj], []) ->
         (* Check that the class of obj has an attribute __iter__ *)
         man.eval obj flow |>
           Eval.bind (fun eobj flow ->
               man.eval (mk_py_type eobj range) flow |>
                 Eval.bind (fun cls' flow ->
                     let cls = object_of_expr cls' in
                     Eval.assume
                       (Utils.mk_object_hasattr cls "__iter__" range)
                       ~fthen:(fun true_flow ->
                         (* Call iter and check that it returns an object with an attribute __next__ *)
                         man.eval (mk_py_call (mk_py_object_attr cls "__iter__" range) [eobj] range) true_flow |>
                           Eval.bind (fun iter flow ->
                               Eval.assume
                                 (Utils.mk_hasattr iter "__next__" range)
                                 ~fthen:(fun true_flow -> Eval.singleton iter true_flow)
                                 ~felse:(fun false_flow ->
                                   man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                                     Eval.empty_singleton)
                                 man flow
                             )
                       )
                       ~felse:(fun false_flow ->
                         man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                           Eval.empty_singleton)
                       man flow
                   )
             )
         |> OptionExt.return

      (* Calls to len built-in function *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "len")}, _)},
                  [obj], [])  ->
         Exceptions.panic "todo@\n"
         (* Check that the class of obj has an attribute __len__ *)
         man.eval obj flow |>
           Eval.bind (fun eobj flow ->
               man.eval (mk_py_type eobj range) flow |>
                 Eval.bind (fun cls flow ->
                     let cls = object_of_expr cls in
                     Eval.assume
                       (Utils.mk_object_hasattr cls "__len__" range)
                       ~fthen:(fun true_flow ->
                         (* Call __len__ and check that it returns an integer *)
                         man.eval (mk_py_call (mk_py_object_attr cls "__len__" range) [eobj] range) true_flow |>
                           Eval.bind (fun len flow ->
                               match etyp len with
                               | T_int -> Eval.singleton len flow
                               | _ ->
                                  man.exec (Utils.mk_builtin_raise "TypeError" range) true_flow |>
                                    Eval.empty_singleton
                             )
                       )
                       ~felse:(fun false_flow ->
                         man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                           Eval.empty_singleton)
                       man flow
                   )
             )
         |> OptionExt.return

      (* Calls to built-in function next *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "next")}, _)},
                  [obj], [])  ->
         (* Check that the class of obj has an attribute __next__ *)
         man.eval obj flow |>
           Eval.bind (fun eobj flow ->
               man.eval (mk_py_type eobj range) flow |>
                 Eval.bind (fun cls flow ->
                     let cls = object_of_expr cls in
                     Eval.assume
                       (Utils.mk_object_hasattr cls "__next__" range)
                       ~fthen:(fun true_flow ->
                         man.eval (mk_py_call (mk_py_object_attr cls "__next__" range) [eobj] range) true_flow
                       )
                       ~felse:(fun false_flow ->
                         man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                           Eval.empty_singleton)
                       man flow
                   )
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "input")}, _)}, args, [])  ->
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         if List.length args <= 1 then
           man.eval (mk_py_top T_string range) flow |> OptionExt.return
         else
           tyerror flow |> OptionExt.return


      | _ ->
         None

    let ask _ _ _ = None

  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
