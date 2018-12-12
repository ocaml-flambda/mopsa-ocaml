(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python standard library. *)

open Mopsa
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
                               Eval.assume
                                 (mk_py_isinstance_builtin len "int" range)
                                 ~fthen:(fun true_flow ->
                                   Eval.singleton len true_flow)
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

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "sum")}, _)} as call, [els], [])  ->
         let args' = els :: (mk_constant T_int (C_int (Z.of_int 0)) range) :: [] in
         man.eval {exp with ekind = E_py_call(call, args', [])} flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "sum")}, _)}, [ els; start ], [])  ->
      (* let's desugar sum into tmp = 0; for x in els: tmp = tmp + x; tmp *)
         let counter = mk_tmp () in
         let counter_var = mk_var counter range in
         let target = mk_tmp () in
         let target_var = mk_var target range in
         let assign = mk_assign counter_var (mk_constant T_int (C_int (Z.of_int 0)) range) range in
         let pass = mk_block [] range in
         let for_loop = mk_stmt (S_py_for (target_var, els,
                                           mk_assign counter_var (mk_binop counter_var O_plus target_var range) range,
                                           pass)) range in
         let stmt = mk_block (assign :: for_loop :: []) range in
         debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
         man.exec stmt flow |>
           man.eval counter_var |>
           Eval.add_cleaners [mk_remove_var counter range; mk_remove_var target range] |>
           OptionExt.return

      (* I prefer the version in typing.ml for now... *)
      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "max")}, _)}, [iterable], []) ->
       *    (\* desugaring max(iterable) into:
       *     *    tmp1 = iter(iterable)
       *     *    max = next(tmp1)
       *     *    for x in tmp1:
       *     *        if x > max:
       *     *            max = x *\)
       *    let iter = mk_tmp () in
       *    let iter_var = mk_var iter range in
       *    let maxi = mk_tmp () in
       *    let maxi_var = mk_var maxi range in
       *    let target = mk_tmp () in
       *    let target_var = mk_var target range in
       *
       *    let cleaners = List.map (fun x -> mk_remove_var x range) [iter; maxi; target] in
       *    let pass = mk_block [] range in
       *
       *    let assign_iter = mk_assign iter_var (Utils.mk_builtin_call "iter" [iterable] range) range in
       *    let assign_max =
       *      Utils.mk_try_stopiteration
       *        (mk_assign maxi_var (Utils.mk_builtin_call "next" [iter_var] range) range)
       *        (Utils.mk_builtin_raise "ValueError" range)
       *      range in
       *    let for_stmt = mk_stmt (S_py_for (target_var, iter_var,
       *                                      mk_if (mk_binop target_var O_gt maxi_var range)
       *                                        (mk_assign maxi_var target_var range)
       *                                        pass range
       *                                      , pass)) range in
       *    let stmt = mk_block (assign_iter :: assign_max :: for_stmt :: []) range in
       *    debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
       *    man.exec stmt flow |>
       *      man.eval maxi_var |>
       *      Eval.add_cleaners cleaners |>
       *      OptionExt.return *)

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "max")}, _)}, [e1; e2], []) ->
         (* desugaring max(e1, e2) into if e1 > e2 then e1 else e2 *)
         let expr = mk_expr (E_py_if (mk_binop e1 O_gt e2 range, e1, e2)) range in
         debug "Rewriting %a into %a@\n" pp_expr exp pp_expr expr;
         man.eval expr flow |> OptionExt.return

      | _ ->
         None

    let ask _ _ _ = None

  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
