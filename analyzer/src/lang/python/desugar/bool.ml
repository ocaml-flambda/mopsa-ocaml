(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of and/or operators. *)

open Mopsa
open Ast
open Addr
open Universal.Ast
open Zone


module Domain =
  struct
    type _ domain += D_python_desugar_bool : unit domain

    let id = D_python_desugar_bool
    let name = "python.desugar.bool"
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_python_desugar_bool -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py]; import = [Zone.Z_py, Zone.Z_py]}

    let init _ _ flow = OptionExt.return flow

    let eval zs exp (man:('a, unit) man) (flow:'a flow) : ('a, Framework.Ast.expr) evl option =
      let range = erange exp in
      match ekind exp with
      (* E⟦ e1 and e2 ⟧ *)
      | E_binop(O_py_and, {ekind = E_constant (C_bool true)}, e2) ->
         man.eval e2 flow |> OptionExt.return

      | E_binop(O_py_and, {ekind = E_constant (C_bool false)}, e2) ->
         Eval.singleton (mk_py_false range) flow |> OptionExt.return

      | E_binop(O_py_and, e1, e2) ->
         Some (man.eval e1 flow |>
                 Eval.bind @@
                   fun e1 flow1 ->
                   Eval.assume e1 man
                     ~fthen:(fun true_flow -> man.eval e2 true_flow)
                     ~felse:(fun false_flow -> Eval.singleton e1 false_flow)
                     flow1)

      (* | E_binop(O_py_or, {ekind = E_constant (C_bool true)}, e2) ->
       *    Eval.singleton (mk_py_true range) flow |> OptionExt.return
       *
       * | E_binop(O_py_or, {ekind = E_constant (C_bool false)}, e2) ->
       *    man.eval e2 flow |> OptionExt.return *)

      (* E⟦ e1 or e2 ⟧ *)
      | E_binop(O_py_or, e1, e2) ->
         man.eval e1 flow |>
           Eval.bind (fun e1 flow1 ->
               Eval.assume e1
                 ~fthen:(fun true_flow -> Eval.singleton e1 true_flow)
                 ~felse:(fun false_flow -> man.eval e2 false_flow)
                 man flow1)
         |> OptionExt.return
      (* combinatorial explosion *)
      (* man.eval (mk_expr (E_py_if (e1, e1, e2)) range) flow |> OptionExt.return *)

      (* E⟦ e1 is not e2 ⟧ *)
      | E_binop(O_py_is_not, e1, e2) ->
         man.eval (mk_not (mk_binop e1 O_py_is e2 range) range) flow |> OptionExt.return

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_in, e1, e2) ->
         Eval.eval_list [e1; e2] man.eval flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

               man.eval (mk_py_type e2 range) flow |>
                 Eval.bind (fun cls2 flow ->
                     Eval.assume
                       (Utils.mk_hasattr cls2 "__contains__" range)
                       ~fthen:(fun true_flow ->
                         let exp' = mk_py_call (mk_py_attr cls2 "__contains__" range) [e2; e1] range in
                         man.eval exp' true_flow
                       )
                       ~felse:(fun false_flow ->
                         Eval.assume
                           (Utils.mk_hasattr cls2 "__iter__" range)
                           ~fthen:(fun true_flow ->
                             let v = mktmp () in
                             let stmt = mk_stmt (S_py_for (
                                                     mk_var v range,
                                                     e2,
                                                     mk_if
                                                       (mk_binop (mk_var v range) O_eq e1 range)
                                                       (mk_stmt S_break range)
                                                       (mk_block [] range)
                                                       range,
                                                     (mk_block [] range)
                                          )) range
                             in
                             let flow = man.exec stmt true_flow in
                             man.eval (mk_var v range) flow |> Eval.add_cleaners [mk_remove_var v range]
                           )
                           ~felse:(fun false_flow ->
                             Eval.assume
                               (Utils.mk_hasattr cls2 "__getitem__" range)
                               ~fthen:(fun true_flow ->
                                 panic_at range "evaluating 'in' operator using __getitem__ not supported"
                               )
                               ~felse:(fun false_flow ->
                                 let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                                 Eval.empty_singleton flow
                               )
                               man false_flow
                           )
                           man false_flow
                       ) man flow
                   )
             )
         |> OptionExt.return

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_not_in, e1, e2) ->
         man.eval (mk_not (mk_binop e1 O_py_in e2 range) range) flow |> OptionExt.return

      (* E⟦ e1 op e2 op e3 ... ⟧ *)
      | E_py_multi_compare(left, ops, rights) ->
         debug "multi compare";
         let range = erange exp in
         man.eval left flow |>
           Eval.bind (fun left flow ->
               debug "left evaluated";
               let rec aux left flow = function
                 | [] ->
                    debug "leaf case -> true";
                    Eval.singleton (mk_py_true range) flow

                 | (op, right) :: tl ->
                    man.eval right flow |>
                      Eval.bind (fun right flow ->
                          Eval.assume
                            (mk_binop left op right range)
                            ~fthen:(fun true_flow -> aux right true_flow tl)
                            ~felse:(fun false_flow -> Eval.singleton (mk_py_false range) flow)
                            man flow
                        )
               in
               aux left flow (List.combine ops rights)
             ) |> OptionExt.return

      | _ -> None


    let exec _ _ _ _ = None

    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
