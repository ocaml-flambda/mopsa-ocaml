(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of and/or operators. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr
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
    let eval_interface = {export = [any_zone, any_zone]; import = []}

    let init _ _ flow = OptionExt.return flow

    let eval zs exp (man:('a, unit) man) (flow:'a flow) : ('a, Framework.Ast.expr) evl option =
      let range = erange exp in
      match ekind exp with
      (* E⟦ e1 and e2 ⟧ *)
      | E_binop(O_py_and, e1, e2) ->
         Some (man.eval e1 flow |>
                 Eval.bind @@
                   fun e1 flow1 ->
                   Eval.assume e1 man
                     ~fthen:(fun true_flow -> man.eval e2 true_flow)
                     ~felse:(fun false_flow -> Eval.singleton e1 false_flow)
                     flow1)

      (* E⟦ e1 or e2 ⟧ *)
      | E_binop(O_py_or, e1, e2) ->
         Some (man.eval e1 flow |>
           Eval.bind @@
             fun e1 flow1 ->
             Eval.assume e1 man
               ~fthen:(fun true_flow -> Eval.singleton e1 true_flow)
               ~felse:(fun false_flow -> man.eval e2 false_flow)
               flow1)

      (* E⟦ e1 is e2 ⟧ *)
      | E_binop(O_py_is, e1, e2) ->
         Debug.fail "FIXME: To implement (maybe with id(e1)==id(e2), or to leave to another domain?)"
         (* Eval.eval_list [e1; e2] man.eval flow |>
          *   Eval.bind @@
          *     fun el flow ->
          *     let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          *     let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
          *     let cls1 = Addr.class_of_object o1 and cls2 = Addr.class_of_object o2 in
          *     if compare_py_object cls1 cls2 <> 0 then
          *       Eval.singleton (mk_py_false range) flow
          *     else
          *       let a1 = addr_of_object o1 and a2 = addr_of_object o2 in
          *       begin match Universal.Heap.Recency.is_weak a1, Universal.Heap.Recency.is_weak a2, compare_addr a1 a2 = 0 with
          *       | false, false, true -> Eval.singleton (mk_py_true range) flow
          *       | false, false, false -> Eval.singleton (mk_py_false range) flow
          *       | true, true, false -> Eval.singleton (mk_py_false range) flow
          *       | _ -> Eval.singleton (mk_py_top T_bool range) flow
          *       end *)

      (* E⟦ e1 is not e2 ⟧ *)
      | E_binop(O_py_is_not, e1, e2) ->
         man.eval (mk_not (mk_binop e1 O_py_is e2 range) range) flow |> OptionExt.return

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_in, e1, e2) ->
         Eval.eval_list [e1; e2] man.eval flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

               man.eval (mk_py_call (mk_py_object (Addr.find_builtin "type") range) [e2] range) flow |>
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
                             let v = mk_tmp () in
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
                                 Framework.Exceptions.panic_at range "evaluating 'in' operator using __getitem__ not supported"
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
