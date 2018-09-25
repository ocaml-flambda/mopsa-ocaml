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
    let eval_interface = {export = [Framework.Zone.Z_top, Framework.Zone.Z_top]; import = []}

    let init _ _ flow = Option.return flow

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
         man.eval (mk_not (mk_binop e1 O_py_is e2 range) range) flow |> Option.return
      (* re_eval_singleton man.eval (Some (mk_not (mk_binop e1 O_py_is e2 range) range), flow, [])*)

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_in, e1, e2) ->
         Debug.fail "FIXME: To implement"
         (* Eval.eval_list [e1; e2] man.eval flow |>
          *   Eval.bind (fun el flow ->
          *       let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          *       let o2 = object_of_expr e2 in
          *       let cls2 = Addr.class_of_object o2 in
          *       Eval.assume
          *         (Utils.mk_object_hasattr cls2 "__contains__" range)
          *         ~fthen:(fun true_flow ->
          *           let exp' = mk_py_call (mk_py_object_attr cls2 "__contains__" range) [e2; e1] range in
          *           man.eval exp' true_flow
          *         )
          *         ~felse:(fun false_flow ->
          *           Eval.assume
          *             (Utils.mk_object_hasattr cls2 "__iter__" range)
          *             ~fthen:(fun true_flow ->
          *               let v = mk_tmp () in
          *               let stmt = mk_stmt (S_py_for (
          *                                       mk_var v range,
          *                                       e2,
          *                                       mk_if
          *                                         (mk_binop (mk_var v range) O_eq e1 range)
          *                                         (mk_stmt S_break range)
          *                                         (mk_block [] range)
          *                                         range,
          *                                       (mk_block [] range)
          *                            )) range
          *               in
          *               let flow = man.exec stmt true_flow in
          *               man.eval (mk_var v range) flow |> Eval.add_cleaners [mk_remove_var v range]
          *             )
          *             ~felse:(fun false_flow ->
          *               Eval.assume
          *                 (Utils.mk_object_hasattr cls2 "__getitem__" range)
          *                 ~fthen:(fun true_flow ->
          *                   Framework.Exceptions.panic_at range "evaluating 'in' operator using __getitem__ not supported"
          *                 )
          *                 ~felse:(fun false_flow ->
          *                   let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
          *                   Eval.empty_singleton flow
          *                 )
          *                 man false_flow
          *             )
          *             man false_flow
          *         ) man flow
          *     ) |> Option.return *)

      (* E⟦ e1 in e2 ⟧ *)
      | E_binop(O_py_not_in, e1, e2) ->
         man.eval (mk_not (mk_binop e1 O_py_in e2 range) range) flow |> Option.return

      (* E⟦ not e ⟧ *)
      (* as S_assume, this is moved to a lower domain *)
      (* | E_unop(Framework.Ast.O_log_not, e') (\*when is_py_expr e'*\) ->
       *    Debug.fail "FIXME: To implement" *)
         (* man.eval e' flow |>
          *   Eval.bind (fun e' flow ->
          *       let o = object_of_expr e' in
          *       if Addr.isinstance o (Addr.find_builtin "bool") then
          *         let e = value_of_object o in
          *         let a = man.ask (Memory.Query.QInt e) flow |> Option.none_to_exn in
          *         match Memory.Value.I.can_be_true a, Memory.Value.I.can_be_false a with
          *         | true, false -> oeval_singleton (Some (mk_py_false range), flow, [])
          *         | false, true -> oeval_singleton (Some (mk_py_true range), flow, [])
          *         | true, true -> oeval_singleton (Some (mk_py_top T_bool range), flow, [])
          *         | false, false -> oeval_singleton (None, flow, [])
          *       else
          *         man.eval (Utils.mk_builtin_call "bool" [e'] range) flow |>
          *           eval_compose (fun ret flow ->
          *               let o = object_of_expr ret in
          *               let e = value_of_object o in
          *               let a = man.ask (Memory.Query.QInt e) flow |> Option.none_to_exn in
          *               match Memory.Value.I.can_be_true a, Memory.Value.I.can_be_false a with
          *               | true, false -> oeval_singleton (Some (mk_py_false range), flow, [])
          *               | false, true -> oeval_singleton (Some (mk_py_true range), flow, [])
          *               | true, true -> oeval_singleton (Some (mk_py_top T_bool range), flow, [])
          *               | false, false -> oeval_singleton (None, flow, [])
          *             )
          *     ) |> Option.return *)

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
             ) |> Option.return

      | _ -> None


    (* let rec exec zone (stmt:Framework.Ast.stmt) (man:('a, unit) man) (flow:'a flow) : 'a post option =
     *   (\* let range = srange stmt in *\)
     *   match skind stmt with
     *   (\* S⟦ ?e ⟧ *\)
     (*   moved to a lower domain *)
     *   | S_assume(e) when is_py_expr e ->
     *      Debug.fail "FIXME: assume: To implement"
     *
     *      (\* let check_bool e ~otherwise flow =
     *       *   let o = object_of_expr e in
     *       *   (\\* Universal.Utils.assume_to_exec
     *       *    *   (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [e; mk_py_object (Addr.find_builtin "bool") range] range)
     *       *    *   (fun true_flow -> *\\)
     *       *   if Addr.isinstance o (Addr.find_builtin "bool") then
     *       *     let true_flow = flow in
     *       *     let e = value_of_object o in
     *       *     let a = man.ask (Memory.Query.QInt e) true_flow |> Option.none_to_exn in
     *       *     begin match Memory.Value.I.can_be_true a, Memory.Value.I.can_be_false a with
     *       *     | true, false -> true_flow
     *       *     | false, true -> man.flow.set TCur man.env.bottom true_flow
     *       *     | true, true -> true_flow
     *       *     | false, false -> man.flow.set TCur man.env.bottom true_flow
     *       *     end
     *       *   else
     *       *     otherwise flow
     *       * in
     *       *
     *       * Some (man.eval e flow |>
     *       *   Post.bind man (fun e flow ->
     *       *       Post.of_flow @@
     *       *         check_bool e
     *       *           ~otherwise:(fun flow ->
     *       *             let post =
     *       *               Post.bind man (fun b flow ->
     *       *                   Post.of_flow @@ check_bool b
     *       *                                     ~otherwise:(fun flow ->
     *       *                                       Framework.Exceptions.fail "call to bool returned a non boolean expression %a" Framework.Ast.pp_expr b
     *       *                                     ) flow
     *       *                 )
     *       *                 (man.eval (Utils.mk_builtin_call "bool" [e] range) flow) in
     *       *             post.Post.flow
     *       *           ) flow
     *       *   )) *\)
     *
     *   | _ -> None *)
    let exec _ _ _ _ = None

    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
