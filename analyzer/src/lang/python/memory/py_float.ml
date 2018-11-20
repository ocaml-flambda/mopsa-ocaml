(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python floating point numbers. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr
open Addr_env

module Domain = struct
  type _ domain += D_python_memory_float : unit domain

  let id = D_python_memory_float
  let name = "python.memory.float"
  let identify : type a. a domain -> (unit, a) eq option = function
    | D_python_memory_float -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [any_zone, any_zone]; import = []}

  let init _ _ flow = Some flow

  let mk_py_float n range =
    let mk_py_float_expr e range =
      let addr = {addr_kind = A_py_instance (find_builtin "float", None);
                  addr_uid = Universal.Heap.Pool.get_fresh ()} in
      mk_py_object (addr, e) range
    in
    mk_py_float_expr (Universal.Ast.mk_float n range) range

  let rec eval zs exp man flow =
    let range = exp.erange in
    match ekind exp with
    (* 𝔼⟦ float.__new__(cls, arg) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "float.__new__")}, _)}, cls :: args, []) ->
       Debug.fail "todo"
    (* begin match args with
       *   | [] -> oeval_singleton (Some (mk_py_float 0.0 range), flow, [])
       *
       *   | [arg] ->
       *     let obj = object_of_expr arg in
       *     begin match type_of_object obj with
       *       | T_float -> oeval_singleton (Some arg, flow, [])
       *       | T_int -> new_float_from_int man ctx arg range flow
       *       | _ -> new_float_from_object man ctx arg range flow
       *     end
       *
       *
       *   | _ ->
       *     let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *     oeval_singleton (None, flow, [])
       * end *)

    (* 𝔼⟦ f | f ∈ R ⟧ *)
    | E_constant (C_float f) ->
      Eval.singleton (mk_py_float f range) flow |> OptionExt.return

    (* 𝔼⟦ float.__op__(e1, e2) | op ∈ {+, -, x, ...} ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
      when is_arithmetic_op_fun f ->
       Debug.fail "todo"
      (* eval_list [e1; e2] (man.eval ctx) flow |>
       * eval_compose (fun el flow ->
       *     let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
       *     let o1 = object_of_expr e1 in
       *     if not (Addr.isinstance o1 (Addr.find_builtin "float")) then
       *       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *       oeval_singleton (None, flow, [])
       *     else
       *       let e2' = Utils.mk_builtin_call "float" [e2] range in
       *       man.eval ctx e2' flow |>
       *       eval_compose (fun e2 flow ->
       *           let o2 = object_of_expr e2 in
       *           let ev1 = value_of_object o1 and ev2 = value_of_object o2 in
       *           let op = arithmetic_op f in
       *           oeval_singleton (Some (mk_py_float_expr (mk_binop ev1 op ev2 ~etyp:T_float range) range), flow, [])
       *         )
       *   ) *)

    (* 𝔼⟦ float.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
      when is_compare_op_fun f ->
       Debug.fail "todo"
      (* eval_list [e1; e2] (man.eval ctx) flow |>
       * eval_compose (fun el flow ->
       *     let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
       *     let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
       *     if not (Addr.isinstance o1 (Addr.find_builtin "float")) then
       *       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *       oeval_singleton (None, flow, [])
       *     else
       *     if not (Addr.isinstance o2 (Addr.find_builtin "float") || Addr.isinstance o2 (Addr.find_builtin "int")) then
       *       oeval_singleton (Some (mk_py_not_implemented range), flow, [])
       *     else
       *       let e2' = Utils.mk_builtin_call "float" [e2] range in
       *       man.eval ctx e2' flow |>
       *       eval_compose (fun e2 flow ->
       *           let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
       *           let ev1 = value_of_object o1 and ev2 = value_of_object o2 in
       *           let op = compare_op f in
       *           Universal.Utils.assume_to_eval
       *             (mk_binop ev1 op ev2 ~etyp:T_bool range)
       *             (fun true_flow -> oeval_singleton (Some (mk_py_true range), true_flow, []))
       *             (fun false_flow -> oeval_singleton (Some (mk_py_false range), false_flow, []))
       *             man ctx flow ()
       *         )
       *   ) *)

    (* 𝔼⟦ float.__op__(e1, e2, ..., en) | n != 2 ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, _, [])
      when is_arithmetic_op_fun f || is_compare_op_fun f ->
       let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
       Eval.empty_singleton flow |> OptionExt.return

    (* 𝔼⟦ float.__bool__(self) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "float.__bool__")}, _)}, [self], []) ->
       Debug.fail "todo"
      (* man.eval ctx self flow |>
       * eval_compose (fun self flow ->
       *     let o = object_of_expr self in
       *     if not (Addr.isinstance o (Addr.find_builtin "float")) then
       *       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *       oeval_singleton (None, flow, [])
       *     else
       *       Universal.Utils.assume_to_eval
       *         (mk_binop self O_eq (mk_py_float 0.0 range) range)
       *         (fun true_flow -> oeval_singleton (Some (mk_py_false range), flow, []))
       *         (fun false_flow -> oeval_singleton (Some (mk_py_true range), flow, []))
       *         (\* ~merge_case:(fun _ _ -> oeval_singleton (Some (mk_py_top T_bool range), flow, [])) *\)
       *         man ctx flow ()
       *   ) *)

    (* 𝔼⟦ float.__bool__(arg1, arg2, ..., argn) | n != 1 ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "float.__bool__")}, _)}, _, []) ->
       let flow = man.exec  (Utils.mk_builtin_raise "TypeError" range) flow in
       Eval.empty_singleton flow |> OptionExt.return

    | _ -> None


  (* and new_float_from_int man ctx arg range flow =
   *   Framework.Exceptions.panic_at range "float(int) not supported"
   *
   *
   * and new_float_from_object man ctx arg range flow =
   *   (\* Check for the presence of __float__ attribute *\)
   *   let obj = object_of_expr arg in
   *   Universal.Utils.assume_to_eval
   *     (Utils.mk_hasattr arg "__float__" range)
   *     (fun true_flow ->
   *        (\* Check the return value of arg.__float__ *\)
   *        man.eval ctx (mk_py_call (mk_py_object_attr (class_of_object obj) "__float__" range) [arg] range) true_flow |>
   *        eval_compose (fun ret flow ->
   *            match type_of_object @@ object_of_expr ret with
   *            | T_float -> oeval_singleton (Some ret, flow, [])
   *            | _ ->
   *              let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
   *              oeval_singleton (None, flow, [])
   *          )
   *     )
   *     (fun false_flow ->
   *        let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
   *        oeval_singleton (None, flow, [])
   *     )
   *     man ctx flow () *)

  and is_arithmetic_op_fun = function
    | "float.__add__"
    | "float.__sub__"
    | "float.__mul__" -> true
    | _ -> false

  and is_compare_op_fun = function
    | "float.__eq__"
    | "float.__ne__"
    | "float.__lt__"
    | "float.__le__"
    | "float.__gt__"
    | "float.__ge__" -> true
    | _ -> false

  and arithmetic_op = function
    | "float.__add__" -> O_plus
    | "float.__sub__" -> O_minus
    | "float.__mul__" -> O_mult
    | f -> Framework.Exceptions.panic "arithmetic_op: %s not yet supported" f

  and compare_op = function
    | "float.__eq__" -> O_eq
    | "float.__ne__" -> O_ne
    | "float.__lt__" -> O_lt
    | "float.__le__" -> O_le
    | "float.__gt__" -> O_gt
    | "float.__ge__" -> O_ge
    | _ -> assert false



  let exec _ _ _ _ = None
  let ask _ _ _ = None


end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
