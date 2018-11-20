(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python integer numbers. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr
open Addr_env

module Domain = struct
  type _ domain += D_python_memory_int : unit domain

  let id = D_python_memory_int
  let name = "python.memory.int"
  let identify : type a. a domain -> (unit, a) eq option = function
    | D_python_memory_int -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [any_zone, any_zone]; import = []}

  let init _ _ flow = Some flow

  let mk_py_int_expr e range =
    let addr = {addr_kind = A_py_instance (find_builtin "int", None);
                addr_uid = Universal.Heap.Pool.get_fresh ()} in
    mk_py_object (addr, e) range

  let mk_py_int n range = mk_py_int_expr (Universal.Ast.mk_z n range) range


  let rec eval zs exp man flow =
    let range = exp.erange in
    match ekind exp with
    (* 𝔼⟦ int.__new__(cls, arg) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__new__")}, _)}, cls :: args, kwds) ->
       Debug.fail "todo@\n"
    (* begin match args with
       *   | [] -> oeval_singleton (Some (mk_py_zero range), flow, [])
       *
       *   | [arg] ->
       *     let obj = object_of_expr arg in
       *     if Addr.isinstance obj (Addr.find_builtin "int") then
       *       let e = value_of_object obj in
       *       oeval_singleton (Some (mk_py_int_expr e range), flow, [])
       *     else
       *     if Addr.isinstance obj (Addr.find_builtin "str") then
       *       let base = match kwds with
       *         | [] -> mk_int 10 range
       *         | [Some "base", base] -> base
       *         | _ -> assert false
       *       in
       *       new_int_from_string man ctx arg base range flow
       *     else
       *       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *       oeval_singleton (None, flow, [])
       *
       *   | [arg; base] ->
       *     let obj = object_of_expr arg in
       *     if Addr.isinstance obj (Addr.find_builtin "str") then
       *       new_int_from_string man ctx arg base range flow
       *     else
       *       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *       oeval_singleton (None, flow, [])
       *
       *   | _ ->
       *     let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *     oeval_singleton (None, flow, [])
       * end *)

    (* 𝔼⟦ n | n ∈ ℤ ⟧ *)
    | E_constant (C_int n) ->
       Eval.singleton (mk_py_int n range) flow |> OptionExt.return

    (* 𝔼⟦ int.__op__(e1, e2) | op ∈ {+, -, x, ...} ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
         when is_arithmetic_binop_fun f ->
       Eval.eval_list [e1; e2] man.eval flow |>
         Eval.bind (fun el flow ->
             let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
             Eval.assume
               (mk_py_isinstance_builtin e1 "int" range)
               ~fthen:(fun flow ->
                 (* FIXME: felse correctN *)
                 Eval.assume
                   (mk_py_isinstance_builtin e2 "int" range)
                   ~fthen:(fun flow ->
                     (* FIXME: il faut évaluer les sous expressions
                        dans la zone numérique puis les reconstruire
                        ensuite ? ou le mecanisme des zones va s'en occuper ?*)
                     let op = arithmetic_binop f in
                     let ev1 = match ekind e1 with
                       | E_py_object ({addr_kind = A_py_instance _; addr_uid}, e) -> e
                          (* mk_var (Addr_env.mk_avar ~vtyp:T_int addr_uid ) range *)
                       | _ -> assert false in
                     let ev2 = match ekind e2 with
                       | E_py_object ({addr_kind = A_py_instance _; addr_uid}, e) -> e
                          (* mk_var (Addr_env.mk_avar ~vtyp:T_int addr_uid) range *)
                       | _ -> assert false in
                     man.eval (mk_py_int_expr (mk_binop ev1 op ev2 ~etyp:T_int range) range) flow
                   )
                   ~felse:(fun flow ->
                     man.eval (Addr_env.mk_py_not_implemented range) flow)
                   man flow
               )
               ~felse:(fun flow ->
                 man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                 Eval.empty_singleton)
               man flow
           )
       |> OptionExt.return
       (*       let o1 = object_of_expr e1 in
        *       if not (Addr.isinstance o1 (Addr.find_builtin "int")) then
        *         let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
        *         oeval_singleton (None, flow, [])
        *       else
        *         let e2' = Utils.mk_builtin_call "int" [e2] range in
        *         man.eval ctx e2' flow |>
        *           eval_compose (fun e2 flow ->
        *               let o2 = object_of_expr e2 in
        *               let ev1 = value_of_object o1 and ev2 = value_of_object o2 in
        *               let op = arithmetic_binop f in
        *               oeval_singleton (Some (mk_py_int_expr (mk_binop ev1 op ev2 ~etyp:T_int range) range), flow, [])
        *             )
        *     )
        * |> OptionExt.return *)
    (* 𝔼⟦ int.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
      when is_compare_op_fun f ->
       Debug.fail "todo@\n"
      (* eval_list [e1; e2] (man.eval ctx) flow |>
       * eval_compose (fun el flow ->
       *     let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
       *     let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
       *     if not (Addr.isinstance o1 (Addr.find_builtin "int")) then
       *       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *       oeval_singleton (None, flow, [])
       *     else
       *     if not (Addr.isinstance o2 (Addr.find_builtin "int")) then
       *       oeval_singleton (Some (mk_py_not_implemented range), flow, [])
       *     else
       *       let e1' = Utils.mk_builtin_call "int" [e1] range in
       *       let e2' = Utils.mk_builtin_call "int" [e2] range in
       *       eval_list [e1'; e2'] (man.eval ctx) flow |>
       *       eval_compose (fun el flow ->
       *           let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
       *           let ev1 = value_of_object o1 and ev2 = value_of_object o2 in
       *           let op = compare_op f in
       *           Universal.Utils.assume_to_eval
       *             (mk_binop ev1 op ev2 ~etyp:T_bool range)
       *             (fun true_flow -> oeval_singleton (Some (mk_py_true range), true_flow, []))
       *             (fun false_flow -> oeval_singleton (Some (mk_py_false range), false_flow, []))
       *             ~merge_case:(fun _ _ -> oeval_singleton (Some (mk_py_top T_bool range), flow, []))
       *             man ctx flow ()
       *         )
       *   ) *)

    (* 𝔼⟦ int.__op__(e) | op ∈ {-, +, ~, abs} ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e], [])
         when is_unop_fun f ->
       Debug.fail "todo@\n"
      (* man.eval ctx e flow |>
       * eval_compose (fun e flow ->
       *     let o = object_of_expr e in
       *     if not (Addr.isinstance o (Addr.find_builtin "int")) then
       *       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *       oeval_singleton (None, flow, [])
       *     else
       *       let ev = value_of_object o in
       *       let ev' =
       *         match f with
       *             | "int.__neg__" -> mk_unop O_minus ev ~etyp:T_int range
       *             | "int.__pos__" -> ev
       *             | "int.__abs__" -> Framework.Exceptions.panic_at range "int.__abs__ not supported"
       *             | "int.__invert__" -> mk_unop O_bit_invert ev ~etyp:T_int range
       *             | _ -> assert false
       *       in
       *       oeval_singleton (Some (mk_py_int_expr ev' range), flow, [])
       *   ) *)

    (* 𝔼⟦ int.__op__(e1, e2, ..., en) | n != 2 ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, _, [])
      when is_arithmetic_binop_fun f || is_compare_op_fun f ->
       let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
       Eval.empty_singleton flow |> OptionExt.return

    (* 𝔼⟦ int.__bool__(self) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__bool__")}, _)}, [self], []) ->
       Debug.fail "todo@\n"
    (* man.eval ctx self flow |>
       * eval_compose (fun self flow ->
       *     let o = object_of_expr self in
       *     if not (Addr.isinstance o (Addr.find_builtin "int")) then
       *       let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       *       oeval_singleton (None, flow, [])
       *     else
       *       let ev = value_of_object o in
       *       Universal.Utils.assume_to_eval
       *         (mk_binop ev O_eq (mk_zero range) range)
       *         (fun true_flow -> oeval_singleton (Some (mk_py_false range), flow, []))
       *         (fun false_flow -> oeval_singleton (Some (mk_py_true range), flow, []))
       *         ~merge_case:(fun _ _ -> oeval_singleton (Some (mk_py_top T_bool range), flow, []))
       *         man ctx flow ()
       *   ) *)

    (* 𝔼⟦ int.__bool__(arg1, arg2, ..., argn) | n != 1 ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__bool__")}, _)}, _, []) ->
       Debug.fail "todo@\n"
      (* let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
       * oeval_singleton (None, flow, []) *)

    | _ -> None

  (* and new_int_from_string man ctx arg base range flow =
   *   man.eval ctx base flow |>
   *   eval_compose (fun base flow ->
   *       let obase = object_of_expr base in
   *       match type_of_object obase with
   *       | T_int ->
   *         let s = man.ask ctx (Memory.Query.QString arg) flow |> Option.none_to_exn in
   *         let base = man.ask ctx (Memory.Query.QInt base) flow |> Option.none_to_exn in
   *         debug "base = %a" Memory.Value.I.print base;
   *         let tmp = mktmp ~vtyp:T_int () in
   *         let n = mk_var tmp range in
   *         if Memory.Value.I.is_constant base then
   *           let base, _ = Memory.Value.(I.get_bounds base) in
   *           Memory.Value.(S.fold (fun s acc ->
   *               let s = String.trim s in
   *               try
   *                 if Z.equal base Z.zero then
   *                   let flow = man.exec ctx (mk_assign n (mk_z (Z.of_string s) range) range) flow in
   *                   oeval_singleton (Some n, flow, [mk_remove_var tmp range]) |>
   *                   oeval_join acc
   *                 else if Z.equal base Z.one || Z.lt base Z.zero || Z.gt base (Z.of_int 32)  then
   *                   let flow = man.exec ctx
   *                       (Utils.mk_builtin_raise "ValueError" range)
   *                       flow
   *                   in
   *                   oeval_singleton (None, flow, []) |>
   *                   oeval_join acc
   *                 else if Z.gt base (Z.of_int 16) then
   *                   Framework.Exceptions.panic "base > 16 not supported"
   *                 else
   *                   let s =
   *                     if List.mem (String.sub s 0 2) ["0b"; "0B"; "0o"; "0O"; "0x"; "0X"] then
   *                       String.sub s 2 (String.length s - 2)
   *                     else
   *                       s
   *                   in
   *                   let flow = man.exec ctx (mk_assign n (mk_z (Z.of_string_base (Z.to_int base) s) range) range) flow in
   *                   oeval_singleton (Some n, flow, [mk_remove_var tmp range]) |>
   *                   oeval_join acc
   *               with Invalid_argument _ ->
   *                 let flow = man.exec ctx (Utils.mk_builtin_raise "ValueError" range) flow in
   *                 oeval_singleton (None, flow, []) |>
   *                 oeval_join acc
   *             ) s None)
   *         else
   *           Framework.Exceptions.panic_at range "TODO: fail with a non-constant base in int.__new__"
   *
   *       | _ ->
   *         let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
   *         oeval_singleton (None, flow, [])
   *     )
   *
   *
   * and new_int_from_object man ctx arg range flow =
   *   (\* Check for the presence of __int__ attribute *\)
   *   let obj = object_of_expr arg in
   *   Universal.Utils.assume_to_eval
   *     (Utils.mk_hasattr arg "__int__" range)
   *     (fun true_flow ->
   *        (\* Check the return value of arg.__int__ *\)
   *        man.eval ctx (mk_py_call (mk_py_object_attr (class_of_object obj) "__int__" range) [arg] range) true_flow |>
   *        eval_compose (fun ret flow ->
   *            match type_of_object @@ object_of_expr ret with
   *            | T_int -> oeval_singleton (Some ret, flow, [])
   *            | _ ->
   *              let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
   *              oeval_singleton (None, flow, [])
   *          )
   *     )
   *     (fun false_flow ->
   *        (\* Check for the presence of __trunc__ attribute *\)
   *        Universal.Utils.assume_to_eval
   *          (Utils.mk_hasattr arg "__trunc__" range)
   *          (fun true_flow ->
   *             (\* Check the return value of arg.__trunc__ *\)
   *             man.eval ctx (mk_py_call (mk_py_object_attr (class_of_object obj) "__trunc__" range) [arg] range) true_flow |>
   *             eval_compose (fun ret flow ->
   *                 match type_of_object @@ object_of_expr ret with
   *                 | T_int -> oeval_singleton (Some ret, flow, [])
   *                 | _ ->
   *                   let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
   *                   oeval_singleton (None, flow, [])
   *               )
   *          )
   *          (fun false_flow ->
   *             let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
   *             oeval_singleton (None, flow, [])
   *          )
   *          man ctx flow ()
   *
   *     )
   *     man ctx flow () *)



  and is_arithmetic_binop_fun = function
    | "int.__add__"
    | "int.__radd__"
    | "int.__and__"
    | "int.__rand__"
    | "int.__floordiv__"
    | "int.__rfloordiv__"
    | "int.__lshift__"
    | "int.__rlshift__"
    | "int.__mod__"
    | "int.__rmod__"
    | "int.__mul__"
    | "int.__rmul__"
    | "int.__or__"
    | "int.__ror__"
    | "int.__pow__"
    | "int.__rpow__"
    | "int.__truediv__"
    | "int.__rtruediv__"
    | "int.__sub__"
    | "int.__rsub__"
    | "int.__xor__"
    | "int.__rxor__" -> true
    | _ -> false

  and is_unop_fun = function
    | "int.__neg__"
    | "int.__pos__"
    | "int.__abs__"
    | "int.__invert__" -> true
    | _ -> false

  and is_compare_op_fun = function
    | "int.__eq__"
    | "int.__ne__"
    | "int.__lt__"
    | "int.__le__"
    | "int.__gt__"
    | "int.__ge__" -> true
    | _ -> false

  and arithmetic_binop = function
    | "int.__add__"
    | "int.__radd__" -> O_plus
    | "int.__and__"
    | "int.__rand__" -> O_bit_and
    | "int.__floordiv__"
    | "int.__rfloordiv__" -> O_py_floor_div
    | "int.__lshift__"
    | "int.__rlshift__" -> O_bit_lshift
    | "int.__mod__"
    | "int.__rmod__" -> O_mod
    | "int.__mul__"
    | "int.__rmul__" -> O_mult
    | "int.__or__"
    | "int.__ror__" -> O_bit_or
    | "int.__pow__"
    | "int.__rpow__" -> O_pow
    | "int.__truediv__"
    | "int.__rtruediv__" -> O_div
    | "int.__sub__"
    | "int.__rsub__" -> O_minus
    | "int.__xor__"
    | "int.__rxor__" -> O_bit_xor
    | f -> Framework.Exceptions.panic "arithmetic_op: %s not yet supported" f

  and compare_op = function
    | "int.__eq__" -> O_eq
    | "int.__ne__" -> O_ne
    | "int.__lt__" -> O_lt
    | "int.__le__" -> O_le
    | "int.__gt__" -> O_gt
    | "int.__ge__" -> O_ge
    | _ -> assert false


  let exec _ _ _ _ = None
  let ask _ _ _ = None


end

let () = Framework.Domains.Stateless.register_domain (module Domain)
