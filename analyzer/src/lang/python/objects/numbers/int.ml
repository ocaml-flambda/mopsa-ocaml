(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python integer numbers. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.objects.numbers.int"
let debug fmt = Debug.debug ~channel:name fmt

module Domain= struct

  let rec eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    (* 𝔼⟦ int.__new__(cls, arg) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__new__")}, _)}, cls :: args, []) ->
      begin match args with
        | [] -> oeval_singleton (Some (mk_py_zero range), flow, [])
        | [arg] ->
          let obj = object_of_expr arg in
          begin match type_of_object obj with
            | T_int -> oeval_singleton (Some arg, flow, [])
            | _ ->
              (* Check for the presence of __int__ attribute *)
              Universal.Utils.assume_to_eval
                (Utils.mk_hasattr arg "__int__" range)
                (fun true_flow ->
                   (* Check the return value of arg.__int__ *)
                   man.eval ctx (mk_py_call (mk_py_object_attr (class_of_object obj) "__int__" range) [arg] range) true_flow |>
                   eval_compose (fun ret flow ->
                       match type_of_object @@ object_of_expr ret with
                       | T_int -> oeval_singleton (Some ret, flow, [])
                       | _ ->
                         let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                         oeval_singleton (None, flow, [])
                     )
                )
                (fun false_flow ->
                   (* Check for the presence of __trunc__ attribute *)
                   Universal.Utils.assume_to_eval
                     (Utils.mk_hasattr arg "__trunc__" range)
                     (fun true_flow ->
                        (* Check the return value of arg.__trunc__ *)
                        man.eval ctx (mk_py_call (mk_py_object_attr (class_of_object obj) "__trunc__" range) [arg] range) true_flow |>
                        eval_compose (fun ret flow ->
                            match type_of_object @@ object_of_expr ret with
                            | T_int -> oeval_singleton (Some ret, flow, [])
                            | _ ->
                              let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                              oeval_singleton (None, flow, [])
                          )
                     )
                     (fun false_flow ->
                        let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                        oeval_singleton (None, flow, [])
                     )
                     man ctx flow ()

                )
                man ctx flow ()
          end
        | _ ->
          let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
          oeval_singleton (None, flow, [])
      end

    (* 𝔼⟦ n | n ∈ ℤ ⟧ *)
    | E_constant (C_int n) ->
      oeval_singleton (Some (mk_py_z n range), flow, [])

    (* 𝔼⟦ int.__op__(e1, e2) | op ∈ {+, -, x, ...} ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
      when is_arithmetic_op_fun f ->
      let e1' = Utils.mk_builtin_call "int" [e1] range in
      let e2' = Utils.mk_builtin_call "int" [e2] range in
      eval_list [e1'; e2'] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
          let ev1 = value_of_object o1 and ev2 = value_of_object o2 in
          let op = arithmetic_op f in
          oeval_singleton (Some (mk_py_int_expr (mk_binop ev1 op ev2 ~etyp:T_int range) range), flow, [])
        )

    (* 𝔼⟦ int.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
      when is_compare_op_fun f ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
          if not (Addr.issubclass o1 (Addr.find_builtin "int")) then
            let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
            oeval_singleton (None, flow, [])
          else
          if not (Addr.issubclass o2 (Addr.find_builtin "int")) then
            oeval_singleton (Some (mk_py_not_implemented range), flow, [])
          else
            let e1' = Utils.mk_builtin_call "int" [e1] range in
            let e2' = Utils.mk_builtin_call "int" [e2] range in
            eval_list [e1'; e2'] (man.eval ctx) flow |>
            eval_compose (fun el flow ->
                let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
                let ev1 = value_of_object o1 and ev2 = value_of_object o2 in
                let op = compare_op f in
                Universal.Utils.assume_to_eval
                  (mk_binop ev1 op ev2 ~etyp:T_bool range)
                  (fun true_flow -> oeval_singleton (Some (mk_py_bool true range), true_flow, []))
                  (fun false_flow -> oeval_singleton (Some (mk_py_bool false range), false_flow, []))
                  man ctx flow ()
              )
        )

    (* 𝔼⟦ int.__op__(e1, e2, ..., en) | n != 2 ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, _, [])
      when is_arithmetic_op_fun f || is_compare_op_fun f ->
      let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
      oeval_singleton (None, flow, [])

    (* 𝔼⟦ int.__bool__(self) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__bool__")}, _)}, [self], []) ->
      man.eval ctx self flow |>
      eval_compose (fun self flow ->
          let o = object_of_expr self in
          if not (Addr.isinstance o (Addr.find_builtin "int")) then
            let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
            oeval_singleton (None, flow, [])
          else
            Universal.Utils.assume_to_eval
              (mk_binop self O_eq (mk_py_int 0 range) range)
              (fun true_flow -> oeval_singleton (Some (mk_py_bool false range), flow, []))
              (fun false_flow -> oeval_singleton (Some (mk_py_bool true range), flow, []))
        (* ~merge_case:(fun _ _ -> oeval_singleton (Some (mk_py_top T_bool range), flow, [])) *)
              man ctx flow ()
        )

    (* 𝔼⟦ int.__bool__(arg1, arg2, ..., argn) | n != 1 ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__bool__")}, _)}, _, []) ->
      let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
      oeval_singleton (None, flow, [])
  
    | _ -> None


  and is_arithmetic_op_fun = function
    | "int.__add__"
    | "int.__sub__"
    | "int.__mul__" -> true
    | _ -> false

  and is_compare_op_fun = function
    | "int.__eq__"
    | "int.__ne__"
    | "int.__lt__"
    | "int.__le__"
    | "int.__gt__"
    | "int.__ge__" -> true
    | _ -> false

  and arithmetic_op = function
    | "int.__add__" -> math_plus
    | "int.__sub__" -> math_minus
    | "int.__mul__" -> math_mult
    | f -> Framework.Exceptions.panic "arithmetic_op: %s not yet supported" f

  and compare_op = function
    | "int.__eq__" -> O_eq
    | "int.__ne__" -> O_ne
    | "int.__lt__" -> O_lt
    | "int.__le__" -> O_le
    | "int.__gt__" -> O_gt
    | "int.__ge__" -> O_ge
    | _ -> assert false


  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
