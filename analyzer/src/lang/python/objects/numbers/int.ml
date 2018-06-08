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
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__new__")}, _)}, cls :: args, kwds) ->
      begin match args with
        | [] -> oeval_singleton (Some (mk_py_zero range), flow, [])

        | [arg] ->
          let obj = object_of_expr arg in
          begin match type_of_object obj, kwds with
            | T_int, [] -> oeval_singleton (Some arg, flow, [])
            | T_string, [] -> new_int_from_string man ctx arg (mk_int 10 range) range flow
            | T_string, [Some "base", base] -> new_int_from_string man ctx arg base range flow
            | _, [] -> new_int_from_object man ctx arg range flow
            | _ -> assert false
          end

        | [arg; base] ->
          let obj = object_of_expr arg in
          begin match type_of_object obj with
            | T_string -> new_int_from_string man ctx arg base range flow
            | _ ->
              let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
              oeval_singleton (None, flow, [])

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
                  (fun true_flow -> oeval_singleton (Some (mk_py_true range), true_flow, []))
                  (fun false_flow -> oeval_singleton (Some (mk_py_false range), false_flow, []))
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
              (fun true_flow -> oeval_singleton (Some (mk_py_false range), flow, []))
              (fun false_flow -> oeval_singleton (Some (mk_py_true range), flow, []))
        (* ~merge_case:(fun _ _ -> oeval_singleton (Some (mk_py_top T_bool range), flow, [])) *)
              man ctx flow ()
        )

    (* 𝔼⟦ int.__bool__(arg1, arg2, ..., argn) | n != 1 ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__bool__")}, _)}, _, []) ->
      let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
      oeval_singleton (None, flow, [])

    | _ -> None

  and new_int_from_string man ctx arg base range flow =
    man.eval ctx base flow |>
    eval_compose (fun base flow ->
        let obase = object_of_expr base in
        match type_of_object obase with
        | T_int ->
          let s = man.ask ctx (Memory.Query.QString arg) flow |> Option.none_to_exn in
          let base = man.ask ctx (Memory.Query.QInt base) flow |> Option.none_to_exn in
          debug "base = %a" Memory.Value.I.print base;
          let tmp = mktmp ~vtyp:T_int () in
          let n = mk_var tmp range in
          if Memory.Value.I.is_constant base then
            let base, _ = Memory.Value.(I.get_bounds base) in
            Memory.Value.(S.fold (fun s acc ->
                let s = String.trim s in
                try
                  if Z.equal base Z.zero then
                    let flow = man.exec ctx (mk_assign n (mk_z (Z.of_string s) range) range) flow in
                    oeval_singleton (Some n, flow, [mk_remove_var tmp range]) |>
                    oeval_join acc
                  else if Z.equal base Z.one || Z.lt base Z.zero || Z.gt base (Z.of_int 32)  then
                    let flow = man.exec ctx
                        (Utils.mk_builtin_raise "ValueError" range)
                        flow
                    in
                    oeval_singleton (None, flow, []) |>
                    oeval_join acc
                  else if Z.gt base (Z.of_int 16) then
                    Framework.Exceptions.panic "base > 16 not supported"
                  else
                    let s =
                      if List.mem (String.sub s 0 2) ["0b"; "0B"; "0o"; "0O"; "0x"; "0X"] then
                        String.sub s 2 (String.length s - 2)
                      else
                        s
                    in
                    let flow = man.exec ctx (mk_assign n (mk_z (Z.of_string_base (Z.to_int base) s) range) range) flow in
                    oeval_singleton (Some n, flow, [mk_remove_var tmp range]) |>
                    oeval_join acc
                with Invalid_argument _ ->
                  let flow = man.exec ctx (Utils.mk_builtin_raise "ValueError" range) flow in
                  oeval_singleton (None, flow, []) |>
                  oeval_join acc
              ) s None)
          else
            Framework.Exceptions.panic_at range "TODO: fail with a non-constant base in int.__new__"

        | _ ->
          let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
          oeval_singleton (None, flow, [])
      )


  and new_int_from_object man ctx arg range flow =
    (* Check for the presence of __int__ attribute *)
    let obj = object_of_expr arg in
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
