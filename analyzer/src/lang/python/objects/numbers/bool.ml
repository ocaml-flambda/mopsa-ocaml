(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python boolean numbers. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.objects.numbers.bool"
let debug fmt = Debug.debug ~channel:name fmt

module Domain= struct

  let rec eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    (* 𝔼⟦ True ⟧ *)
    | E_constant (C_true) ->
      oeval_singleton (Some (mk_py_true range), flow, [])

    (* 𝔼⟦ False ⟧ *)
    | E_constant (C_false) ->
      oeval_singleton (Some (mk_py_false range), flow, [])

    (* 𝔼⟦ bool.__new__(cls, arg) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "bool.__new__")}, _)}, cls :: args, []) ->
      eval_list args (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          match el with
          | [] -> oeval_singleton (Some (mk_py_false range), flow, [])

          | [arg] -> new_bool_of_object man ctx arg range flow

          | _ ->
            let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
            oeval_singleton (None, flow, [])

        )

    | _ -> None

  and new_bool_of_object man ctx arg range flow =
    let obj = object_of_expr arg in
    let cls = class_of_object obj in
    Universal.Utils.assume_to_eval
      (Utils.mk_object_hasattr cls "__bool__" range)
      (fun true_flow ->
         let exp = mk_py_call (mk_py_object_attr cls "__bool__" range) [arg] range in
         man.eval ctx exp flow |>
         eval_compose (fun ret flow ->
             let o = object_of_expr ret in
             if Addr.isinstance o (Addr.find_builtin "bool") then
               oeval_singleton (Some exp, flow, [])
             else
               let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
               oeval_singleton (None, flow, [])
           )
      )
      (fun false_flow ->
         Universal.Utils.assume_to_eval
           (Utils.mk_object_hasattr cls "__len__" range)
           (fun true_flow ->
              Universal.Utils.assume_to_eval
                (mk_binop (mk_py_call (mk_py_object_attr cls "__len__" range) [arg] range) O_eq (mk_py_zero range) range)
                (fun true_flow -> oeval_singleton (Some (mk_py_false range), true_flow, []))
                (fun false_flow -> oeval_singleton (Some (mk_py_true range), false_flow, []))
                man ctx true_flow ()
           )
           (fun false_flow ->
              oeval_singleton (Some (mk_py_true range), false_flow, [])
           )
           man ctx false_flow ()
      )
      man ctx flow ()


  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
