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

    | _ -> None


  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
