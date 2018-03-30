(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of C intra-procedural control flow *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Stateless
open Framework.Utils
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.libs.mopsa"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let is_builtin_function = function
    | "_mopsa_assert_true"
    | "_mopsa_assert_false"
    | "_mopsa_assert_safe"
    | "_mopsa_assert_unsafe" -> true
    | _ -> false

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prg man fa = fa

  let exec stmt man ctx flow = None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_function(f) when is_builtin_function f.c_func_var.vname ->
      debug "builtin function";
      let exp' = mk_expr (E_c_builtin_function f.c_func_var.vname) ~etyp:T_c_builtin_fn exp.erange in
      oeval_singleton (Some exp', flow, [])

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_true"}, [cond]) ->
      let stmt = mk_assert cond exp.erange in
      let flow = man.exec stmt ctx flow in
      oeval_singleton (Some (mk_int 0 exp.erange), flow, [])

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_false"}, [cond]) ->
      let stmt = mk_assert (mk_not cond exp.erange) exp.erange in
      let flow = man.exec stmt ctx flow in
      oeval_singleton (Some (mk_int 0 exp.erange), flow, [])

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_safe"}, [e]) ->
      man.eval e ctx flow |>
      eval_compose
        (fun e flow ->
           let stmt = mk_assert (mk_one exp.erange) exp.erange in
           let flow = man.exec stmt ctx flow in
           oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
        )
        ~empty:(fun flow ->
           let stmt = mk_assert (mk_zero exp.erange) exp.erange in
           let flow = man.exec stmt ctx flow in
           oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
        )
        
    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_unsafe"}, [e]) ->
      man.eval e ctx flow |>
      eval_compose
        (fun e flow ->
           let stmt = mk_assert (mk_zero exp.erange) exp.erange in
           let flow = man.exec stmt ctx flow in
           oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
        )
        ~empty:(fun flow ->
           let stmt = mk_assert (mk_one exp.erange) exp.erange in
           let flow = man.exec stmt ctx flow in
           oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
        )

    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain)
