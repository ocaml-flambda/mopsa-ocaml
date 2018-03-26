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
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.libs.mopsa"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let is_builtin_function = function
    | "_mopsa_assert_true" -> true
    | _ -> false

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prg man fa = fa

  let exec stmt man ctx flow = None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_function(f) when is_builtin_function f.c_func_var.orgname ->
      debug "builtin function";
      let exp' = mk_expr (E_c_builtin_function f.c_func_var.orgname) ~etyp:T_c_builtin_fn exp.erange in
      Eval.singleton (Some exp', flow, [])

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_true"}, [cond]) ->
      let stmt = mk_assert cond exp.erange in
      let flow = man.exec stmt ctx flow in
      Eval.singleton (Some (mk_int 0 exp.erange), flow, [])

    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain)
