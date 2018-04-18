(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** MOPSA Python library. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Eval
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.libs.mopsa"
let debug fmt = Debug.debug ~channel:name fmt



(*==========================================================================*)
(**                               {2 Domain }                               *)
(*==========================================================================*)


module Domain =
struct


  (*==========================================================================*)
  (**                       {2 Transfer functions }                           *)
  (*==========================================================================*)


  let exec man ctx stmt flow = None

  let init _ ctx _ flow = ctx, flow

  let check man ctx cond range flow =
    let flow = man.exec ctx (mk_stmt (Universal.Ast.S_assert cond) range) flow in
    oeval_singleton (Some (mk_py_none range), flow, [])

  let eval man ctx exp flow =
    match ekind exp with
    (* Calls to _mopsa_assert_equal function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin f)}},
        [x; y], []
      ) when f = "_mopsa_assert_equal_" || f = "mopsa.assert_equal" ->
      let range = erange exp in
      check man ctx (mk_binop x O_eq y (tag_range range "eq")) range flow

    (* Calls to _mopsa_assert_true function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin f)}},
        [x], []
      ) when f = "_mopsa_assert_true_" || f = "mopsa.assert_true" ->
      let range = erange exp in
      check man ctx x range  flow

    (* Calls to _mopsa_assert_false function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin f)}},
        [x], []
      ) when f = "_mopsa_assert_false_" || f = "mopsa.assert_false" ->
      let range = erange exp in
      check man ctx (mk_not x (tag_range range "not")) range flow

    | _ ->
      None

  let ask _ _ _ _ = None

end




(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let setup () =
  Stateless.register_domain name (module Domain)
