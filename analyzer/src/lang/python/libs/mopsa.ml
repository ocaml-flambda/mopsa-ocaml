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
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.libs.mopsa"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                        {2 Assertions tokens }                           *)
(*==========================================================================*)

type token +=
  | TSafeAssert of range
  | TFailAssert of range
  | TMayAssert of range

let () =
  register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TSafeAssert r1, TSafeAssert r2
      | TFailAssert r1, TFailAssert r2
      | TMayAssert r1, TMayAssert r2 ->
        compare_range r1 r2
      | _ -> next tk1 tk2
    );
  register_pp_token (fun next fmt -> function
      | TSafeAssert r -> Format.fprintf fmt "safe@%a" Framework.Pp.pp_range r
      | TFailAssert r -> Format.fprintf fmt "fail@%a" Framework.Pp.pp_range r
      | TMayAssert r -> Format.fprintf fmt "may@%a" Framework.Pp.pp_range r
      | tk -> next fmt tk
    )

(*==========================================================================*)
(**                               {2 Domain }                               *)
(*==========================================================================*)


module Domain =
struct


  (*==========================================================================*)
  (**                       {2 Transfer functions }                           *)
  (*==========================================================================*)


  let exec stmt manager ctx flow = None

  let init _ _ flow = flow

  let check cond range man ctx flow =
    Universal.Utils.cond_flow
      cond
      (fun safe_flow ->
         let flow = man.flow.add (TSafeAssert range) (man.flow.get TCur safe_flow) safe_flow in
         Eval.singleton (Some (mk_py_none range), flow, [])
      )
      (fun fail_flow ->
         let flow = man.flow.add (TFailAssert range) (man.flow.get TCur fail_flow) fail_flow |>
                    man.flow.set TCur man.env.bottom
         in
         Eval.singleton (Some (mk_py_none range), flow, [])
      )
      (fun () -> Eval.singleton (None, flow, []))
      (fun safe_flow fail_flow ->
         let flow = man.flow.join safe_flow fail_flow in
         let flow = man.flow.add (TMayAssert range) (man.flow.get TCur flow) flow |>
                    man.flow.set TCur (man.flow.get TCur safe_flow)
         in
         Eval.singleton (Some (mk_py_none range), flow, [])
      )
      man ctx flow

  
  let eval exp man ctx flow =
    match ekind exp with
    (* Calls to _mopsa_assert_equal function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin f)}},
        [x; y], []
      ) when f = "_mopsa_assert_equal_" || f = "mopsa.assert_equal" ->
      let range = erange exp in
      check (mk_binop x O_eq y (tag_range range "eq")) range man ctx flow

    (* Calls to _mopsa_assert_true function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin f)}},
        [x], []
      ) when f = "_mopsa_assert_true_" || f = "mopsa.assert_true" ->
      let range = erange exp in
      check x range man ctx flow

    (* Calls to _mopsa_assert_false function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin f)}},
        [x], []
      ) when f = "_mopsa_assert_false_" || f = "mopsa.assert_false" ->
      let range = erange exp in
      check (mk_not x (tag_range range "not")) range man ctx flow
  
    | _ ->
      None

  let ask _ _ _ _ = None

end




(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let setup () =
  Stateless.register_domain name (module Domain)
