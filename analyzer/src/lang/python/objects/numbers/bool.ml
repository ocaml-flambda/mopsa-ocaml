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
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__bool__")}, _)}, cls :: args, []) ->
      assert false

    | _ -> None


  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
