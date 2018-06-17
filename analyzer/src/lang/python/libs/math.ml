(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Math Python library. *)

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

let name = "python.libs.math"
let debug fmt = Debug.debug ~channel:name fmt


let check man ctx cond range flow =
  let flow = man.exec ctx (mk_stmt (Universal.Ast.S_assert cond) range) flow in
  oeval_singleton (Some (mk_py_none range), flow, [])


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

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "math.sqrt")}, _)}, [e], []) ->
      let exp' = mk_unop O_sqrt e ~etyp:T_float range in
      oeval_singleton (Some exp', flow, [])

    | _ ->
      None

  let ask _ _ _ _ = None

end




(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let setup () =
  Stateless.register_domain name (module Domain)
