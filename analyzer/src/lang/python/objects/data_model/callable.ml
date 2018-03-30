(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for callables. *)

open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Utils
open Universal.Ast
open Framework.Domains.Stateless
open Universal.Ast
open Ast
open Addr

let name = "python.objects.data_model.callable"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let init _ _ flow = flow

  let exec stmt man ctx flow = None

  let eval exp manager ctx flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call(f, args, []) ->
      debug "Calling %a from %a" pp_expr exp pp_range_verbose exp.erange;
      manager.eval f ctx flow |>
      eval_compose
        (fun f flow ->
           match ekind f with
           (* Calls on non-object variables and constants is not allowed *)
           | E_var _ | E_constant _ ->
             let stmt = Builtins.mk_builtin_raise "TypeError" (tag_range range "call") in
             let flow = manager.exec stmt ctx flow in
             oeval_singleton (None, flow, [])

           (* Calls on instances is OK if __call__ is defined *)
           | E_addr {addr_kind = A_py_instance(cls, None)} ->
             assert false

           (* Calls on other kinds of addresses is handled by other domains *)
           | E_addr _ ->
             let exp = {exp with ekind = E_py_call(f, args, [])} in
             re_eval_singleton (Some exp, flow, []) manager ctx

           | _ -> assert false
        )

    | _ -> None


  let ask _ _ _ _ = None



end

let setup () =
  register_domain name (module Domain)
