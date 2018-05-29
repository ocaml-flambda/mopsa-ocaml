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
open Framework.Eval
open Universal.Ast
open Framework.Domains.Stateless
open Universal.Ast
open Ast
open Addr

let name = "python.data_model.callable"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let init _ ctx _ flow = ctx, flow

  let exec man ctx stmt flow = None

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_addr _}, _, _) ->
      (* Calls to addresses should be captured by other domains. If we
         are here, then we are missing an implementation of the
         function *)
      Framework.Exceptions.panic "call %a can not be resolved" pp_expr exp

    | E_py_call(f, args, []) ->
      debug "Calling %a from %a" pp_expr exp pp_range_verbose exp.erange;
      man.eval ctx f flow |>
      eval_compose
        (fun f flow ->
           match ekind f with
           (* Calls on non-object variables and constants is not allowed *)
           | E_var _ | E_constant _ ->
             let stmt = Utils.mk_builtin_raise "TypeError" range in
             let flow = man.exec ctx stmt flow in
             oeval_singleton (None, flow, [])

           (* Calls on instances is OK if __call__ is defined *)
           | E_addr {addr_kind = A_py_instance(cls, None)} ->
             assert false

           (* Calls on other kinds of addresses is handled by other domains *)
           | E_addr _ ->
             eval_list args (man.eval ctx) flow |>
             eval_compose (fun args flow ->
                 let exp = {exp with ekind = E_py_call(f, args, [])} in
                 re_eval_singleton (man.eval ctx) (Some exp, flow, [])
               )

           | _ -> assert false
        )

    | E_py_call(f, args, _) ->
      Framework.Exceptions.panic_at range "calls with keyword arguments not supported"

    | _ -> None


  let ask _ _ _ _ = None



end

let setup () =
  register_domain name (module Domain)
