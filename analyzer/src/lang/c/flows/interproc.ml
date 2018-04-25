(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of C function calls *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Stateless
open Framework.Eval
open Framework.Ast
open Ast

let name = "c.flows.interproc"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let exec man ctx stmt flow = None

  let eval man ctx exp flow =
    match ekind exp with
    | E_c_call(f, args) ->
      man.eval ctx f flow |>
      eval_compose
        (fun f flow ->
           match ekind f with
           | E_c_builtin_function(name) ->
             let exp' = {exp with ekind = E_c_call(f, args)} in
             re_eval_singleton (man.eval ctx) (Some exp', flow, [])

           | E_c_function(fundec) ->
             debug "call to %a, body @[%a@]" Framework.Pp.pp_var fundec.c_func_var Framework.Pp.pp_stmt fundec.c_func_body;
             let open Universal.Ast in
             let fundec' = {
               fun_name = var_uniq_name (fundec.c_func_var);
               fun_parameters = fundec.c_func_parameters;
               fun_locvars = List.map fst fundec.c_func_local_vars;
               fun_body = {skind = S_c_goto_stab (fundec.c_func_body); srange = srange fundec.c_func_body};
               fun_return_type = fundec.c_func_return;
             } in
             let exp' = mk_call fundec' args exp.erange in
             re_eval_singleton (man.eval ctx) (Some exp', flow, [])

           | _ ->
             assert false
        )

    | E_c_function(f) ->
      oeval_singleton (Some exp, flow, [])

    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  register_domain name (module Domain)
