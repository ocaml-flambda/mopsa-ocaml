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
open Framework.Ast
open Ast

let name = "c.flows.interproc"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prg man fa = fa

  let exec stmt man ctx flow = None

  let is_builtin_function = function
    | "_mopsa_assert_true" -> true
    | _ -> false

  let eval_builtin_function fundec args range man ctx flow =
    let f = fundec.c_func_var.Universal.Ast.orgname in
    let open Universal.Ast in
    match f, args with
    | "_mopsa_assert_true", [cond] ->
      let stmt = mk_assert cond range in
      let flow = man.exec stmt ctx flow in
      Eval.singleton (Some (mk_int 0 range), flow, [])
        
    | _ -> assert false
  
  let eval exp man ctx flow =
    match ekind exp with
    | E_c_call(f, args) ->
      Eval.compose_eval
        f
        (fun f flow ->
           match ekind f with
           | E_c_function(fundec) when is_builtin_function fundec.c_func_var.Universal.Ast.orgname ->
             eval_builtin_function fundec args exp.erange man ctx flow

           | E_c_function(fundec) ->
             let open Universal.Ast in
             let fundec' = {
               fun_name = fundec.c_func_var.unname;
               fun_parameters = fundec.c_func_parameters;
               fun_locvars = List.map fst fundec.c_func_local_vars;
               fun_body = fundec.c_func_body;
             } in
             let exp' = mk_call fundec' args exp.erange in
             Eval.re_eval_singleton man ctx (Some exp', flow, [])

           | _ ->
             assert false
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow
        
    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  register_domain name (module Domain)
