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
open Framework.Utils
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
      let () = debug "%a : %a" Framework.Pp.pp_typ f.etyp Framework.Pp.pp_expr f in
      man.eval ctx f flow |>
      eval_compose
        (fun f flow ->
           let () = debug "inside : %a" Framework.Pp.pp_expr f in
           match ekind f with
           | E_c_builtin_function(name) ->
             let () = debug "builtin : %a" Framework.Pp.pp_expr f in
             let exp' = {exp with ekind = E_c_call(f, args)} in
             re_eval_singleton (man.eval ctx) (Some exp', flow, [])

           | E_c_function fundec ->
             let body = get_c_fun_body_panic fundec in
             debug "call to %a, body @[%a@]" Framework.Pp.pp_var fundec.c_func_var Framework.Pp.pp_stmt body;
             let open Universal.Ast in
             let fundec' = {
               fun_name = var_uniq_name (fundec.c_func_var);
               fun_parameters = fundec.c_func_parameters;
               fun_locvars = List.map fst fundec.c_func_local_vars;
               fun_body = {skind = S_c_goto_stab (body); srange = srange body};
               fun_return_type = fundec.c_func_return;
             } in
             let exp' = mk_call fundec' args exp.erange in
             re_eval_singleton (man.eval ctx) (Some exp', flow, [])
           | E_var pf when pf.vtyp |> is_c_pointer_type ->
             let f = {f with ekind = E_c_deref f} in
             re_eval_singleton (man.eval ctx) (Some {exp with ekind = E_c_call(f,args)}, flow, [])

           | _ -> Debug.fail "Call to %a not supported" Framework.Pp.pp_expr f
        )

    | E_c_cast(e, _) when (exp |> etyp |> is_c_pointer_type) && (exp |> etyp |> under_pointer_type |> is_c_function_type) ->
      let t' = exp |> etyp |> under_pointer_type in
      debug "t' = %a, e = %a" Framework.Pp.pp_typ t' Framework.Pp.pp_expr e;
      re_eval_singleton (man.eval ctx) (Some ({e with etyp = t'}), flow, [])

    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  register_domain name (module Domain)
