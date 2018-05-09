(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interpreter of expressions containing statements. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.desugar.stmt_rvals"
let debug fmt = Debug.debug ~channel:name fmt


(** Abstract domain. *)
module Domain =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let eval man ctx exp flow =
    match ekind exp with
    | E_c_assign(lval, rval) ->
      man.eval ctx rval flow |>
      eval_compose
        (fun rval flow ->
           let flow = man.exec ctx (Universal.Ast.mk_assign lval rval exp.erange) flow in
           oeval_singleton (Some rval, flow, [])
        )

    | E_c_statement {skind = S_block l} ->
      begin
        match List.rev l with
        | {skind = S_expression e}::q ->
          let q' = List.rev q in
          let stmt' = mk_block q' (tag_range (erange exp) "block'") in
          let flow' = man.exec ctx stmt' flow in
          re_eval_singleton (man.eval ctx) (Some e, flow', [])
        | _ ->
          begin
            Debug.fail "E_c_statement %a" Framework.Pp.pp_expr exp
          end
      end
    | E_c_statement {skind = S_expression e} ->
      re_eval_singleton (man.eval ctx) (Some e, flow, [])
    | _ -> None

  let exec man ctx stmt flow = None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
