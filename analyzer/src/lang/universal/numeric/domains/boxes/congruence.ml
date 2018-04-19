(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational congruence abstract domain of integer variables. *)

open Framework.Query
open Framework.Ast
open Framework.Manager
open Framework.Domains.Reduction.Domain
open Framework.Eval
open Framework.Exec
open Ast
open Bot

let name = "universal.numeric.congruence"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct
  module Value = Values.Congruence

  include Nonrel.Domain.Make(Value)

  let print fmt a =
    Format.fprintf fmt "congruence: @[%a@]@\n" print a

  let init man ctx prog flow =
    ctx, set_domain_cur top man flow

  let exec man ctx stmt flow =
    match skind stmt with
    | S_assign({ekind = E_var var}, e, STRONG) ->
      man.eval ctx e flow |>
      eval_to_orexec
        (fun e flow ->
           debug "assign %a to %a" Framework.Pp.pp_expr e Framework.Pp.pp_var var;
           let a = get_domain_cur man flow in
           let v = eval_value a e in
           debug "value = %a" Value.print v;
           let flow' = set_domain_cur (VarMap.add var v a) man flow in
           if Value.is_minf_inf v then
             return flow'
           else
             try
               let c = bot_to_exn v in
               Some {
                 out = flow'; mergers = []; subscribe = (fun _ _ -> None);
                 publish = [Reduction.CIntCongruence(var, c)];
               }
             with Found_BOT ->
               return flow'
        )
        (man.exec ctx) man.flow

    | _ -> exec man ctx stmt flow


  let eval man ctx exp flow =
    match ekind exp with
    | E_binop((O_plus | O_minus | O_mult | O_div | O_mod |
               O_eq | O_ne | O_lt | O_le | O_gt | O_ge | O_bit_rshift | O_bit_lshift  as op), e1, e2) ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose
        (fun el flow ->
           let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
           let exp' = {exp with ekind = E_binop(op, e1, e2)} in
           oeval_singleton (Some (exp', []), flow, [])
        )

    | E_unop((O_minus | O_plus | O_log_not as op), e) ->
      man.eval ctx e flow |>
      eval_compose
        (fun e flow ->
           let exp' = {exp with ekind = E_unop(op, e)} in
           oeval_singleton (Some (exp', []), flow, [])
        )

    | _ -> None



end

let setup () =
  register_domain name (module Domain)
