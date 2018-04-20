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

let name = "universal.numeric.domains.boxes.congruence"

module Domain =
struct
  module Value = Values.Congruence

  include Nonrel.Domain.Make(Value)

  let debug fmt = Debug.debug ~channel:name fmt

  let print fmt a =
    Format.fprintf fmt "congruence: @[%a@]@\n" print a

  let init man ctx prog flow =
    ctx, set_domain_cur top man flow

  let exec man ctx stmt flow =
    match skind stmt with
    | S_assign({ekind = E_var var}, e, STRONG) ->
      debug "assign";
      man.eval ctx e flow |>
      eval_to_orexec
        (fun e flow ->
           let a = get_domain_cur man flow in
           let v = eval_value a e in
           let a' = VarMap.add var v a in
           let flow' = set_domain_cur a' man flow in
           if Value.is_minf_inf v || Value.is_bottom v then return flow'
           else
             let vars = Framework.Visitor.expr_vars e in
             match vars with
             | [] -> return flow'
             | _ ->
               let c = bot_to_exn v in
               debug "publish CIntCongruence %a" Value.print v;
               Some {
                 out = flow'; mergers = [];
                 publish = [Reduction.CIntCongruence(var, c)];
               }
        )
        (man.exec ctx) man.flow

    | _ -> exec man ctx stmt flow

  let refine man ctx channel flow =
    match channel with
    | Reduction.CIntConstant(var, n) ->
      debug "refine with CIntConstant(%a, %a)" Framework.Pp.pp_var var Z.pp_print n;
      let a = get_domain_cur man flow in
      let v = find var a in
      bot_dfl1 None (fun c ->
          if Values.Congruence.C.is_singleton c then None
          else
            let c' = Values.Congruence.of_constant (C_int n) in
            debug "applying refinement";
            let a' = VarMap.add var c' a in
            let flow' = set_domain_cur a' man flow in
            return flow'
        ) v

    | _ -> None



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
