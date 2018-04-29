(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational abstract domain of integer variables. *)

open Framework.Query
open Framework.Ast
open Framework.Manager
open Framework.Domains.Reduce.Domain
open Framework.Flow
open Framework.Eval
open Ast
open Bot

let name = "universal.numeric.domains.boxes.int"

module Domain =
struct
  module Value = Values.Int

  include Nonrel.Domain.Make(Value)

  let debug fmt = Debug.debug ~channel:name fmt

  let print fmt a =
    Format.fprintf fmt "int: @[%a@]@\n" print a

  let init man ctx prog flow =
    ctx, set_domain_cur top man flow

  let exec man ctx stmt flow =
    match skind stmt with
    | S_assume(e) ->
      man.eval ctx e flow |>
      eval_to_orexec
        (fun e flow ->
           let a = get_domain_cur man flow in
           let (_,r) as t = annotate_expr a e in
           let rr = Value.assume_true r in
           if Value.is_bottom rr then return_flow (set_domain_cur bottom man flow)
           else
             let a' = refine_expr a t rr in
             let vars = Framework.Visitor.expr_vars e in
             debug "trying constant reduction in %a" print a;
             let publish = List.fold_left (fun acc var ->
                 let v = find var a in
                 let v' = find var a' in
                 if not (Value.is_constant v) && Value.is_constant v' then
                   let n, _ = Value.get_bounds v' in
                   debug "publish new constant reduction";
                   Reduction.CIntConstant(var, n) :: acc
                 else
                   acc
               ) [] vars
             in
             let flow' = set_domain_cur a' man flow in
             Some {out = flow'; publish; mergers = []}
        )
        (man.exec ctx) man.flow

    | _ -> exec man ctx stmt flow

  let refine man ctx channel flow =
    match channel with
    | Reduction.CIntCongruence(var, c) ->
      debug "refine with CIntCongruence %a" Values.Congruence.C.fprint c;
      let a = get_domain_cur man flow in
      let v = find var a in
      bot_dfl1 None (fun itv ->
          debug "trying congruence -> interval reduction on itv = %a with c = %a" Values.Int.I.fprint itv Values.Congruence.C.fprint c;
          let r = Congruences.IntCong.meet_inter c itv in
          try
            let (c', itv') = bot_to_exn r in
            debug "checking precision";
            if Intervals.IntItv.included itv itv' then let _ = debug "none" in None
            else
              let a' = VarMap.add var (Bot.Nb itv') a in
              debug "reduction applied, itv' = %a" Values.Int.I.fprint itv';
              let flow' = set_domain_cur a' man flow in
              if Value.I.is_singleton itv' then
                let n = match itv' with Value.I.B.Finite n, _ -> n | _ -> assert false in
                debug "publish new constant reduction";
                Some {
                  out = flow'; mergers = []; publish = [Reduction.CIntConstant(var, n)]
                }
              else
                return_flow flow'
          with Found_BOT ->
            return_flow (set_domain_cur bottom man flow)
        ) v

    | _ -> None



  let eval man ctx exp flow =
    match ekind exp with
    | E_binop((O_plus | O_minus | O_mult | O_div | O_mod |
               O_eq | O_ne | O_lt | O_le | O_gt | O_ge |
               O_log_and | O_log_or | O_bit_and | O_bit_or |
               O_bit_xor | O_bit_lshift | O_bit_rshift as op), e1, e2) ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose
        (fun el flow ->
           let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
           let exp' = {exp with ekind = E_binop(op, e1, e2)} in
           oeval_singleton (Some (exp', []), flow, [])
        )

    | E_unop((O_minus | O_plus | O_log_not | O_bit_invert | O_sqrt as op), e) ->
      man.eval ctx e flow |>
      eval_compose
        (fun e flow ->
           let exp' = {exp with ekind = E_unop(op, e)} in
           oeval_singleton (Some (exp', []), flow, [])
        )

    | _ -> None


  let ask : type r. ('a, t) manager -> Framework.Context.context -> r Framework.Query.query -> 'a flow -> r option =
    fun man ctx query flow ->
      match query with
      | Query.QIntList e ->
        let a = get_domain_cur man flow in
        let v = eval_value a e in
        bot_dfl1 None (fun itv ->
            if Value.I.is_bounded itv then
              Some (Value.I.to_list itv)
            else
              None
          ) v

      | _ ->
        None


end

let setup () =
  register_domain name (module Domain)
