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
open Framework.Domains.Reduction.Domain
open Framework.Flow
open Framework.Eval
open Ast
open Bot

let name = "universal.numeric.integers"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct
  module Value = Values.Int
                   
  include Nonrel.Domain.Make(Value)

  let print fmt a =
    Format.fprintf fmt "int: @[%a@]@\n" print a

  let init man ctx prog flow =
    ctx, set_domain_cur top man flow

  let exec man ctx stmt flow =
    match skind stmt with
    | S_assign({ekind = E_var var}, e, STRONG) ->
      man.eval ctx e flow |>
      eval_to_orexec
        (fun e flow ->
           let a = get_domain_cur man flow in
           let v = eval_value a e in
           let a' = VarMap.add var v a in
           let flow' = set_domain_cur a' man flow in
           try
             let itv = Bot.bot_to_exn v in
             if Intervals.IntItv.is_singleton itv then
               return flow'
             else
               Some {
                 out = flow'; mergers = []; publish = [];
                 subscribe = (fun ch flow ->
                   match ch with
                     | Reduction.CIntCongruence(var', c) when compare_var var var' = 0 ->
                       begin
                         let r = Congruences.IntCong.meet_inter c itv in
                         try
                           let c', itv' = Bot.bot_to_exn r in
                           debug "congruence reduction: %a, %a" Values.Congruence.print (Bot.Nb c') Values.Int.print (Bot.Nb itv');
                           if Intervals.IntItv.included itv itv' then
                             return flow
                           else
                             let a'' = VarMap.add var (Bot.Nb itv') a' in
                             let flow' = set_domain_cur a'' man flow in
                              Some {
                                out = flow'; mergers = []; subscribe = (fun _ flow -> return flow);
                                publish = [Reduction.CIntCongruence(var, c')]
                              }
                         with Bot.Found_BOT ->
                           let flow' = set_domain_cur bottom man flow in
                           return flow'
                       end
                     | _ -> None
                   );
               }
           with Bot.Found_BOT ->
             return flow'
        )
        (man.exec ctx) man.flow

    | _ -> exec man ctx stmt flow


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
