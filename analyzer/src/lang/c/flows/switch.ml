(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Control flow abstraction for switch statements. *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Stateless
open Framework.Eval
open Framework.Context
open Framework.Lattice
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.flows.switch"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                            {2 Flow tokens}                              *)
(*==========================================================================*)

type Framework.Flow.token +=
  | TSwitch
  (** Switch environments uncaught by previous cases. *)



(*==========================================================================*)
(**                             {2 Contexts}                                *)
(*==========================================================================*)

type _ Framework.Context.key +=
  | KSwitchExpr: expr Framework.Context.key
  (** Condition expression of the most enclosing switch statement. *)



(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)


module Domain =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let exec man ctx stmt flow =
    match skind stmt with

    | S_c_switch(e, body) ->
      (* Save TCur env in TSwitch token, then reset TCur and TBreak. *)
      let cur = man.flow.get TCur flow in
      let flow0 = man.flow.set TSwitch cur flow |>
                  man.flow.remove TCur |>
                  man.flow.remove Universal.Flows.Loops.TBreak
      in

      (* Store e in the context and execute body. *)
      let ctx = Framework.Context.add KSwitchExpr e ctx in
      let flow1 = man.exec ctx body  flow0 in

      (* Merge resulting TCur, TBreak and TSwitch (in case when there is no default case) *)
      let cur =
        man.env.join
          (man.flow.get TCur flow1)
          (man.flow.get Universal.Flows.Loops.TBreak flow1) |>
        man.env.join
          (man.flow.get TSwitch flow1)

      in
      (* Put the merge into TCur and restore previous TBreak and TSwitch. *)
      let flow2 = man.flow.set TCur cur flow1 |>
                  man.flow.set Universal.Flows.Loops.TBreak (man.flow.get Universal.Flows.Loops.TBreak flow) |>
                  man.flow.set TSwitch (man.flow.get TSwitch flow)
      in
      return flow2

    | S_c_switch_case(e) ->
      let e0 = Framework.Context.find KSwitchExpr ctx in
      let flow0 = man.flow.set TCur (man.flow.get TSwitch flow) flow in
      Universal.Utils.assume_to_exec
        (mk_binop e0 O_eq e stmt.srange ~etyp:u8)
        (fun true_flow -> man.flow.set TSwitch man.env.bottom true_flow)
        (fun false_flow ->
           let cur = man.flow.get TCur false_flow in
           man.flow.set TCur man.env.bottom false_flow |>
           man.flow.set TSwitch cur
        )
        ~merge_case:(fun true_flow false_flow ->
            let true_cur = man.flow.get TCur true_flow in
            let false_cur = man.flow.get TCur false_flow in
            man.flow.merge (fun tk true_env false_env ->
                match tk, true_env, false_env with
                | TCur, _, _ -> Some true_cur
                | TSwitch, _, _ -> Some false_cur
                | _, Some env, _ | _, _, Some env -> Some env
                | _, None, None -> None
              ) true_flow false_flow
          )
        man ctx flow0 () |>
      man.flow.add TCur (man.flow.get TCur flow) |>
      return

    | S_c_switch_default ->
      let cur = man.env.join (man.flow.get TSwitch flow) (man.flow.get TCur flow) in
      let flow = man.flow.set TCur cur flow |>
                 man.flow.set TSwitch man.env.bottom
      in
      return flow

    | _ -> None


  let eval man ctx exp flow = None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain);
  Framework.Flow.register_pp_token (fun next fmt -> function
      | TSwitch -> Format.fprintf fmt "switch"
      | tk -> next fmt tk
    );
  Framework.Context.register_key_equality {
    case = (let f : type a b. chain -> a key -> b key -> (a, b) eq option =
              fun chain k1 k2 ->
                match k1, k2 with
                | KSwitchExpr, KSwitchExpr -> Some Eq
                | _ -> chain.check k1 k2
            in
            f);
  }
