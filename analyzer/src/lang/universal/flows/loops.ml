(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Loops iterator with widening. *)

open Framework.Lattice
open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Global
open Framework.Domains.Stateless
open Framework.Ast
open Ast


let name = "universal.flows.loops"

let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                         {2 Loops flow token}                            *)
(*==========================================================================*)

type token +=
  | TBreak
  (** Control flows reaching a break statement *)

  | TContinue
  (** Control flows reaching a continue statement *)

let () =
  register_pp_token (fun next fmt -> function
      | TBreak -> Format.pp_print_string fmt "break"
      | TContinue -> Format.pp_print_string fmt "cont"
      | tk -> next fmt tk
    )

(*==========================================================================*)
(**                       {2 Command line options}                          *)
(*==========================================================================*)

let opt_loop_widening_delay : int ref = ref 1
(** Number of iterations before applying a widening. *)

let opt_loop_unrolling : int ref = ref 1
(** Number of unrolling iterations before joining the environments. *)


(*==========================================================================*)
(**                            {2 Domain}                                   *)
(*==========================================================================*)

module Domain : Stateless.DOMAIN =
struct

  (*==========================================================================*)
  (**                      {2 Transfer functions}                             *)
  (*==========================================================================*)

  let init prg man ctx flow = ctx, flow

  let rec exec stmt manager ctx flow =
    match skind stmt with
    | S_while(cond, body) ->
      debug "while:@\n abs = @[%a@]" manager.flow.print flow;

      let flow0 = manager.flow.remove TContinue flow |>
                  manager.flow.remove TBreak
      in

      let flow_init, flow_out = unroll cond body manager ctx flow0 in

      debug "post unroll:@\n abs0 = @[%a@]@\n abs out = @[%a@]"
        manager.flow.print flow_init
        manager.flow.print flow_out
      ;


      let res0 =
        lfp
          !opt_loop_widening_delay
          cond body
          manager ctx
          flow_init flow_init
        |>
        manager.exec
          (mk_assume
             (mk_not cond (tag_range cond.erange "neg"))
             (tag_range cond.erange "while out cond")
          ) ctx
        |>
        manager.flow.join flow_out
      in

      debug "lfp reached:@\n abs = @[%a@]" manager.flow.print res0;

      let res1 = manager.flow.map (fun eabs -> function
          | TCur ->
            let brk = manager.flow.get TBreak res0 in
            manager.env.join eabs brk

          | TBreak ->
            manager.flow.get TBreak flow

          | TContinue ->
            manager.flow.get TContinue flow

          | _ -> eabs
        ) res0 in

      debug "while post abs:@\n abs = @[%a@]" manager.flow.print res1;

      return res1

    | S_break ->
      let cur = manager.flow.get TCur flow in
      manager.flow.add TBreak cur flow |>
      manager.flow.remove TCur |>
      return

    | S_continue ->
      let cur = manager.flow.get TCur flow in
      manager.flow.add TContinue cur flow |>
      manager.flow.remove TCur |>
      return

    | _ ->
      None

  and lfp delay cond body manager ctx flow_init flow =
    let flow0 = manager.flow.map (fun eabs -> function
        | TCur ->
          let cont = manager.flow.get TContinue flow in
          manager.env.join eabs cont
        | _ -> eabs
      ) flow in

    debug "lfp:@\n delay = %d@\n abs = @[%a@]"
      delay manager.flow.print flow0
    ;

    let flow1 =
      manager.exec
        (mk_assume cond (tag_range cond.erange "while cond"))
        ctx flow0 |>
      manager.exec body ctx
    in

    debug "lfp post:@\n res = @[%a@]" manager.flow.print flow1;

    let flow2 = manager.flow.join flow_init flow1 in

    debug "lfp join:@\n res = @[%a@]" manager.flow.print flow2;

    if manager.flow.leq flow2 flow then
      flow2
    else
    if delay = 0 then
      let wflow = manager.flow.widening ctx flow flow2 in
      debug
        "widening:@\n abs =@\n@[  %a@]@\n abs' =@\n@[  %a@]@\n res =@\n@[  %a@]"
        manager.flow.print flow
        manager.flow.print flow2
        manager.flow.print wflow;
      lfp !opt_loop_widening_delay cond body manager ctx flow_init wflow
    else
      lfp (delay - 1) cond body manager ctx flow_init flow2


  and unroll cond body manager ctx flow =
    let rec loop i flow =
      debug "unrolling iteration %d" i;
      if i = 0 then (flow, manager.flow.bottom)
      else
        let flow1 =
          manager.exec {skind = S_assume cond; srange = cond.erange} ctx flow |>
          manager.exec body ctx
        in
        let flow2 =
          manager.exec (mk_assume (mk_not cond cond.erange) cond.erange) ctx flow
        in
        let flow1', flow2' = loop (i - 1) flow1 in
        flow1', manager.flow.join flow2 flow2'
    in
    loop !opt_loop_unrolling flow


  let eval _ _ _ _ = None

  let ask _ _ _ _ = None

end

(*==========================================================================*)
(**                               {2 Setup}                                 *)
(*==========================================================================*)


let setup () =
  Stateless.register_domain name (module Domain)
