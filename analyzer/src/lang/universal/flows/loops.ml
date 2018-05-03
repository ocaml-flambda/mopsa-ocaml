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
open Framework.Domains.Stateful
open Framework.Domains.Stateless
open Framework.Exec
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

let opt_loop_widening_delay : int ref = ref 0
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

  let init man ctx prg flow = ctx, flow

  let rec exec manager ctx stmt flow =
    match skind stmt with
    | S_while(cond, body) ->
      debug "while:@\n abs = @[%a@]" manager.flow.print flow;

      let flow0 = manager.flow.remove TContinue flow |>
                  manager.flow.remove TBreak
      in

      let flow_init, flow_out = unroll manager ctx cond body flow0 in

      debug "post unroll:@\n abs0 = @[%a@]@\n abs out = @[%a@]"
        manager.flow.print flow_init
        manager.flow.print flow_out
      ;


      let res0 =
        lfp
          manager ctx !opt_loop_widening_delay
          cond body
          flow_init flow_init
        |>
        manager.exec ctx
          (mk_assume
             (mk_not cond (tag_range cond.erange "neg"))
             (tag_range cond.erange "while out cond")
          )
        |>
        manager.flow.join flow_out
      in

      debug "lfp reached:@\n abs = @[%a@]" manager.flow.print res0;

      let res1 = manager.flow.add TCur (manager.flow.get TBreak res0) res0 |>
                 manager.flow.set TBreak (manager.flow.get TBreak flow) |>
                 manager.flow.set TContinue (manager.flow.get TContinue flow)
      in

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

  and lfp manager ctx delay cond body flow_init flow =
    let flow0 = manager.flow.remove TContinue flow |>
                manager.flow.remove TBreak
    in

    debug "lfp:@\n delay = %d@\n abs = @[%a@]"
      delay manager.flow.print flow0
    ;

    let flow1 =
      manager.exec ctx
        (mk_assume cond (tag_range cond.erange "while cond"))
        flow0 |>
      manager.exec ctx body
    in

    let flow2 = merge_cur_and_continue manager flow1 in
    
    debug "lfp post:@\n res = @[%a@]" manager.flow.print flow1;

    let flow3 = manager.flow.join flow_init flow2 in

    debug "lfp join:@\n res = @[%a@]" manager.flow.print flow3;

    if manager.flow.leq flow3 flow then
      flow3
    else
    if delay = 0 then
      let wflow = manager.flow.widening ctx flow flow3 in
      debug
        "widening:@\n abs =@\n@[  %a@]@\n abs' =@\n@[  %a@]@\n res =@\n@[  %a@]"
        manager.flow.print flow
        manager.flow.print flow3
        manager.flow.print wflow;
      lfp manager ctx !opt_loop_widening_delay cond body flow_init wflow
    else
      lfp manager ctx (delay - 1) cond body flow_init flow3

  and unroll manager ctx cond body flow =
    let rec loop i flow =
      debug "unrolling iteration %d" i;
      if i = 0 then (flow, manager.flow.bottom)
      else
        let flow1 =
          manager.exec ctx {skind = S_assume cond; srange = cond.erange} flow |>
          manager.exec ctx body |>
          merge_cur_and_continue manager
        in
        let flow2 =
          manager.exec ctx (mk_assume (mk_not cond cond.erange) cond.erange) flow
        in
        let flow1', flow2' = loop (i - 1) flow1 in
        flow1', manager.flow.join flow2 flow2'
    in
    loop !opt_loop_unrolling flow

  and merge_cur_and_continue manager flow =
    manager.flow.map (fun eabs -> function
        | TCur ->
          let cont = manager.flow.get TContinue flow in
          manager.env.join eabs cont
        | TContinue -> manager.env.bottom
        | _ -> eabs
      ) flow


  let eval _ _ _ _ = None

  let ask _ _ _ _ = None

end

(*==========================================================================*)
(**                               {2 Setup}                                 *)
(*==========================================================================*)


let setup () =
  Stateless.register_domain name (module Domain);
  Framework.Options.register (
    "-widening-delay", Arg.Set_int opt_loop_widening_delay, " number of iterations before applying a widening (default: 0)"
  );
  Framework.Options.register (
    "-loop-unrolling", Arg.Set_int opt_loop_unrolling, " number of unrolling iterations before joining the environments (default: 1)"
  )
