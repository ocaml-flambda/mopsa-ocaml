(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Loops iterator with widening. *)


open Framework.Essentials
open Ast



let name = "universal.flows.loops"

let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                         {2 Loops flow token}                            *)
(*==========================================================================*)

type Flow.token +=
  | TBreak
  (** Control flows reaching a break statement *)

  | TContinue
  (** Control flows reaching a continue statement *)

let () =
  Flow.register_pp_token (fun next fmt -> function
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

module Domain : Framework.Domains.Stateless.DOMAIN =
struct

  (*==========================================================================*)
  (**                      {2 Transfer functions}                             *)
  (*==========================================================================*)

  let init prog man ctx flow = None

  let import_exec = []
  let export_exec = [Framework.Zone.top]

  let rec exec zone (stmt: stmt) (man: ('a, unit) manager) ctx flow =
    match skind stmt with
    | S_while(cond, body) ->
      debug "while:@\n abs = @[%a@]" man.flow.print flow;

      let flow0 = man.flow.remove TContinue flow |>
                  man.flow.remove TBreak
      in

      let flow_init, flow_out = unroll cond body man ctx flow0 in

      debug "post unroll:@\n abs0 = @[%a@]@\n abs out = @[%a@]"
        man.flow.print flow_init
        man.flow.print flow_out
      ;


      let res0 =
        lfp
          !opt_loop_widening_delay
          cond body
          man ctx flow_init flow_init
        |>
        man.exec
          (mk_assume
             (mk_not cond cond.erange)
             cond.erange
          )
          ctx
        |>
        man.flow.join flow_out
      in

      debug "lfp reached:@\n abs = @[%a@]" man.flow.print res0;

      let res1 = man.flow.add Flow.TCur (man.flow.get TBreak res0) res0 |>
                 man.flow.set TBreak (man.flow.get TBreak flow) |>
                 man.flow.set TContinue (man.flow.get TContinue flow)
      in

      debug "while post abs:@\n abs = @[%a@]" man.flow.print res1;

      return (Framework.Post.of_flow res1)

    | S_break ->
      let cur = man.flow.get Flow.TCur flow in
      man.flow.add TBreak cur flow |>
      man.flow.remove Flow.TCur |>
      Post.of_flow |>
      return

    | S_continue ->
      let cur = man.flow.get Flow.TCur flow in
      man.flow.add TContinue cur flow |>
      man.flow.remove Flow.TCur |>
      Post.of_flow |>
      return

    | _ ->
      None

  and lfp delay cond body man ctx flow_init flow =
    let flow0 = man.flow.remove TContinue flow |>
                man.flow.remove TBreak
    in

    debug "lfp:@\n delay = %d@\n abs = @[%a@]"
      delay man.flow.print flow0
    ;

    let flow1 =
      man.exec
        (mk_assume cond cond.erange)
        ctx flow0 |>
      man.exec body ctx
    in

    let flow2 = merge_cur_and_continue man flow1 in

    debug "lfp post:@\n res = @[%a@]" man.flow.print flow1;

    let flow3 = man.flow.join flow_init flow2 in

    debug "lfp join:@\n res = @[%a@]" man.flow.print flow3;

    if man.flow.leq flow3 flow then
      flow3
    else
    if delay = 0 then
      let wflow = man.flow.widening ctx flow flow3 in
      debug
        "widening:@\n abs =@\n@[  %a@]@\n abs' =@\n@[  %a@]@\n res =@\n@[  %a@]"
        man.flow.print flow
        man.flow.print flow3
        man.flow.print wflow;
      lfp !opt_loop_widening_delay cond body man ctx flow_init wflow
    else
      lfp (delay - 1) cond body man ctx flow_init flow3

  and unroll cond body man ctx flow =
    let rec loop i flow =
      debug "unrolling iteration %d" i;
      if i = 0 then (flow, man.flow.bottom)
      else
        let flow1 =
          man.exec {skind = S_assume cond; srange = cond.erange} ctx flow |>
          man.exec body ctx |>
          merge_cur_and_continue man
        in
        let flow2 =
          man.exec (mk_assume (mk_not cond cond.erange) cond.erange) ctx flow
        in
        let flow1', flow2' = loop (i - 1) flow1 in
        flow1', man.flow.join flow2 flow2'
    in
    loop !opt_loop_unrolling flow

  and merge_cur_and_continue man flow =
    man.flow.map (fun tk eabs ->
        match tk with
        | Flow.TCur ->
          let cont = man.flow.get TContinue flow in
          man.env.join eabs cont
        | TContinue -> man.env.bottom
        | _ -> eabs
      ) flow

  let import_eval = []
  let export_eval = []

  let eval zpath _ _ _ _ = None

  let ask _ _ _ _ = None

end

(*==========================================================================*)
(**                               {2 Setup}                                 *)
(*==========================================================================*)


let setup () =
  Framework.Domains.Stateless.register_domain name (module Domain);
  Framework.Utils.Options.register (
    "-widening-delay", Arg.Set_int opt_loop_widening_delay, " number of iterations before applying a widening (default: 0)"
  );
  Framework.Utils.Options.register (
    "-loop-unrolling", Arg.Set_int opt_loop_unrolling, " number of unrolling iterations before joining the environments (default: 1)"
  )
