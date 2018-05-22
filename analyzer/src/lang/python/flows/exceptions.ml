(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of exceptions flows. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Flow
open Framework.Ast
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Framework.Alarm
open Universal.Ast
open Ast
open Addr

let name = "python.flows.exceptions"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                          {2 Flow tokens}                                *)
(*==========================================================================*)

type token +=
  | TExn of addr (** address of the exception instance *)



(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)

module Domain =
struct

  let rec exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_try(body, excepts, orelse, finally) ->
      let old_flow = flow in
      (* Remove all previous exception flows *)
      let flow0 = man.flow.filter (fun _ -> function TExn _ -> false | _ -> true) flow in

      (* Execute try body *)
      let try_flow = man.exec ctx body flow0 in

      (* Execute handlers *)
      let flow_caught, flow_uncaught = List.fold_left (fun (acc_caught, acc_uncaught) excpt ->
          let caught = exec_except man ctx excpt range acc_uncaught in
          let uncaught = escape_except man ctx excpt range acc_uncaught in
          man.flow.join caught acc_caught, uncaught
        ) (man.flow.bottom, try_flow)  excepts in

      (* Execute else body after removing all exceptions *)
      let orelse_flow = man.flow.filter (fun _ -> function TExn _ -> false | _ -> true) try_flow |>
                        man.exec ctx orelse
      in

      (* Execute finally body *)
      let flow =
        man.flow.join orelse_flow flow_caught |>
        man.exec ctx finally |> (* FIXME : execute finally also on still uncaught envs *)
        man.flow.join flow_uncaught
      in

      (* Restore old exceptions *)
      man.flow.fold (fun acc env tk ->
          match tk with
          | TExn _ -> man.flow.add tk env acc
          | _ -> acc
        ) flow old_flow |>
      return

    | S_py_raise(Some exp) ->
      man.eval ctx exp flow |>
      eval_to_oexec (fun exp flow ->
          match ekind exp with
          | E_addr ({addr_kind = A_py_instance _} as addr) ->
            if Addr.isinstance addr (Addr.find_builtin "BaseException") then
              let cur = man.flow.get TCur flow in
              let flow' = man.flow.add (TExn addr) cur flow |>
                          man.flow.set TCur man.env.bottom
              in
              return flow'
            else
              man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow |>
              return

          | E_addr ({addr_kind = A_py_class _} as cls) ->
            eval_alloc_instance man ctx cls None range flow |>
            oeval_to_oexec (fun addr flow ->
                exec man ctx {stmt with skind = S_py_raise(Some (mk_addr addr range))} flow
              ) (man.exec ctx) man.flow
          | _ ->
            man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow |>
            return
        ) (man.exec ctx) man.flow


    | S_py_raise None ->
      Framework.Exceptions.panic "exceptions: re-raise previous caught exception not supported"


    | _ -> None


  and exec_except man ctx excpt range flow =
    debug "exec except";
    let flow0 = man.flow.set TCur man.env.bottom flow |>
                man.flow.filter (fun _ -> function TExn _ -> false | _ -> true)
    in
    let flow1 =
      match excpt.py_excpt_type with
      (* Default except case: catch all exceptions *)
      | None ->
        (* Add all remaining exceptions env to cur *)
        man.flow.fold (fun acc env tk ->
            match tk with
            | TExn _ -> man.flow.add TCur env acc
            | _ -> acc
          ) flow0 flow

      (* Catch a particular exception *)
      | Some e ->
        (* Add exception that match expression e *)
        man.flow.fold (fun acc env tk ->
            match tk with
            | TExn eaddr ->
              (* Evaluate e in env to check if it corresponds to eaddr *)
              let flow = man.flow.set TCur env flow0 in
              let flow' =
                man.eval ctx e flow |>
                eval_to_exec (fun e flow ->
                    match ekind e with
                    | E_addr cls ->
                      if Addr.issubclass cls (Addr.find_builtin "BaseException") then
                        if not (Addr.isinstance eaddr cls) then
                          man.flow.set TCur man.env.bottom flow
                        else
                          match excpt.py_excpt_name with
                          | None -> flow
                          | Some v -> man.exec ctx (mk_assign (mk_var v range) (mk_addr eaddr range) range) flow
                      else
                        man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow
                    | _ ->
                      man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow
                  ) (man.exec ctx) man.flow
              in
              let cur = man.flow.get TCur flow' in
              man.flow.add TCur cur acc
            | _ -> acc
          ) flow0 flow
    in
    (* Execute exception handler *)
    man.exec ctx excpt.py_excpt_body flow1


    and escape_except man ctx excpt range flow =
      debug "escape except";
      let flow0 = man.flow.set TCur man.env.bottom flow |>
                  man.flow.filter (fun _ -> function TExn _ -> false | _ -> true)
      in
      match excpt.py_excpt_type with
      | None -> flow0

      | Some e ->
        man.flow.fold (fun acc env tk ->
            match tk with
            | TExn eaddr ->
              (* Evaluate e in env to check if it corresponds to eaddr *)
              let flow = man.flow.set TCur env flow0 in
              let flow' =
                man.eval ctx e flow |>
                eval_to_exec (fun e flow ->
                    match ekind e with
                    | E_addr cls ->
                      if Addr.issubclass cls (Addr.find_builtin "BaseException") && not (Addr.isinstance eaddr cls) then
                        man.flow.add (TExn eaddr) env flow
                      else
                        flow
                    | _ -> flow
                  )  (man.exec ctx) man.flow
              in
              man.flow.fold (fun acc env tk ->
                  match tk with
                  | TExn _ -> man.flow.add tk env acc
                  | _ -> acc
                ) flow0 flow'
            | _ -> acc
          ) flow0 flow

    let eval _ _ _ _ = None

    let ask : type r. ('a, unit) manager -> Framework.Context.context -> r Framework.Query.query -> 'a flow -> r option =
      fun man ctx query flow ->
      match query with
      | Framework.Alarm.QGetAlarms ->
        let alarms = man.flow.fold (fun acc env tk ->
            match tk with
            | TExn exn ->
              let exn_name =
                match exn.addr_kind with
                | A_py_instance({addr_kind = A_py_class(C_user(cls), _)}, _) -> cls.py_cls_var.vname
                | A_py_instance({addr_kind = A_py_class(C_builtin(name), _)}, _) -> name
                | _ -> assert false
              in
              let alarm = {
                alarm_kind = Alarm.UncaughtException(exn_name);
                alarm_range = exn.addr_range;
              }
              in
              alarm :: acc
            | _ -> acc
          ) [] flow
        in
        Some alarms
      | _ -> None

  let init man ctx prog flow = ctx, flow

end

let setup () =
  register_domain name (module Domain);
  register_pp_token (fun next fmt -> function
      | TExn(exn) -> Format.fprintf fmt "exn:%a" Universal.Pp.pp_addr exn
      | tk -> next fmt tk
    )
