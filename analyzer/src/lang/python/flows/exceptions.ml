(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of exceptions flows. *)

open Framework.Essentials
open Framework.Domains.Stateless
open Universal.Ast
open Ast
open Addr

let name = "python.flows.exceptions"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                          {2 Flow tokens}                                *)
(*==========================================================================*)

type Flow.token +=
  | TExn of py_object (** Flows emerging from a raised exception instance *)


(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)

module Domain =
struct


  let exec_interface = Framework.Domain.{
      import = [];
      export = [Zone.Z_py];
    }

  let eval_interface = Framework.Domain.{
      import = [
        (Zone.Z_py, Zone.Z_py_object);
        (Universal.Zone.Z_heap, Universal.Zone.Z_heap)
      ];
      export = [];
    }

  let exec_except excpt range (man: ('a, unit) manager) ctx (flow: 'a Flow.flow) : 'a Flow.flow =
    debug "exec except on@ @[%a@]" man.flow.print flow;
    let flow0 = man.flow.set Flow.TCur man.env.bottom flow |>
                man.flow.filter (fun tk _ -> match tk with TExn _ -> false | _ -> true)
    in
    debug "exec except flow0@ @[%a@]" man.flow.print flow0;
    let flow1 =
      match excpt.py_excpt_type with
      (* Default except case: catch all exceptions *)
      | None ->
        (* Add all remaining exceptions env to cur *)
        man.flow.fold (fun tk env acc ->
            match tk with
            | TExn _ -> man.flow.add Flow.TCur env acc
            | _ -> acc
          ) flow flow0

      (* Catch a particular exception *)
      | Some e ->
        (* Add exception that match expression e *)
        man.flow.fold (fun tk env acc ->
            match tk with
            | TExn exn ->
              (* Evaluate e in env to check if it corresponds to eaddr *)
              man.flow.set Flow.TCur env flow0 |>
              man.eval e ~zpath:(Zone.Z_py, Zone.Z_py_object) ctx |>
              Post.bind_flow man ctx @@ fun e flow ->
              begin match ekind e with
                | E_py_object obj ->
                  let flow' =
                    if Addr.issubclass obj (Addr.find_builtin "BaseException") then
                      if not (Addr.isinstance exn obj) then man.flow.set Flow.TCur man.env.bottom flow
                      else
                        match excpt.py_excpt_name with
                        | None -> flow
                        | Some v -> man.exec (mk_assign (mk_var v range) (mk_py_object exn range) range) ctx flow
                    else
                      man.exec (Utils.mk_builtin_raise "TypeError" range) ctx flow
                  in
                  man.flow.fold (fun tk env acc ->
                      match tk with
                      | Flow.TCur | TExn _ -> man.flow.add tk env acc
                      | _ -> acc
                    ) flow' acc
                | _ -> assert false
              end
            | _ -> acc
          ) flow flow0
    in
    debug "except flow1 =@ @[%a@]" man.flow.print flow1;
    (* Execute exception handler *)
    man.exec excpt.py_excpt_body ctx flow1


  let escape_except excpt range man ctx flow =
    debug "escape except";
    let flow0 = man.flow.set Flow.TCur man.env.bottom flow |>
                man.flow.filter (fun tk _ -> match tk with TExn _ -> false | _ -> true)
    in
    match excpt.py_excpt_type with
    | None -> flow0

    | Some e ->
      man.flow.fold (fun tk env acc ->
          match tk with
          | TExn exn ->
            (* Evaluate e in env to check if it corresponds to exn *)
            man.flow.set Flow.TCur env flow0 |>
            man.eval e ~zpath:(Zone.Z_py, Zone.Z_py_object) ctx |>
            Post.bind_flow man ctx @@ fun e flow ->
            begin match ekind e with
              | E_py_object obj ->
                let flow' =
                  if Addr.issubclass obj (Addr.find_builtin "BaseException") && not (Addr.isinstance exn obj) then
                    man.flow.add (TExn exn) env flow
                  else
                    flow
                in
                man.flow.fold (fun tk env acc ->
                    match tk with
                    | TExn _ -> man.flow.add tk env acc
                    | _ -> acc
                  ) flow' flow0

              | _ -> assert false
            end

          | _ -> acc
        ) flow flow0

  let exec zone stmt man ctx flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_try(body, excepts, orelse, finally) ->
      let old_flow = flow in
      (* Remove all previous exception flows *)
      let flow0 = man.flow.filter (fun tk _ -> match tk with TExn _ -> false | _ -> true) flow in

      (* Execute try body *)
      let try_flow = man.exec body ctx flow0 in

      debug "post try flow:@\n  @[%a@]" man.flow.print try_flow;

      (* Execute handlers *)
      let flow_caught, flow_uncaught = List.fold_left (fun (acc_caught, acc_uncaught) excpt ->
          let caught = exec_except excpt range man ctx acc_uncaught in
          let uncaught = escape_except excpt range man ctx acc_uncaught in
          man.flow.join caught acc_caught, uncaught
        ) (man.flow.bottom, try_flow)  excepts in

      (* Execute else body after removing all exceptions *)
      let orelse_flow = man.flow.filter (fun tk _ -> match tk with TExn _ -> false | _ -> true) try_flow |>
                        man.exec orelse ctx
      in

      (* Execute finally body *)
      let flow =
        man.flow.join orelse_flow flow_caught |>
        man.exec finally ctx |> (* FIXME : execute finally also on still uncaught envs *)
        man.flow.join flow_uncaught
      in

      (* Restore old exceptions *)
      man.flow.fold (fun tk env acc ->
          match tk with
          | TExn _ -> man.flow.add tk env acc
          | _ -> acc
        ) old_flow flow |>

      Post.return

    | S_py_raise(Some exp) ->
      man.eval exp ~zpath:(Zone.Z_py, Zone.Z_py_object) ctx flow |>
      Post.bind man ctx @@ fun exp flow ->
      begin match ekind exp with
        | E_py_object obj ->
          if Addr.isinstance obj (Addr.find_builtin "BaseException") then
            let cur = man.flow.get Flow.TCur flow in
            man.flow.add (TExn obj) cur flow |>
            man.flow.set Flow.TCur man.env.bottom |>
            Post.return
          else
          if Addr.isclass obj then
            man.eval (mk_alloc_instance obj range) ~zpath:(Universal.Zone.Z_heap, Universal.Zone.Z_heap) ctx flow |>
            Post.bind  man ctx @@ fun alloc flow ->
            match ekind alloc with
            | E_addr addr ->
              let obj = (addr, mk_py_empty range) in
              man.exec {stmt with skind = S_py_raise(Some (mk_py_object obj range))} ctx flow |>
              Post.return
            | _ -> assert false
          else
            man.exec (Utils.mk_builtin_raise "TypeError" range) ctx flow |>
            Post.return

        | _ -> assert false
      end

    | S_py_raise None ->
      Framework.Exceptions.panic_at stmt.srange "exceptions: re-raise previous caught exception not supported"

    | _ -> None


    let eval _ _ _ _ _ = None

    let ask : type r. r Framework.Query.query -> ('a, unit) manager -> Framework.Context.context -> 'a Flow.flow -> r option =
      fun query man ctx flow ->
        let open Framework.Alarm in
        match query with
        | QGetAlarms ->
          let alarms = man.flow.fold (fun tk env acc ->
              match tk with
              | TExn exn ->
                let exn_name = Addr.class_of_object exn |> Addr.object_name in
                let range = Addr.addr_of_object exn |> Universal.Ast.range_of_addr in
                let alarm = {
                  alarm_kind = Alarm.UncaughtException(exn_name);
                  alarm_range = range;
                  alarm_level = ERROR;
                }
                in
                alarm :: acc
              | _ -> acc
            ) flow []
          in
          Some alarms
        | _ -> None

  let init prog man ctx flow = None

end

let setup () =
  register_domain name (module Domain);
  Flow.register_pp_token (fun next fmt -> function
      | TExn(exn) -> Format.fprintf fmt "exn:%a" pp_py_object exn
      | tk -> next fmt tk
    )
