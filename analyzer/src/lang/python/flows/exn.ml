(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of exceptions flows. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr
open Alarms

(* type token +=
 *    | T_exn of expr
 *
 * let () =
 *   register_token {
 *       compare = (fun next tk1 tk2 -> match tk1, tk2 with
 *                                      | T_exn e1, T_exn e2 -> compare_expr e1 e2
 *                                      | _ -> next tk1 tk2);
 *       print = (fun next fmt tk ->
 *         match tk with
 *         | T_exn e -> Format.fprintf fmt "exn(%a)" pp_expr e
 *         | _ -> next fmt tk);
 *     } *)


module Domain =
  struct

    type _ domain += D_python_flows_exceptions : unit domain

    let id = D_python_flows_exceptions
    let name = "python.flows.exceptions"
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_python_flows_exceptions -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = [any_zone]; import = []}
    let eval_interface = {export = []; import = []}

    let init _ _ flow = Some flow
    let eval _ _ _ _ = None

    let rec exec (zone:Framework.Zone.zone) (stmt:Framework.Ast.stmt) (man:('a, unit) man) (flow:'a flow) : 'a post option =
      let range = srange stmt in
      match skind stmt with
      | S_py_try(body, excepts, orelse, finally) ->
         let old_flow = flow in
         (* Remove all previous exception flows *)
         let flow0 = Flow.filter (function
                         | T_alarm {alarm_kind = APyException _} -> fun _ -> false
                         | _ -> fun _ -> true) man flow in

         (* Execute try body *)
         let try_flow = man.exec body flow0 in
          debug "post try flow:@\n  @[%a@]" (Flow.print man) try_flow;
         (* Execute handlers *)
          let flow_caught, flow_uncaught =
            List.fold_left (fun (acc_caught, acc_uncaught) excpt ->
                let caught = exec_except man excpt range acc_uncaught in
                let uncaught = escape_except man excpt range acc_uncaught in
                Flow.join man caught acc_caught, uncaught)
              (Flow.bottom (Flow.get_all_annot try_flow), try_flow)  excepts in

         (* Execute else body after removing all exceptions *)
         let orelse_flow = Flow.filter (function
                               | T_alarm {alarm_kind = APyException _} -> fun _ -> false
                               | _ -> fun _ -> true) man try_flow |>
                             man.exec orelse
         in

         (* Execute finally body *)
         let flow =
           Flow.join man orelse_flow flow_caught |>
             man.exec finally |> (* FIXME : execute finally also on still uncaught envs *)
             Flow.join man flow_uncaught
         in

         (* Restore old exceptions *)
         Flow.fold (fun acc tk env ->
             match tk with
             | T_alarm {alarm_kind = APyException _} -> Flow.add tk env man acc
             | _ -> acc
           ) flow man old_flow |>
           Post.return

      | S_py_raise(Some exp) ->
         debug "Raising %a@\n" pp_expr exp;
         (man.eval exp flow |>
            Post.bind_with_cleaners man (fun exp cleaners flow ->
                (* match ekind exp with
                 * | E_py_object obj -> *)
                Post.assume
                  (mk_py_isinstance_builtin exp "BaseException" range)
                  man
                  ~fthen:(fun true_flow ->
                    debug "True flow, exp is %a@\n" pp_expr exp;
                    (*if Addr.isinstance obj (Addr.find_builtin "BaseException") then*)

                    let true_flow =  man.exec (mk_block cleaners range) true_flow in
                    let cur = Flow.get T_cur man true_flow in
                    let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack true_flow in
                    let a = mk_alarm (APyException exp) range ~cs ~level:ERROR in
                    let flow' = Flow.add (T_alarm a) cur man true_flow |>
                                  Flow.set T_cur man.bottom man
                    in
                    Post.of_flow flow')
                  ~felse:(fun false_flow ->
                    Post.assume
                      (* isclass obj <=> isinstance(obj, type) *)
                      (mk_py_isinstance_builtin exp "type" range)
                      man
                      ~fthen:(fun true_flow ->
                        man.exec {stmt with skind = S_py_raise(Some (mk_py_call exp [] range))} true_flow
                        |> Post.of_flow)
                      ~felse:(fun false_flow ->
                        man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow
                        |> Post.of_flow)
                      false_flow
                  )
                  flow
              (* | _ -> debug "%a@\n" pp_expr exp; assert false *)
              )
         )
         |> OptionExt.return


      | S_py_raise None ->
         panic_at stmt.srange "exceptions: re-raise previous caught exception not supported"

      | _ -> None


    and exec_except (man:('a, unit) man) excpt range (flow:'a flow) : 'a flow =
      debug "exec except on@ @[%a@]" (Flow.print man) flow;
      let flow0 = Flow.set T_cur man.bottom man flow in
      debug "flow_cur %a@\n" (Flow.print man) flow;
      let flow0 = Flow.filter (function
                        | T_alarm {alarm_kind = APyException _} -> fun _ -> false
                        | _ -> fun _ -> true) man flow0 in
      debug "exec except flow0@ @[%a@]" (Flow.print man) flow0;
      let flow1 =
        match excpt.py_excpt_type with
        (* Default except case: catch all exceptions *)
        | None ->
           (* Add all remaining exceptions env to cur *)
           Flow.fold (fun acc tk env ->
               match tk with
               | T_alarm {alarm_kind = APyException _} -> Flow.add T_cur env man acc
               | _ -> acc)
             flow0 man flow

        (* Catch a particular exception *)
        | Some e ->
           (* Add exception that match expression e *)
           Flow.fold (fun acc tk env ->
               match tk with
               | T_alarm {alarm_kind = APyException exn} ->
                  (* Evaluate e in env to check if it corresponds to eaddr *)
                  let flow = Flow.set T_cur env man flow0 in
                  let flow' =
                    man.eval e flow |>
                      Post.bind man (fun e flow ->
                          match ekind e with
                          | E_py_object obj ->
                             Post.assume
                               (* if Addr.issubclass obj (Addr.find_builtin "BaseException") then *)
                               (* issubclass cls1 cls2 <-> ???*)
                               (mk_py_call (mk_py_object (Addr.find_builtin "issubclass") range) [e; mk_py_object (Addr.find_builtin "BaseException") range] range)
                               man
                               ~fthen:(fun true_flow ->
                                 Post.assume
                                   (mk_py_isinstance exn e range)
                                   man
                                   ~fthen:(fun true_flow ->
                                     match excpt.py_excpt_name with
                                     | None -> Post.of_flow true_flow
                                     | Some v -> man.exec (mk_assign (mk_var v range) exn range) true_flow |> Post.of_flow)
                                   ~felse:(fun false_flow ->
                                     (*if not (Addr.isinstance exn obj) then*)
                                     Flow.set T_cur man.bottom man false_flow |> Post.of_flow
                                   )
                                   true_flow
                               )
                               ~felse:(fun false_flow ->
                                 (* else *)
                                 man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Post.of_flow)
                               flow
                          | _ -> assert false
                        )
                  in
                  let flow' = flow'.Framework.Post.flow in
                  Flow.fold (fun acc tk env ->
                      match tk with
                      | T_cur | T_alarm {alarm_kind = APyException _} -> Flow.add tk env man acc
                      | _ -> acc
                    ) acc man flow'
               | _ -> acc
             ) flow0 man flow
      in
      debug "except flow1 =@ @[%a@]" (Flow.print man) flow1;
      (* Execute exception handler *)
      man.exec excpt.py_excpt_body flow1


    and escape_except man excpt range flow =
      debug "escape except";
      let flow0 = Flow.set T_cur man.bottom man flow |>
                    Flow.filter (function
                        | T_alarm {alarm_kind = APyException _} -> fun _ -> false
                        | _ -> fun _ -> true) man in
      match excpt.py_excpt_type with
      | None -> flow0

      | Some e ->
         Flow.fold (fun acc tk env ->
             match tk with
             | T_alarm {alarm_kind = APyException exn} ->
                (* Evaluate e in env to check if it corresponds to exn *)
                let flow = Flow.set T_cur env man flow0 in
                let flow' =
                  man.eval e flow |>
                    Post.bind man (fun e flow ->
                        match ekind e with
                        | E_py_object obj ->
                           Post.assume
                             (mk_py_call (mk_py_object (Addr.find_builtin "issubclass") range) [e; mk_py_object (Addr.find_builtin "BaseException") range] range)
                             man
                             (* if Addr.issubclass obj (Addr.find_builtin "BaseException") && not (Addr.isinstance exn obj) then
                              *   man.flow.add (TExn exn) env flow *)
                             ~fthen:(fun true_flow ->
                               Post.assume
                                 (mk_py_isinstance exn e range)
                                 man
                                 ~fthen:(fun true_flow -> Post.of_flow true_flow)
                                 ~felse:(fun false_flow ->
                                   let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack false_flow in
                                   let a = mk_alarm (APyException exn) range ~cs ~level:ERROR in
                                   Flow.add (T_alarm a) env man false_flow |> Post.of_flow)
                                 true_flow)
                             ~felse:(fun false_flow -> Post.of_flow false_flow)
                             flow
                        | _ -> Post.of_flow flow
                      )
                in
                let flow' = flow'.Framework.Post.flow in
                Flow.fold (fun acc tk env ->
                    match tk with
                    | T_alarm {alarm_kind = APyException _} -> Flow.add tk env man acc
                    | _ -> acc
                  ) flow0 man flow'
             | _ -> acc
           ) flow0 man flow


    let ask _ _ _ = None
  end

let () = Framework.Domains.Stateless.register_domain (module Domain)
