(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of exceptions flows. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast
open Alarms


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.flows.exceptions"
      end)


    let opt_unprecise_exn = ref []
    (* Be unprecise on some exceptions *)

    let () =
      register_domain_option name {
        key = "-unprecise-exn";
        category = "Python";
        doc = " raised exceptions passed to this arguments will be collapsed \
               into one environment. Useful for exceptions the analysis is \
               unprecise on (for example, IndexError for the smashing \
               abstraction of lists).";
        spec = ArgExt.Set_string_list opt_unprecise_exn;
        default = "";
      }



    let interface = {
      iexec = {provides = [Zone.Z_py]; uses = [Zone.Z_py]};
      ieval = {provides = []; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let alarms = []

    let init _ _ flow = flow
    let eval _ _ _ _ = None

    let rec exec (zone:zone) (stmt:stmt) (man:('a, unit, 's) man) (flow:'a flow) : 'a post option =
      let range = srange stmt in
      match skind stmt with
      | S_py_try(body, excepts, orelse, finally) ->
        let old_flow = flow in
        (* Remove all previous exception flows *)
        let flow0 = Flow.filter (function
            | T_py_exception _ -> fun _ -> false
            | _ -> fun _ -> true) flow in

        (* Execute try body *)
        let try_flow = man.exec body flow0 in
        debug "post try flow:@\n  @[%a@]" (Flow.print man.lattice.print) try_flow;
        (* Execute handlers *)
        let flow_caught, flow_uncaught =
          List.fold_left (fun (acc_caught, acc_uncaught) excpt ->
              let caught = exec_except man excpt range acc_uncaught in
              let acc_uncaught = Flow.copy_ctx caught acc_uncaught in
              let uncaught = escape_except man excpt range acc_uncaught in
              let caught = Flow.copy_ctx uncaught caught in
              Flow.join man.lattice acc_caught caught, uncaught)
            (Flow.bottom (Flow.get_ctx try_flow) (Flow.get_alarms try_flow), try_flow)  excepts in

        (* Execute else body after removing all exceptions *)
        let orelse_flow = Flow.filter (function
            | T_py_exception _ -> fun _ -> false
            | _ -> fun _ -> true) try_flow |>
                          man.exec orelse
        in

        let apply_finally finally flow =
          let open Universal.Iterators.Loops in
          let open Universal.Iterators.Interproc.Common in
          Flow.fold (fun acc tk env ->
              match tk with
              | T_break | T_continue | T_return _ | T_py_exception _ ->
                Flow.singleton (Flow.get_ctx acc) T_cur env |>
                man.exec finally |>
                Flow.rename T_cur tk man.lattice |>
                Flow.join man.lattice acc
              | _ ->
                Flow.add tk env man.lattice acc
            ) (Flow.bottom_from flow) flow in

        (* Execute finally body *)
        let flow_caught_finally =
          Flow.join man.lattice orelse_flow flow_caught |>
          apply_finally finally in

        let flow_uncaught = Flow.copy_ctx flow_caught_finally flow_uncaught in
        let flow_uncaught_finally =
          apply_finally finally flow_uncaught in

        let flow = Flow.join man.lattice flow_caught_finally flow_uncaught_finally in

        (* Restore old exceptions *)
        Flow.fold (fun acc tk env ->
            match tk with
            | T_py_exception _ -> Flow.add tk env man.lattice acc
            | _ -> acc
          ) flow old_flow |>
        Post.return |> OptionExt.return

      | S_py_raise(Some exp) ->
        debug "Raising %a@\n" pp_expr exp;
        (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) exp flow |>
         bind_full (fun exp flow log cleaners ->
             match exp with
             | None -> Cases.return None flow ~log ~cleaners
             | Some exp ->
             (* match ekind exp with
              * | E_py_object obj -> *)
             assume
               (mk_py_isinstance_builtin exp "BaseException" range)
               man
               ~fthen:(fun true_flow ->
                   debug "True flow, exp is %a@\n" pp_expr exp;
                   let true_flow =  man.exec (mk_block cleaners range) true_flow in
                   let cur = Flow.get T_cur man.lattice true_flow in
                   debug "asking...@\ntrue_flow = %a" (Flow.print man.lattice.print) true_flow;
                   let exc_str, exc_message = man.ask (Types.Structural_types.Q_exn_string_query exp) true_flow in
                   debug "ok@\n";
                   let tk =
                     if List.exists (fun x ->
                         Stdlib.compare x exc_str = 0) !opt_unprecise_exn then
                       let exp = Utils.strip_object exp in
                       mk_py_unprecise_exception exp exc_str (* we don't keep the message for unprecise exns *)
                     else
                       let cs = Flow.get_callstack true_flow in
                       mk_py_exception exp
                         (if exc_message = "" then exc_str else (exc_str ^ ": " ^ exc_message))
                         cs range
                   in
                   let flow' = Flow.add tk cur man.lattice true_flow |>
                               Flow.set T_cur man.lattice.bottom man.lattice
                   in
                   Post.return flow' ~log)
               ~felse:(fun false_flow ->
                   assume
                     (* isclass obj <=> isinstance(obj, type) *)
                     (mk_py_isinstance_builtin exp "type" range)
                     man
                     ~fthen:(fun true_flow ->
                         man.exec {stmt with skind = S_py_raise(Some (mk_py_call exp [] range))} true_flow
                         |> Post.return ~log)
                     ~felse:(fun false_flow ->
                         man.exec (Utils.mk_builtin_raise_msg "TypeError" "exceptions must derive from BaseException" range) false_flow
                         |> Post.return ~log)
                     false_flow
                 )
               flow
           )
        )
        |> OptionExt.return

      | S_py_raise None ->
        panic_at stmt.srange "exceptions: re-raise previous caught exception not supported"

      | _ -> None


    and exec_except (man:('a, unit, 's) man) excpt range (flow:'a flow) : 'a flow =
      debug "exec except on@ @[%a@]" (Flow.print man.lattice.print) flow;
      let flow0 = Flow.set T_cur man.lattice.bottom man.lattice flow in
      debug "flow_cur %a@\n" (Flow.print man.lattice.print) flow;
      let flow0 = Flow.filter (function
          | T_py_exception _ -> fun _ -> false
          | _ -> fun _ -> true) flow0 in
      debug "exec except flow0@ @[%a@]" (Flow.print man.lattice.print) flow0;
      let flow1 =
        match excpt.py_excpt_type with
        (* Default except case: catch all exceptions *)
        | None ->
          (* Add all remaining exceptions env to cur *)
          Flow.fold (fun acc tk env ->
              match tk with
              | T_py_exception _ -> Flow.add T_cur env man.lattice acc
              | _ -> acc)
            flow0 flow

        (* Catch a particular exception *)
        | Some e ->
          (* Add exception that match expression e *)
          Flow.fold (fun acc tk env ->
              match tk with
              | T_py_exception (exn, _, _) ->
                (* Evaluate e in env to check if it corresponds to eaddr *)
                debug "T_cur now matches tk %a@\n" pp_token tk;
                let flow = Flow.set T_cur env man.lattice flow0 in
                let flow' =
                  man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
                  bind_some (fun e flow ->
                      match ekind e with
                      | E_py_object obj ->
                        assume
                          (* if issubclass obj (find_builtin "BaseException") then *)
                          (* issubclass cls1 cls2 <-> ???*)
                          (mk_py_call (mk_py_object (find_builtin "issubclass") range) [e; mk_py_object (find_builtin "BaseException") range] range)
                          man flow
                          ~fthen:(fun true_flow ->
                              assume
                                (mk_py_isinstance exn e range)
                                man
                                ~fthen:(fun true_flow ->
                                    match excpt.py_excpt_name with
                                    | None -> Post.return true_flow
                                    | Some v -> man.exec (mk_assign (mk_var v range) exn range) true_flow |> Post.return)
                                ~felse:(fun false_flow ->
                                    (*if not (isinstance exn obj) then*)
                                    Flow.set T_cur man.lattice.bottom man.lattice false_flow |> Post.return
                                  )
                                true_flow
                            )
                          ~felse:(fun false_flow ->
                              (* else *)
                              man.exec (Utils.mk_builtin_raise_msg "TypeError" "catching classes that do not inherit from BaseException is not allowed" range) flow |> Post.return)
                      | _ -> assert false
                    )
                in
                let flow' = post_to_flow man flow' in
                Flow.fold (fun acc tk env ->
                    match tk with
                    | T_cur | T_py_exception _ -> Flow.add tk env man.lattice acc
                    | _ -> acc
                  ) acc flow'
              | _ -> acc
            ) flow0 flow
      in
      let clean_except_var =
        match excpt.py_excpt_name with
        | None -> mk_block [] (tag_range range "clean_except_var")
        | Some v -> mk_remove_var v (tag_range range "clean_except_var") in
      debug "except flow1 =@ @[%a@]" (Flow.print man.lattice.print) flow1;
      (* Execute exception handler *)
      man.exec excpt.py_excpt_body flow1
      |> man.exec clean_except_var


    and escape_except man excpt range flow =
      debug "escape except";
      let flow0 = Flow.set T_cur man.lattice.bottom man.lattice flow |>
                  Flow.filter (function
                      | T_py_exception _ -> fun _ -> false
                      | _ -> fun _ -> true) in
      match excpt.py_excpt_type with
      | None -> flow0

      | Some e ->
        Flow.fold (fun acc tk env ->
            match tk with
            | T_py_exception (exn, s, k) ->
              (* Evaluate e in env to check if it corresponds to exn *)
              let flow = Flow.set T_cur env man.lattice flow0 in
              let flow' =
                man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
                bind_some (fun e flow ->
                    match ekind e with
                    | E_py_object obj ->
                      assume
                        (mk_py_call (mk_py_object (find_builtin "issubclass") range) [e; mk_py_object (find_builtin "BaseException") range] range)
                        man
                        (* if issubclass obj (find_builtin "BaseException") && not (isinstance exn obj) then
                         *   man.flow.add (TExn exn) env flow *)
                        ~fthen:(fun true_flow ->
                            assume
                              (mk_py_isinstance exn e range)
                              man
                              ~fthen:(fun true_flow -> Post.return true_flow)
                              ~felse:(fun false_flow ->
                                  Flow.add tk env man.lattice false_flow |> Post.return
                                )
                              true_flow)
                        ~felse:(fun false_flow -> Post.return false_flow)
                        flow
                    | _ -> Post.return flow
                  )
              in
              let flow' = post_to_flow man flow' in
              Flow.fold (fun acc tk env ->
                  match tk with
                  | T_py_exception _ -> Flow.add tk env man.lattice acc
                  | _ -> acc
                ) flow0 flow'
            | _ -> acc
          ) flow0 flow


    let ask _ _ _ = None
  end

let () = register_stateless_domain (module Domain)
