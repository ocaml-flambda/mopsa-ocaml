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

    type flow_type = | Caught of string list | Uncaught


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

    let checks = []

    let init _ _ flow = flow
    let eval _ _ _ = None

    let rec exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_py_try(body, excepts, orelse, finally) ->
        let old_flow = flow in
        (* Remove all previous exception flows *)
        let flow0 = Flow.filter (function
            | T_py_exception _ -> fun _ -> false
            | _ -> fun _ -> true) flow in

        (* Execute try body *)
        begin
          man.exec body flow0 >>% fun try_flow ->
          debug "post try flow:@\n  @[%a@]" (format (Flow.print man.lattice.print)) try_flow;
          (* Execute handlers *)
          let flow_caught, flow_uncaught =
            List.fold_left (fun (acc_caught, acc_uncaught) excpt ->
                (* FIXME: I don't get why exec_except and escape_except are separated, and it seems to create more issues than anything. To merge *)
                let caught, uncaught = exec_except man excpt range acc_uncaught in
                debug "caught: %a@.uncaught: %a" (format @@ Flow.print man.lattice.print) caught (format @@ Flow.print man.lattice.print) uncaught;
                Flow.join man.lattice acc_caught caught, uncaught)
              (Flow.bottom_from try_flow, try_flow)  excepts in

          (* Execute else body after removing all exceptions *)
          Flow.filter (function
              | T_py_exception _ -> fun _ -> false
              | _ -> fun _ -> true) try_flow |>
          man.exec orelse >>% fun orelse_flow ->

          let apply_finally finally flow =
            let open Universal.Iterators.Loops in
            let open Universal.Iterators.Interproc.Common in
            Flow.fold (fun acc tk env ->
                match tk with
                | T_cur | T_break | T_continue | T_return _ | T_py_exception _ ->
                   Flow.singleton (Flow.get_ctx acc) T_cur env |>
                     man.exec finally |> post_to_flow man |>
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
          Post.return @@ Flow.fold (fun acc tk env ->
                             match tk with
                             | T_py_exception _ -> Flow.add tk env man.lattice acc
                             | _ -> acc
                           ) flow old_flow
        end |> OptionExt.return

      | S_py_raise(Some exp) ->
        Debug.debug ~channel:"bla" "Raising %a@\n" pp_expr exp;
        (man.eval   exp flow |>
         bind (fun case flow ->
             match case with
             | Empty -> Cases.empty flow
             | NotHandled -> assert false
             | Result(exp,_,cleaners) ->
             assume
               (mk_py_isinstance_builtin exp "BaseException" range)
               man
               ~fthen:(fun true_flow ->
                   debug "True flow, exp is %a@\n" pp_expr exp;
                   (* FIXME: remove cleaners after executing them *)
                   man.exec (mk_block (StmtSet.elements cleaners) range) true_flow >>% fun true_flow ->
                   let cur = Flow.get T_cur man.lattice true_flow in
                   debug "asking...@\ntrue_flow = %a" (format (Flow.print man.lattice.print)) true_flow;
                   let exc_str, exc_message = man.ask (Types.Structural_types.Q_exn_string_query exp) true_flow in
                   debug "ok@\n";
                   let tk =
                     if List.exists (fun x ->
                         Stdlib.compare x exc_str = 0) !opt_unprecise_exn then
                       let exp = Utils.strip_object exp in
                       (* messages are removed for unprecise exns *)
                       mk_py_unprecise_exception exp exc_str
                     else
                       let cs = Flow.get_callstack true_flow in
                       mk_py_exception exp exc_str exc_message
                         ~cs range
                   in
                   let flow' = Flow.add tk cur man.lattice true_flow |>
                               Flow.set T_cur man.lattice.bottom man.lattice
                   in
                   Flow.add_safe_check Alarms.CHK_PY_TYPEERROR exp.erange flow' |>
                   Post.return)
               ~felse:(fun false_flow ->
                   assume
                     (* isclass obj <=> isinstance(obj, type) *)
                     (mk_py_isinstance_builtin exp "type" range)
                     man
                     ~fthen:(fun true_flow ->
                         Flow.add_safe_check Alarms.CHK_PY_TYPEERROR exp.erange true_flow |>
                         man.exec {stmt with skind = S_py_raise(Some (mk_py_call exp [] range))}
                         >>% Post.return)
                     ~felse:(fun false_flow ->
                         man.exec (Utils.mk_builtin_raise_msg "TypeError" "exceptions must derive from BaseException" range) false_flow
                         >>% Post.return)
                     false_flow
                 )
               flow
           )
        )
        |> OptionExt.return

      | S_py_raise None ->
         man.exec (Utils.mk_builtin_raise_msg "RuntimeError" "No active exception to reraise" range) flow
         |> OptionExt.return

      | _ -> None


    and exec_except man excpt range flow =
      (* FIXME: if no excpt variable but there is a raise w/o argument, we should do something *)
      debug "exec except %a on@ @[%a@]" (OptionExt.print pp_expr) excpt.py_excpt_type (format (Flow.print man.lattice.print)) flow;
      let flow0 = Flow.set T_cur man.lattice.bottom man.lattice flow in
      debug "flow_cur %a@\n" (format (Flow.print man.lattice.print)) flow;
      (* flow0 is our base: all the non-exceptional flows *)
      let flow0 = Flow.filter (function
          | T_py_exception _ -> fun _ -> false
          | _ -> fun _ -> true) flow0 in
      debug "exec except flow0@ @[%a@]" (format (Flow.print man.lattice.print)) flow0;
      let except_var =
        match excpt.py_excpt_name with
        | None -> mk_range_attr_var range "artificial_except_var" (T_py None)
        | Some v -> v in
      let cases = match excpt.py_excpt_type with
        (* Default except case: catch all exceptions *)
        | None ->
          (* Add all remaining exceptions env to cur *)
          let flow, exns = Flow.fold (fun (acc, excs) tk env ->
              match tk with
              | T_py_exception (_, exc_name, _, _) ->
                 Flow.add T_cur env man.lattice acc, exc_name :: excs
              | _ -> (acc, excs))
                             (flow0, []) flow in
          Cases.singleton (Caught exns) flow

        (* Catch a particular exception *)
        | Some e ->
           (* Add exception that match expression e *)
           let caught_flow, exns, uncaught_flow =
             Flow.fold (fun (caught, excs, uncaught) tk env ->
                 match tk with
                 | T_py_exception (exn, exc_name, _, _) ->
                    (* Evaluate e in env to check if it corresponds to eaddr *)
                    debug "T_cur now matches tk %a@\n" pp_token tk;
                    let flow = Flow.set T_cur env man.lattice flow0 in

                    let apply_except except flow =
                      assume
                        (mk_py_isinstance exn except range)
                        man
                        ~fthen:(fun flow ->
                          Cases.singleton (Caught [])
                            (Flow.add_safe_check Alarms.CHK_PY_TYPEERROR exn.erange flow |>
                               man.exec (mk_assign (mk_var except_var range) exn range) |> post_to_flow man))
                        ~felse:(fun flow ->
                          Flow.rename T_cur tk man.lattice flow |>
                          Cases.singleton Uncaught)
                        flow in

                    let flow' =
                      (* FIXME: the evaluation of e is performed for all T_py_exception token, but there is no clean way to perform it outside either... in which environment should it be otherwise? This is thus probably unsound if e is a function with side effects and multiple exceptions are non-deterministically raised in the try body. *)
                      man.eval e flow >>$ fun e flow ->
                      assume (mk_py_isinstance_builtin e "tuple" range) man flow
                        ~fthen:(fun flow ->
                          let vars = Objects.Tuple.Domain.var_of_addr (addr_of_object @@ object_of_expr e) in
                          Cases.join_list ~empty:(fun () -> assert false)
                            (List.map (fun v -> apply_except (mk_var v range) flow ) vars))
                        ~felse:(fun flow ->
                          let baseexception_err = fun flow ->
                              man.exec (Utils.mk_builtin_raise_msg "TypeError" "catching classes that do not inherit from BaseException is not allowed" range) flow >>% Cases.empty in
                          assume
                            (mk_py_isinstance_builtin e "type" range) man flow
                            ~fthen:(fun flow ->
                              assume (mk_py_issubclass_builtin_r e "BaseException" range) man flow
                                ~fthen:(fun flow -> apply_except e flow)
                                ~felse:baseexception_err
                            )
                            ~felse:baseexception_err
                        ) in

                    let c, u =
                      Cases.reduce_result (fun exc_typ flow ->
                          match exc_typ with
                          | Caught _ -> flow, Flow.bottom_from flow
                          | Uncaught -> Flow.bottom_from flow, flow)
                        ~join:(fun (c, u) (c', u') -> Flow.join man.lattice c c', Flow.join man.lattice u u')
                        ~meet:(fun (c, u) (c', u') -> Flow.meet man.lattice c c', Flow.meet man.lattice u u')
                        ~bottom:(Flow.bottom_from flow, Flow.bottom_from flow) flow' in
                    c, exc_name :: excs, u
                 | _ -> (caught, excs, uncaught))
               (flow0, [], flow0) flow in
           Cases.join
             (Cases.return (Caught exns) caught_flow)
             (Cases.return Uncaught uncaught_flow) in

      Cases.reduce_result (fun exc_type flow1 ->
          let flow1 = exec_cleaners man (Post.return flow1) |> post_to_flow man in
          match exc_type with
          | Caught caught_excs ->
             let clean_except_var = mk_remove_var except_var (tag_range range "clean_except_var") in
             let except_body =
               (* replace raise without arguments with `raise except_var` *)
               Visitor.map_stmt
                 (fun e -> Keep e)
                 (fun s -> match skind s with
                           | S_py_raise None -> Keep {s with skind=(S_py_raise (Some (mk_var except_var range)))}
                           | _ -> VisitParts s)
                 excpt.py_excpt_body
             in
             debug "except flow1 =@ @[%a@]" (format (Flow.print man.lattice.print)) flow1;
             let flow = man.exec except_body flow1
             >>% man.exec clean_except_var >>%
               (fun flow ->
                 Post.return
                   (if not @@ man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then
                      List.fold_left (fun flow name ->
                          Flow.add_safe_check (Alarms.py_name_to_check name) except_body.srange flow) flow caught_excs
                    else
                      flow)) in
             post_to_flow man flow, Flow.bottom_from flow1
          | Uncaught ->

             Flow.bottom_from flow1, flow1
        )
        ~join:(fun (c, u) (c', u') -> Flow.join man.lattice c c', Flow.join man.lattice u u')
        ~meet:(fun (c, u) (c', u') -> Flow.meet man.lattice c c', Flow.meet man.lattice u u')
        ~bottom:(Flow.bottom_from flow, Flow.bottom_from flow)
        cases


    let ask _ _ _ = None
    let print_expr _ _ _ _ = ()

  end

let () = register_stateless_domain (module Domain)
