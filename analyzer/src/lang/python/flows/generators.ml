(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2020 The MOPSA Project.                               *)
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

(** Abstraction of generators *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast



type token +=
  | T_py_gen_start of py_object
  (** Initial generator flows *)

  | T_py_gen_next of py_object * range
  (** Flows starting from a call to __next__ that should resume
      execution at the given location point *)

  | T_py_gen_yield of py_object * expr * range
  (** Flow starting from a yield expression and suspended until
      reaching the calling next statement *)

  | T_py_gen_stop of py_object
  (** Flows reaching the end of the generator *)

let () = register_token {
             compare=(fun next tk1 tk2 ->
               match tk1, tk2 with
               | T_py_gen_start(g1), T_py_gen_start(g2) -> compare_py_object g1 g2
               | T_py_gen_next(g1, r1), T_py_gen_next(g2, r2) ->
                  Compare.compose [
                      (fun () -> compare_py_object g1 g2);
                      (fun () -> compare_range r1 r2);
                    ]
               | T_py_gen_yield(g1, _, r1), T_py_gen_yield(g2, _, r2) ->
                  Compare.compose [
                      (fun () -> compare_py_object g1 g2);
                      (fun () -> compare_range r1 r2);
                    ]
               | T_py_gen_stop(g1), T_py_gen_stop(g2) -> compare_py_object g1 g2
               | _ -> next tk1 tk2
             );
             print=(fun next fmt -> function
                     | T_py_gen_start(gen) -> Format.fprintf fmt "gstart(%a)" Pp.pp_py_object gen
                     | T_py_gen_next(gen, r) -> Format.fprintf fmt "gnext(%a) -> %a" Pp.pp_py_object gen pp_range r
                     | T_py_gen_yield(gen, e, r) -> Format.fprintf fmt "gyield(%a) <- %a" Pp.pp_py_object gen pp_range r
                     | T_py_gen_stop(gen) -> Format.fprintf fmt "gstop(%a)" Pp.pp_py_object gen
                     | tk -> next fmt tk
                   );
             }


let mk_framed_var v obj =
  mk_attr_var v (Format.asprintf "%a" Universal.Ast.pp_addr (fst obj)) v.vtyp

(** The current generator being analyzed is stored in the context. *)
module GenKey =
  Context.GenContextKey(
      struct
        type 'a t = py_object
        let print pp fmt (a, oe) = Format.fprintf fmt "Generator:(%a, %a)" pp_addr a (OptionExt.print pp_expr) oe
      end
    )

let generator_key = GenKey.key

type addr_kind += A_py_generator of py_fundec

let () = register_addr_kind {
             print = (fun default fmt a ->
               match a with
               | A_py_generator f -> Format.fprintf fmt "Generator{%a}" pp_var f.py_func_var
               | _ -> default fmt a);
             compare = (fun default a1 a2 ->
               match a1, a2 with
               | A_py_generator g1, A_py_generator g2 ->
                  Compare.compose
                    [
                      (fun () -> compare_var g1.py_func_var g2.py_func_var);
                      (fun () -> Compare.list compare_var g1.py_func_parameters g2.py_func_parameters);
                      (fun () -> Compare.list (Compare.option compare_expr) g1.py_func_defaults g2.py_func_defaults);
                      (fun () -> compare_stmt g1.py_func_body g2.py_func_body);
                      (fun () -> Stdlib.compare g1.py_func_is_generator g2.py_func_is_generator);
                      (fun () -> Compare.list compare_expr g1.py_func_decors g2.py_func_decors);
                      (fun () -> compare_range g1.py_func_range g2.py_func_range);
                    ]
               | _ -> default a1 a2);
           };
         register_addr_kind_nominal_type (fun default ak ->
             match ak with
             | A_py_generator _ -> "generator"
             | _ -> default ak);
         register_addr_kind_structural_type (fun default ak s ->
             match ak with
             | A_py_generator _ -> false
             | _ -> default ak s);

module Domain = struct
  include GenStatelessDomainId(struct
              let name = "python.flows.generators"
            end)

  let alarms = []

  let eval exp (man: ('a, unit) man) (flow: 'a flow) =
    let range = erange exp in
    match ekind exp with
    (* E⟦ g(e1, e2, ...) | is_generator(g) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_user func)}, _)}, args, [])
      when func.py_func_is_generator
      ->
       OptionExt.return
         (Cases.bind_list args man.eval flow >>$
            fun el flow ->
            eval_alloc man (A_py_generator func) range flow >>$
              fun gen_aaddr flow ->
              let obj = (gen_aaddr, None) in

              let flow0 = flow in
              (* Assign arguments to parameters in a new flow *)

              (* FIXME: default arguments in generators are not supported yet *)
              if List.length args <> List.length func.py_func_parameters then
                panic_at range "generators: only calls with correct number of arguments is supported"
              else
                (* Change all parameters into framed variables *)
                let params = List.map (fun v -> mk_framed_var v obj) func.py_func_parameters in

                (* Perform assignments to arguments *)
                let flow1 = List.fold_left (fun flow (v, e) ->
                                man.exec (mk_assign (mk_var v range) e range)  flow |> post_to_flow man
                              ) flow (List.combine params args)
                in

                (* Save the projected cur env in the initial flow of the generator *)
                let cur' = man.exec (mk_project_vars params range) flow1 |> post_to_flow man |>
                             Flow.get T_cur man.lattice
                in
                let flow2 = Flow.add (T_py_gen_start obj) cur' man.lattice flow0 in
                Eval.singleton (mk_py_object obj range) flow2
         )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("generator.__iter__" as f, _))}, _)}, args, []) ->
       (* FIXME: error message if argument is not a generator is incorrect *)
       Utils.check_instances f man flow range args ["generator"]
         (fun args flow ->
           Eval.singleton (List.hd args) flow
         )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("generator.__next__" as f, _))}, _)}, args, []) ->
       (* FIXME: error message if argument is not a generator is incorrect *)
       Utils.check_instances f man flow range args ["generator"]
         (fun args flow ->
           let self = match ekind @@ List.hd args with
             | E_py_object (a, oe) -> (a, oe)
             | _  -> assert false in
           let func = match akind @@ fst self with
             | A_py_generator g -> g
             | _ -> assert false in

           (* Keep the input cur environment *)
           let cur = Flow.get T_cur man.lattice flow in

           debug "flow = %a" (Flow.print man.lattice.print) flow;

           let ctx = Flow.get_ctx flow in
           (* Compute the next tokens *)
           let flow1 = Flow.fold (fun acc tk env ->
                           match tk with
                           | T_cur -> acc
                           | T_py_gen_start(g) when compare_addr (fst g) (fst self) = 0 ->
                              Flow.add T_cur env man.lattice acc
                           | T_py_gen_start _ -> acc

                           | T_py_gen_next(g, r) when compare_addr (fst g) (fst self) = 0 ->
                              Flow.add tk env man.lattice acc
                           | T_py_gen_next _ ->
                              acc

                           | T_py_gen_yield(g, _, r) when compare_addr (fst g) (fst self) = 0 ->
                              Flow.add T_cur env man.lattice acc
                           | T_py_gen_yield _ -> acc

                           | T_py_gen_stop(g) -> acc (* this case is handled later *)
                           | Universal.Iterators.Interproc.Common.T_return _ -> acc

                           | tk ->
                              Flow.add T_cur env man.lattice acc
                         ) (Flow.bottom_from flow) flow
           in
           debug "flow1 = %a" (Flow.print man.lattice.print) flow1;

           (* Filter flow by location reachability *)
           (* FIXME: add relational counter *)
           let flow2 = Flow.map (fun tk env ->
                           match tk with
                           | T_cur -> man.lattice.join ctx env cur
                           | T_py_gen_next(g, r) -> man.lattice.join ctx env cur
                           | _ -> env
                         ) flow1
           in
           debug "flow2 = %a" (Flow.print man.lattice.print) flow2;

           (* Modify the body of the generator by changing local variables into framed variables *)
           let is_local v = List.exists (fun v' -> compare_var v v' = 0) (func.py_func_locals @ func.py_func_parameters) in
           let body' = Visitor.map_stmt
                         (fun expr ->
                           match ekind expr with
                           | E_var (v, m) when is_local v -> Keep {expr with ekind = E_var (mk_framed_var v self, m)}
                           | _ -> VisitParts expr
                         )
                         (fun stmt -> VisitParts stmt)
                         func.py_func_body
           in
           debug "body after variable renaming:@\n @[%a@]" pp_stmt body';
           let locals' = List.map (fun v -> mk_framed_var v self) (func.py_func_locals @ func.py_func_parameters) in

           (* Execute the body statement *)
           let flow2 = Flow.set_ctx (Context.add_ctx Universal.Iterators.Interproc.Common.return_key (Universal.Iterators.Interproc.Common.mk_return_var exp) (Flow.get_ctx flow2)) flow2 in
           let flow2 = Flow.set_ctx (Context.add_ctx generator_key self (Flow.get_ctx flow2)) flow2 in
           let flow3 = man.exec body' flow2 |> post_to_flow man in

           debug "flow3 = %a" (Flow.print man.lattice.print) flow3;

           (* Add the input stop flows  *)
           let flow3 = Flow.fold (fun acc tk env ->
                           match tk with
                           | T_py_gen_stop(g) when compare_addr (fst g) (fst self) = 0 ->
                              Flow.add tk env man.lattice acc
                           | _ -> acc
                         ) flow3 flow
           in


           (* Restore the input flows *)
           let flow4 = Flow.fold (fun acc tk env ->
                           match tk with
                           | Universal.Iterators.Interproc.Common.T_return _ ->
                              Flow.add tk env man.lattice acc
                           | T_py_gen_start(g) when compare_addr (fst g) (fst self) <> 0 ->
                              Flow.add tk env man.lattice acc
                           | T_py_gen_yield(g, _, _) when compare_addr (fst g) (fst self) <> 0 ->
                              Flow.add tk env man.lattice acc
                           | T_py_gen_stop(g) when compare_addr (fst g) (fst self) <> 0 ->
                              Flow.add tk env man.lattice acc
                           | _ -> acc
                         ) (Flow.bottom_from flow) flow
           in

           (* Process the resulting yield, return and exception flows *)
           let cases = Flow.fold (fun acc tk env ->
               match tk with
               | T_py_gen_yield(g, e, r) when compare_addr (fst g) (fst self) = 0 ->
                  (* Assign the yielded value to a temporary return variable *)
                  let tmp = mktmp ~typ:(T_py None) () in
                  debug "putting yielded value into %a" pp_var tmp;
                  let flow = Flow.set T_cur env man.lattice flow4 |>
                               man.exec (mk_assign (mk_var tmp range) e range) |>
                               post_to_flow man
                  in
                  (* Clean the cur environment by removing the generator local variables *)
                  let flow = List.fold_left (fun acc v ->
                                 man.exec (mk_remove_var v range) acc |> post_to_flow man
                               ) flow locals'
                  in
                  (* Clean the yield frame by projecting on the generator local variables *)
                  (* let cur' = Flow.set T_cur env man.lattice flow4 |>
                   *              man.exec (mk_project_vars locals' range) |>
                   *              post_to_flow man |>
                   *              Flow.get T_cur man.lattice
                   * in *)
                  let r = flow |> (* Flow.set (T_py_gen_next(g, r)) cur' man.lattice flow |>*)
                    man.eval (mk_var tmp range) |>
                            Cases.add_cleaners [mk_remove_var tmp range] in
                  r :: acc

               | T_py_gen_next _ ->
                  let flow = Flow.set T_cur env man.lattice flow4 in
                  (* Clean the cur environment by removing the generator local variables *)
                  let flow = List.fold_left (fun acc v ->
                                 man.exec (mk_remove_var v range) acc |> post_to_flow man
                               ) flow locals'
                  in
                  (* Clean the yield frame by projecting on the generator local variables *)
                  let cur' = Flow.set T_cur env man.lattice flow4 |>
                               man.exec (mk_project_vars locals' range) |>
                               post_to_flow man |>
                               Flow.get T_cur man.lattice in
                  let r = Eval.empty (Flow.add tk cur' man.lattice flow) in
                  r :: acc

               | Alarms.T_py_exception (obj, name, kind) ->
                  (* Save env in the token T_py_gen_stop and re-raise the exception *)
                  let r = Flow.add (T_py_gen_stop(self)) env man.lattice flow4 |>
                    Flow.set (Alarms.T_py_exception (obj, name, kind)) env man.lattice |>
                            Eval.empty in
                  r :: acc

               | T_py_gen_stop _
                 | Universal.Iterators.Interproc.Common.T_return _ ->
                  (* Save env in the token T_py_gen_stop and raise a StopIteration exception *)
                  let r = Flow.add (T_py_gen_stop(self)) env man.lattice flow4 |>
                    Flow.set T_cur env man.lattice |>
                    man.exec (Utils.mk_builtin_raise "StopIteration" range) |>
                    post_to_flow man |>
                            Eval.empty in
                  r :: acc

               | _ -> acc
                         ) [] flow3 in
           Eval.join_list ~empty:(fun () -> assert false) cases


         )
       |> OptionExt.return


    (* E⟦ x for x in g | isinstance(g, generator) ⟧ *)
    | E_py_generator_comprehension _ ->
      panic_at range "Generator comprehension not supported"

    | _ -> None

  let init _ _ flow = flow
  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_expression {ekind = E_py_yield e} ->
       let ctx = Flow.get_ctx flow  in
       let g = Context.find_ctx generator_key ctx in
       let flow = Flow.fold (fun acc tk env ->
                      match tk with
                      | T_cur ->
                         let acc = Flow.add (T_py_gen_yield(g, e, range)) env man.lattice acc in
                         Flow.add (T_py_gen_next (g, range)) env man.lattice acc
                      | T_py_gen_next(g, r) when compare_range r range = 0 ->
                         debug "moving a gen_next to cur";
                         Flow.add T_cur env man.lattice acc
                      | _ -> Flow.add tk env man.lattice acc
                    ) (Flow.bottom_from flow) flow
       in
       OptionExt.return @@ Post.return flow

    (* FIXME: renaming of framed variables, when the generator address is renamed *)
    | _ -> None

  let ask _ _ _ = None
  let checks = []
end

let () = register_stateless_domain (module Domain)
