(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2018-2019 The MOPSA Project.                               *)
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

(* A general smashing abstraction for Python lists, (hopefully)
   irrelevant of the value/type domain *)

(* currently, lists are smashed into one variable abstracting all
   its elements. To avoid allocating a new variable each time
   (especially during loops), we allocate them only if there has
   been not other allocation at the same point. This is quite
   similar to the recency abstraction, and is probably not optimal
*)

(* TODO: add length for lists and position for iterator? *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast
open Callstack
open Data_container_utils

type addr_kind +=
  | A_py_list of Rangeset.t (* rangeset because a list may be a summarization of list allocated at different ranges *)
  | A_py_iterator of string (* iterator kind (list_iterator, ...) *) * addr list  (* addr of the containers iterated on *) * int option (* potential position in the iterator *)


let () =
  register_join_akind (fun default ak1 ak2 ->
      match ak1, ak2 with
      | A_py_list r1, A_py_list r2 -> A_py_list (Rangeset.union r1 r2)
      | _ -> default ak1 ak2);
  register_is_data_container (fun default ak -> match ak with
      | A_py_list _ -> true
      | _ -> default ak)


let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_list r -> fprintf fmt "list[%a]" (fun fmt -> Rangeset.iter (fun ra -> pp_range fmt ra)) r
          | A_py_iterator (s, addr, d) ->
            begin match d with
            | None -> fprintf fmt "%s[%a]" s (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_addr) addr
            | Some d -> fprintf fmt "%s[%a, %d]" s (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_addr) addr d end
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_list v1, A_py_list v2 -> Rangeset.compare v1 v2
          | A_py_iterator (s1, a1, d1), A_py_iterator (s2, a2, d2) ->
            Compare.compose [
              (fun () -> Stdlib.compare s1 s2);
              (fun () -> Compare.list compare_addr a1 a2);
              (fun () -> Compare.option Stdlib.compare d1 d2);
            ]
          | _ -> default a1 a2);})


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.list"
    end)

  let interface = {
    iexec = {provides = [Zone.Z_py_obj]; uses = [Zone.Z_py_obj; Universal.Zone.Z_u_heap]};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any]}
  }

  let alarms = []

  let init (prog:program) man flow =
    flow

  let var_of_addr a = match akind a with
    | A_py_list _ -> mk_addr_attr a "list" T_any
    | _ -> assert false

  let var_of_eobj e = match ekind e with
    | E_py_object (a, _) -> var_of_addr a
    | _ -> assert false

  let addr_of_expr exp = match ekind exp with
    | E_addr a -> a
    | _ -> Exceptions.panic "%a@\n" pp_expr exp

  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_list ls ->
      debug "Skipping list.__new__, list.__init__ for now@\n";

      let addr_list = mk_alloc_addr (A_py_list (Rangeset.singleton range)) range in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
      Eval.bind (fun eaddr_list flow ->
          let addr_list = addr_of_expr eaddr_list in
          let els_var = var_of_addr addr_list in
          (* let flow = man.exec (mk_add_var els_var range) flow in *)
          let flow = List.fold_left (fun acc el ->
              let stmt = mk_assign (mk_var ~mode:(Some WEAK) els_var range) el range in
              (* debug "fold_left %a@\n" pp_stmt stmt; *)
              man.exec ~zone:Zone.Z_py stmt acc) flow ls in
          Eval.singleton (mk_py_object (addr_list, None) range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__getitem__", _))}, _)}, [list; index], []) ->
      bind_list [list; index] man.eval flow |>
      bind_some (fun exprs flow ->
          let list, index = match exprs with [l; i] -> l, i | _ -> assert false in
          assume (mk_py_isinstance_builtin list "list" range) man flow
            ~fthen:(fun flow ->
                let var_els = var_of_eobj list in
                assume (mk_py_isinstance_builtin index "int" range) man flow
                  ~fthen:(fun flow ->
                      let indexerror_f = man.exec (Utils.mk_builtin_raise "IndexError" range) flow in
                      let indexerror = Eval.empty_singleton indexerror_f in
                      let flow = Flow.copy_ctx indexerror_f flow in
                      let evals = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var var_els range) flow in
                      Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (evals :: Eval.copy_ctx evals indexerror :: [])
                    )
                  ~felse:(fun flow ->
                      assume (mk_py_isinstance_builtin index "slice" range) man flow
                        ~fthen:(fun flow ->
                            let addr_list = mk_alloc_addr (A_py_list (Rangeset.singleton range)) range in
                            man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
                            Eval.bind (fun eaddr_list flow ->
                                let addr_list = addr_of_expr eaddr_list in
                                let slicedlist_var = var_of_addr addr_list in
                                let flow = man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:(Some WEAK) slicedlist_var range) (mk_var ~mode:(Some WEAK) var_els range) range) flow in
                                Eval.singleton (mk_py_object (addr_list, None) range) flow
                              )
                          )
                        ~felse:(fun flow ->
                            Format.fprintf Format.str_formatter "list indices must be integers or slices, not %a" pp_addr_kind (akind @@ fst @@ object_of_expr index);
                            man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
                            Eval.empty_singleton
                          )
                    )
              )
            ~felse:(fun flow ->
                Format.fprintf Format.str_formatter "descriptor '__getitem__' requires a 'list' object but received %a" pp_addr_kind (akind @@ fst @@ object_of_expr list);
                man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
                Eval.empty_singleton
              )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__add__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"; "list"]
        (fun args flow ->
           let listl, listr = match args with [l; r] -> l, r | _ -> assert false in
           let elsl_var = var_of_eobj listl in
           let elsr_var = var_of_eobj listr in
           (* First, allocate new addr for the list, and new addr for the list elements *)
           (* Then assign the el addr to both addresses above *)
           let addr_list = mk_alloc_addr (A_py_list (Rangeset.singleton range)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
           Eval.bind (fun list_addr flow ->
               let alist_addr = addr_of_expr list_addr in
               let els_res_var = var_of_addr alist_addr in
               let flow = List.fold_left (fun acc el ->
                   man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:(Some WEAK) els_res_var range) el range) acc)
                   flow [mk_var ~mode:(Some WEAK) elsl_var range;
                         mk_var ~mode:(Some WEAK) elsr_var range] in
               Eval.singleton (mk_py_object (alist_addr, None) range) flow
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__iadd__", _))}, _)}, args, []) ->
      (* let's rewrite list.__iadd__(s, t) into list.extend(s, t) && return s *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object (find_builtin "list.extend") range) args range) flow
      |> Eval.bind (fun nonety flow ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (List.hd args) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__setitem__" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:2 f man flow range args
        ["list"]
        (fun args flow ->
           let list, index, value = match args with | [l; i; v] -> l, i, v | _ -> assert false in
           assume (mk_py_isinstance_builtin index "int" range) man flow
             ~fthen:(fun flow ->
                 let var_els = var_of_eobj list in
                 let indexerror_f = man.exec (Utils.mk_builtin_raise_msg "IndexError" "list assignment index out of range" range) flow in
                 let flow = Flow.copy_ctx indexerror_f flow in

                 let assignment_f = man.exec (mk_assign (mk_var ~mode:(Some WEAK) var_els range) value range) flow in
                 let indexerror_f = Flow.copy_ctx assignment_f indexerror_f in

                 let assignment = man.eval (mk_py_none range) assignment_f in
                 let indexerror = Eval.empty_singleton indexerror_f in
                 Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (assignment :: (Eval.copy_ctx assignment indexerror) ::[])
               )
             ~felse:(fun flow ->
                 assume (mk_py_isinstance_builtin index "slice" range) man flow
                   ~fthen:(fun flow ->
                       man.eval (mk_py_call (mk_py_object (find_builtin "list.extend") range) [list; value] range) flow
                     )
                   ~felse:(fun flow ->
                       Format.fprintf Format.str_formatter "list indices must be integers or slices, not %a" pp_addr_kind (akind @@ fst @@ object_of_expr index);
                       man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
                       Eval.empty_singleton
                     )
               )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, args, [])
      when is_compare_op_fun "list" f ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args ["list"]
        (fun eargs flow ->
           let e1, e2 = match args with [l; r] -> l, r | _ -> assert false in
           assume (mk_py_isinstance_builtin e2 "list" range) man flow
             ~fthen:(man.eval (mk_py_top T_bool range))
             ~felse:(fun flow ->
                 let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                 man.eval expr flow)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__mul__" as f, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__rmul__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"; "int"]
        (fun args flow ->
           let list, int = match args with [l; r] -> l, r | _ -> assert false in
           let els_list = var_of_eobj list in
           let addr_list = mk_alloc_addr (A_py_list (Rangeset.singleton range)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
           Eval.bind (fun eaddr_list flow ->
               let addr_list = addr_of_expr eaddr_list in
               let els_var = var_of_addr addr_list in
               let flow = man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:(Some WEAK) els_var range) (mk_var ~mode:(Some WEAK) els_list range) range) flow in
               Eval.singleton (mk_py_object (addr_list, None) range) flow
             )
        )
      |> OptionExt.return

    | E_py_object ({addr_kind = A_py_list _}, e) ->
      Eval.singleton exp flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.append" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["list"]
        (fun args flow ->
           let list, element = match args with | [l; e] -> l, e | _ -> assert false in
           debug "list: %a@\nelement = %a@\nflow = %a@\n" pp_expr list pp_expr element (Flow.print man.lattice.print) flow;
           let var_els = var_of_eobj list in
           man.exec (mk_assign (mk_var var_els ~mode:(Some WEAK) range) element range) flow |>
           man.eval (mk_py_none range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.insert" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args
        ["list"; "int"]
        (fun args flow ->
           let list, index, element = match args with | [l; i; e] -> l, i, e | _ -> assert false in
           let var_els = var_of_eobj list in
           man.exec (mk_assign (mk_var var_els ~mode:(Some WEAK) range) element range) flow |>
           man.eval (mk_py_none range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__new__", _))}, _)}, cls::args, []) ->
      Utils.new_wrapper man range flow "list" cls
        ~fthennew:(man.eval (mk_expr (E_py_list []) range))

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__init__" as f, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.extend" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args
        ["list"]
        (fun eargs flow ->
           (* FIXME: check manually (with ekind list) that we have a list or list_iterator as we are in the same abstract domain? *)
           let list, other = match eargs with e1::e2::[] -> e1, e2 | _ -> assert false in
           let var_els = var_of_eobj list in
           assume (mk_py_isinstance_builtin other "list" range) man flow
             ~fthen:(fun flow ->
                 let var_sndels = var_of_eobj other in
                 man.exec (mk_assign (mk_var var_els ~mode:(Some WEAK) range) (mk_var var_sndels ~mode:(Some WEAK) range) range) flow |>
                 man.eval (mk_py_none range)
               )
             ~felse:(fun flow ->
                 assume (mk_py_isinstance_builtin other "range" range) man flow
                   ~fthen:(fun flow ->
                       (* TODO: more precision on top (for range) *)
                       man.exec (mk_assign (mk_var var_els ~mode:(Some WEAK) range) (mk_py_top T_int range) range) flow  |>
                       man.eval (mk_py_none range)
                     )
                   (* TODO: if object has iter field call it and then call next *)
                   ~felse:(fun flow ->
                       assume (mk_py_isinstance_builtin other "list_reverseiterator" range) man flow
                         ~fthen:(fun flow ->
                             let var_sndels = match ekind other with
                               | E_py_object ({addr_kind = A_py_iterator (_, [{addr_kind = A_py_list _} as a], _)}, _) -> var_of_addr a
                               | _ -> assert false in
                             man.exec (mk_assign (mk_var var_els ~mode:(Some WEAK) range) (mk_var var_sndels ~mode:(Some WEAK) range) range) flow |>
                             man.eval (mk_py_none range)
                           )
                         ~felse:(fun flow ->
                             assume (mk_py_isinstance_builtin other "zip" range) man flow
                               ~fthen:(fun flow ->
                                   let var_l1, var_l2 = match ekind other with
                                     | E_py_object ({addr_kind = A_py_iterator (_, [{addr_kind = A_py_list _} as a1; {addr_kind = A_py_list _} as a2], _)}, _) -> var_of_addr a1, var_of_addr a2
                                     | _ -> assert false in
                                   man.exec (mk_assign (mk_var var_els ~mode:(Some WEAK) range) (mk_expr (E_py_tuple [mk_var var_l1 ~mode:(Some WEAK) range; mk_var var_l2 ~mode:(Some WEAK) range]) range) range) flow |>
                                   man.eval (mk_py_none range)
                                 )
                               ~felse:(fun flow ->
                                   Format.fprintf Format.str_formatter "%a is not iterable" pp_expr list;
                                   man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |> Eval.empty_singleton)
                                 )
                     )
               )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.count" as f, _))}, _)}, args, []) ->
      (* TODO: something smarter depending on the occurence of \gamma(element) in \gamma(list elements) ? *)
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["list"]
        (fun _ flow -> man.eval (mk_py_top T_int range) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.clear" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["list"]
        (fun args flow ->
           let list = List.hd args in
           let var_els = var_of_eobj list in
           man.exec (mk_remove_var var_els range) flow |>
           man.eval (mk_py_none range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.index" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args
        ["list"]
        (fun args flow ->
           Format.fprintf Format.str_formatter "%a is not in list" pp_expr (List.hd @@ List.tl args);
           let eval_verror_f = man.exec (Utils.mk_builtin_raise_msg "ValueError" (Format.flush_str_formatter ()) range) flow in
           let flow = Flow.copy_ctx eval_verror_f flow in
           let eval_verror = Eval.empty_singleton eval_verror_f in
           let eval_res = man.eval (mk_py_top T_int range) flow in
           Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (eval_res :: eval_verror :: []))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.pop", _))}, _)} as call, [arg], []) ->
      let args' = arg :: (mk_constant T_int (C_int (Z.of_int (-1))) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.pop" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"; "int"]
        (fun args flow ->
           let list = match args with l::_::[] -> l | _ -> assert false in
           let var_els = var_of_eobj list in
           let eval_indexerror = man.exec (Utils.mk_builtin_raise_msg "IndexError" "pop from empty list" range) flow
                                 |> Eval.empty_singleton in
           let eval_el = man.eval (mk_var ~mode:(Some WEAK) var_els range) flow in
           Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Eval.copy_ctx eval_indexerror eval_el :: eval_indexerror :: [])
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.remove" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["list"]
        (fun args flow ->
           let eval_verror_f = man.exec (Utils.mk_builtin_raise_msg "ValueError" "list.remove(x): x not in list" range) flow in
           let eval_verror = Eval.empty_singleton eval_verror_f in
           let flow = Flow.copy_ctx eval_verror_f flow in
           let eval_none = man.eval (mk_py_none range) flow in
           Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (eval_none :: eval_verror :: [])
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.reverse" as f, _))}, _)}, args, kwargs)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.sort" as f, _))}, _)}, args, kwargs) ->
      if kwargs <> [] then warn_at range "list.sort/reverse kwargs ignored";
      Utils.check_instances f man flow range args
        ["list"]
        (fun _ flow -> man.eval (mk_py_none range) flow)
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"]
        (fun args flow ->
           let list = match args with | [l] -> l | _ -> assert false in
           let list_addr = match ekind list with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("list_iterator", [list_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_reverseiterator.__next__" as s, _))}, _)}, [iterator], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_iterator.__next__" as s, _))}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      let it_name = String.sub s 0 (String.index s '.') in
      man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun iterator it_flow ->
          let list_addr = match ekind iterator with
            | E_py_object ({addr_kind = A_py_iterator (s, [a], _)}, _) when s = it_name -> a
            | _ -> assert false in
          let var_els = var_of_addr list_addr in
          let els = man.eval (mk_var var_els ~mode:(Some WEAK) range) it_flow in
          debug "Result = %a" (Cases.print (fun fmt e flow -> Format.fprintf fmt "%a@\n%a@\n@\n" (OptionExt.print pp_expr) e (Flow.print man.lattice.print) flow)) els;
          OptionExt.none_to_exn @@ Cases.bind_opt
            (fun oels flow ->
               let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) it_flow |> Eval.empty_singleton in
               Some (match oels with
                   | None ->
                     stopiteration
                   | Some e ->
                     Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) [Eval.copy_ctx stopiteration (Eval.singleton e flow) ; stopiteration ]
                 )
            )
            els
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_reverseiterator.__iter__", _))}, _)}, [iterator], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_iterator.__iter__", _))}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__reversed__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"]
        (fun args flow ->
           let list = match args with | [l] -> l | _ -> assert false in
           let list_addr = addr_of_object (object_of_expr list) in
           let a = mk_alloc_addr (A_py_iterator ("list_reverseiterator", [list_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"]
        (fun args flow ->
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__contains__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args ["list"]
        (fun args flow ->
           man.eval (mk_py_top T_bool range) flow)
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("math.fsum", _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some (fun args flow ->
          if List.length args = 1 then
            let in_ty = List.hd args in
            assume (mk_py_isinstance_builtin in_ty "list" range) man flow
              ~fthen:(fun flow ->
                  (* FIXME: we're assuming that we use the list abstraction *)
                  let var_els_in_ty = var_of_eobj in_ty in
                  assume (mk_py_isinstance_builtin (mk_var ~mode:(Some WEAK) var_els_in_ty range) "float" range) man flow
                    ~fthen:(man.eval (mk_py_top (T_float F_DOUBLE) range))
                    ~felse:(fun flow ->
                        man.exec (Utils.mk_builtin_raise_msg "TypeError" "must be real number" range) flow |>
                        Eval.empty_singleton
                      )
                )
              ~felse:(fun flow ->
                  Format.fprintf Format.str_formatter "%a is not iterable" pp_expr in_ty;
                  man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
                  Eval.empty_singleton
                )
          else
            let () = Format.fprintf Format.str_formatter "fsum() takes exactly one argument (%d given)" (List.length args) in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
            Eval.empty_singleton
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("enumerate.__new__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        (* FIXME: first argument should be subclass of enumerate *)
        ["type"; "list"]
        (fun args flow ->
           let list = match args with | [_; l] -> l | _ -> assert false in
           let list_addr = match ekind list with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("enumerate", [list_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("enumerate.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["enumerate"]
        (fun args flow ->
           Eval.singleton (List.hd args) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("enumerate.__next__", _))}, _)}, [iterator], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun iterator flow ->
          let list_addr = match ekind iterator with
            | E_py_object ({addr_kind = A_py_iterator (s, [a], _)}, _) when s = "enumerate" -> a
            | _ -> Exceptions.panic "%a@\n" pp_expr iterator in
          let var_els = var_of_addr list_addr in
          let els = man.eval (mk_expr (E_py_tuple [mk_top T_int range;
                                                   mk_var var_els ~mode:(Some WEAK) range]) range) flow in
          let flow = Flow.set_ctx (Eval.get_ctx els) flow in
          let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
          Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Eval.copy_ctx stopiteration els::stopiteration::[])
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("zip.__new__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        (* FIXME: first argument should be subclass of enumerate *)
        ["type"; "list"; "list"]
        (fun args flow ->
           let list1, list2 = match args with | [_; l1; l2] -> l1, l2 | _ -> assert false in
           let list1_addr = match ekind list1 with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let list2_addr = match ekind list2 with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("zip", [list1_addr; list2_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with
                 | E_addr a -> a
                 | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("zip.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["zip"]
        (fun args flow ->
           Eval.singleton (List.hd args) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("zip.__next__", _))}, _)}, [iterator], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun iterator flow ->
          let list1_addr, list2_addr = match ekind iterator with
            | E_py_object ({addr_kind = A_py_iterator (s, [a1; a2], _)}, _) when s = "zip" -> a1, a2
            | _ -> Exceptions.panic "%a@\n" pp_expr iterator in
          let var_els1 = var_of_addr list1_addr in
          let var_els2 = var_of_addr list2_addr in
          let els = man.eval (mk_expr (E_py_tuple [mk_var var_els1 ~mode:(Some WEAK) range;
                                                   mk_var var_els2 ~mode:(Some WEAK) range]) range) flow in
          let flow = Flow.set_ctx (Eval.get_ctx els) flow in
          let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
          Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Eval.copy_ctx stopiteration els::stopiteration::[])
        )
      |> OptionExt.return

    (* the last case of str.split uses this list abstraction so every case is here... *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.split", _))}, _)} as call, [str], []) ->
      (* rewrite into str.split(str, " ", -1) *)
      let args' = (mk_constant T_string (C_string " ") range) :: (mk_constant T_int (C_int (Z.of_int 1)) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, str :: args', [])} flow
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.split", _))}, _)} as call , [str; split], []) ->
      (* rewrite into str.split(str, split, -1) *)
      let args' = (mk_constant T_int (C_int (Z.of_int 1)) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, str :: split :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.split" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["str"; "str"; "int"]
        (fun eargs flow ->
           (* FIXME: notok, as one strong element. Fixed by adding to tops, but terrible *)
           man.eval (mk_expr (E_py_list [mk_py_top T_string range; mk_py_top T_string range]) range) flow
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.split", _))}, _)} as call, [str], []) ->
      (* rewrite into str.split(str, " ", -1) *)
      let args' = (mk_py_top T_py_bytes range) :: (mk_constant T_int (C_int (Z.of_int 1)) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, str :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.split", _))}, _)} as call , [str; split], []) ->
      (* rewrite into str.split(str, split, -1) *)
      let args' = (mk_constant T_int (C_int (Z.of_int 1)) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, str :: split :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.split" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["bytes"; "bytes"; "int"]
        (fun eargs flow ->
           (* FIXME: notok, as one strong element. Fixed by adding to tops, but terrible *)
           man.eval (mk_expr (E_py_list [mk_py_top T_py_bytes range; mk_py_top T_py_bytes range]) range) flow
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.splitlines" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["str"]
        (fun eargs flow ->
           man.eval (mk_expr (E_py_list [mk_py_top T_string range]) range) flow
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dir" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args []
        (fun _ -> man.eval (mk_expr (E_py_list [mk_py_top T_string range]) range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_list_of", _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          let list, type_v = match eargs with [d;e] -> d,e | _ -> assert false in
          assume (mk_py_isinstance_builtin list "list" range) man flow
            ~fthen:(fun flow ->
                let var = var_of_eobj list in
                Libs.Py_mopsa.check man
                  (mk_py_isinstance (mk_var ~mode:(Some WEAK) var range) type_v range)
                  range flow
              )
            ~felse:(Libs.Py_mopsa.check man (mk_py_false range) range)
        )
      |> OptionExt.return

    | E_py_annot {ekind = E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) } when get_orig_vname c.py_cls_a_var = "List" ->
      let addr_list = mk_alloc_addr (A_py_list (Rangeset.singleton range)) range in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
      Eval.bind (fun eaddr_list flow ->
          let addr_list = addr_of_expr eaddr_list in
          let els_var = var_of_addr addr_list in
          let stmt = mk_stmt (S_py_annot (mk_var ~mode:(Some WEAK) els_var range, mk_expr (E_py_annot i) range)) range in
          man.exec ~zone:Zone.Z_py stmt flow |>
          Eval.singleton (mk_py_object (addr_list, None) range)
        )
      |> OptionExt.return

    | E_py_check_annot (tocheck, {ekind = E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) }) when get_orig_vname c.py_cls_a_var = "List" ->
      debug "s_py_check_annot list";
      assume (mk_py_isinstance_builtin tocheck "list" range) man flow
        ~fthen:(fun flow ->
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) tocheck flow |>
            bind_some (fun iterator flow ->
                let list_addr = match ekind iterator with
                  | E_py_object ({addr_kind = A_py_list r} as a, _) -> a
                  | _ -> Exceptions.panic "should be a list: %a@\nflow = %a@\n" pp_expr iterator (Flow.print man.lattice.print) flow in
                let var_els = var_of_addr list_addr in
                man.eval (mk_expr (E_py_check_annot (mk_var var_els range, i)) range) flow
              )
          )
        ~felse:(fun flow ->
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_false range) flow
          )
      |> OptionExt.return

    | _ -> None



  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_rename ({ekind = E_addr ({addr_kind = A_py_list _} as a)}, {ekind = E_addr a'}) ->
      (* FIXME: I guess we could just do it for every data_container. Maybe add a data_container domain on top of them performing the renaming?*)
      (* working on lists entails smashed element variable being index by the address, meaning we need to rename them *)
      let va = var_of_addr a in
      let va' = var_of_addr a' in
      debug "renaming %a into %a@\n" pp_var va pp_var va';
      let flow = man.exec ~zone:Zone.Z_py (mk_rename_var va va' range) flow in
      (* FIXME: now we need to do the same for iterators based on this address, but it's complicated *)
      (* let to_rename = []
       *     man.ask (Universal.Heap.Recency.Q_select_allocated_addresses
       *                            (fun addr -> match akind addr with
       *                               | A_py_iterator (_, l, _) ->
       *                                 List.exists (fun a_l -> compare_addr a_l a = 0) l
       *                               | _ -> false)
       *                         )
       *                 flow
       * in
       * List.fold_left (fun flow iterator ->
       *     let new_iterator = match akind iterator with
       *       | A_py_iterator (name, addrs, pos) ->
       *         let new_addrs = List.map (fun addr ->
       *             if compare_addr addr a = 0 then a' else addr) addrs in
       *         {iterator with addr_kind = A_py_iterator(name, new_addrs, pos)}
       *       | _ -> assert false
       *     in
       *     man.exec ~zone:Universal.Zone.Z_u_heap (mk_rename (mk_addr iterator range) (mk_addr new_iterator range) range) flow
       *   ) flow to_rename *)
      flow
      |> Post.return |> OptionExt.return



    | _ -> None

  let ask : type r. r query -> ('a, unit) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_print_addr_related_info ({addr_kind = A_py_list _} as addr) ->
      OptionExt.return @@
      fun fmt ->
      Format.fprintf fmt "%a"
        (man.ask Framework.Engines.Interactive.Q_print_var flow) (var_of_addr addr).vname

    | _ -> None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
