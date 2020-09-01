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

(** A general smashing abstraction for Python dicts *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast
open Data_container_utils

let name = "python.objects.dict"

type addr_kind +=
  | A_py_dict
  (* variables where the smashed elements are stored (one for the keys and one for the values *)
  | A_py_dict_view of string (* name *)


let () = register_addr_kind_nominal_type (fun default ak ->
             match ak with
             | A_py_dict -> "dict"
             | A_py_dict_view s -> s
             | _ -> default ak);
         register_addr_kind_structural_type (fun default ak s ->
             match ak with
             | A_py_dict | A_py_dict_view _ -> false
             | _ -> default ak s)



let () =
  register_is_data_container (fun default ak -> match ak with
                                                | A_py_dict -> true
                                                | A_py_dict_view _ -> true
                                                | _ -> default ak)


let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_dict -> fprintf fmt "dict"
          | A_py_dict_view s -> fprintf fmt "%s" s
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_dict_view s1, A_py_dict_view s2 ->
             Stdlib.compare s1 s2
          | _ -> default a1 a2);})


let opt_py_dict_allocation_policy : string ref = ref "all"
let () = Universal.Heap.Policies.register_option opt_py_dict_allocation_policy name "-py-dict-alloc-pol" "for smashed dictionaries"
           (fun default ak -> match ak with
                              | A_py_dict
                              | A_py_dict_view _ ->
                                 (Universal.Heap.Policies.of_string !opt_py_dict_allocation_policy) ak
                              | _ -> default ak)

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = name
    end)

  let alarms = []

  let init (prog:program) man flow = flow

  let kvar_of_addr a = match akind a with
    | A_py_dict -> mk_addr_attr a "dict_key" (T_py None)
    | _ -> assert false

  let vvar_of_addr a = match akind a with
    | A_py_dict -> mk_addr_attr a "dict_val" (T_py None)
    | _ -> assert false

  let var_of_addr a = match akind a with
    | A_py_dict -> mk_addr_attr a "dict_key" (T_py None),
                     mk_addr_attr a "dict_val" (T_py None)
    | _ -> assert false

  let viewseq_of_addr a = mk_addr_attr a "view_seq" (T_py None)

  let addr_of_expr exp = match ekind exp with
    | E_addr a -> a
    | _ -> Exceptions.panic "%a@\n" pp_expr exp

  let addr_of_eobj exp = match ekind exp with
    | E_py_object (a, _) -> a
    | _ -> Exceptions.panic "%a@\n" pp_expr exp

  let extract_vars dictobj =
    match ekind dictobj with
    | E_py_object ({addr_kind = A_py_dict} as addr, _) ->
      mk_addr_attr addr "dict_key" (T_py None),
      mk_addr_attr addr "dict_val" (T_py None)
    | _ -> assert false


  let rec eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_dict (ks, vs) ->
      debug "Skipping dict.__new__, dict.__init__ for now@\n";

      let addr_dict = mk_alloc_addr A_py_dict range in
      man.eval   addr_dict flow >>$
 (fun eaddr_dict flow ->
          let addr_dict = addr_of_expr eaddr_dict in
          let els_keys, els_vals = var_of_addr addr_dict in
          let flow = List.fold_left2 (fun acc key valu ->
              acc >>%
              man.exec   (mk_assign (mk_var ~mode:(Some WEAK) els_keys range) key range) >>%
              man.exec   (mk_assign (mk_var ~mode:(Some WEAK) els_vals range) valu range)
            ) (Post.return flow) ks vs in
          flow >>% Eval.singleton (mk_py_object (addr_dict, None) range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__new__", _))}, _)}, cls :: _, []) ->
      Utils.new_wrapper man range flow "dict" cls
        ~fthennew:(man.eval (mk_expr ~etyp:(T_py None) (E_py_dict ([],[])) range))

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__init__" as f, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.update" as f, _))}, _)}, args, []) ->
      (* FIXME: it's actually a bit more complicated than that, as you can pass **kwargs *)
      Utils.check_instances f man flow range args
        ["dict"; "dict"]
        (fun eargs flow ->
           let dict, snddict = match eargs with l::s::[] -> l, s | _ -> assert false in
           let var_keys, var_values = extract_vars dict in
           let var_sndkeys, var_sndvalues = extract_vars dict in
           man.exec (mk_assign (mk_var var_keys ~mode:(Some WEAK) range) (mk_var var_sndkeys ~mode:(Some WEAK) range) range) flow >>%
           man.exec (mk_assign (mk_var var_values ~mode:(Some WEAK) range) (mk_var var_sndvalues ~mode:(Some WEAK) range) range) >>%
           man.eval (mk_py_none range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__setitem__" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:2 f man flow range args
        ["dict"]
        (fun args flow ->
           let dict, key, value = match args with [d;k;v] -> d,k,v | _ -> assert false in
           let var_keys, var_values = extract_vars dict in
           man.exec (mk_assign (mk_var ~mode:(Some WEAK) var_keys range) key range) flow >>%
           man.exec (mk_assign (mk_var ~mode:(Some WEAK) var_values range) value range) >>%
           man.eval (mk_py_none range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.copy" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in
           man.eval (mk_expr ~etyp:(T_py None) (E_py_dict ([mk_var ~mode:(Some WEAK) var_k range], [mk_var ~mode:(Some WEAK) var_v range])) range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.clear" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in
           man.exec (mk_remove_var var_k range) flow >>%
           man.exec (mk_remove_var var_v range) >>%
           man.eval (mk_py_none range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.pop" as f, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__getitem__" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in

           let keyerror_f = man.exec (Utils.mk_builtin_raise "KeyError" range) flow |> post_to_flow man in
           let keyerror = Eval.empty_singleton keyerror_f in

           let flow = Flow.copy_ctx keyerror_f flow in
           let evals = man.eval   (mk_var var_v range) flow in

           Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (evals :: Cases.copy_ctx evals keyerror :: [])
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.get", _))}, _)} as call, dict :: key :: [], []) ->
      (* we specify that default = none *)
      man.eval {exp with ekind=E_py_call(call, dict::key::(mk_py_none range)::[], [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.get" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:2 f man flow range args ["dict"]
        (fun args flow ->
           debug "at range %a, flow = %a@\n" pp_range range (Flow.print man.lattice.print) flow;
           let var_k, var_v = extract_vars (List.hd args) in

           let eval_r = man.eval   (mk_var var_v range) flow in
           let default = man.eval   (List.nth args 2) flow  in

           Eval.join default eval_r
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.popitem" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in

           let eval_r = man.eval   (mk_expr ~etyp:(T_py None) (E_py_tuple [mk_var var_k range; mk_var var_v range]) range) flow in

           let flow = Flow.set_ctx (Cases.get_ctx eval_r) flow in

           let empty = man.exec (Utils.mk_builtin_raise "KeyError" range) flow >>% Eval.empty_singleton in

           Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) ( empty ::  Cases.copy_ctx empty eval_r :: [])
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__contains__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args ["dict"]
        (fun args flow ->
           man.eval (mk_py_top (T_py (Some Bool)) range) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__iter__" as f, _))}, _)}, args, []) ->
         Utils.check_instances f man flow range args ["dict"]
        (fun args flow ->
           let dict = List.hd args in
           let a = mk_alloc_addr (Py_list.A_py_iterator ("dict_keyiterator", None)) range in
           man.eval   a flow >>$
 (fun addr_it flow ->
                 let addr_it = match ekind addr_it with E_addr a -> a | _ -> assert false in
                 man.exec   (mk_assign (mk_var (Py_list.Domain.itseq_of_addr addr_it) range) dict range) flow >>%
                   Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.setdefault", _))}, _)} as call, dict::key::[], []) ->
      man.eval {exp with ekind=E_py_call(call, dict::key::(mk_py_none range)::[], [])} flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.setdefault" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:2 man flow range args ["dict"]
        (fun args flow ->
           let dict, key, default = match args with a::b::c::[] -> a,b,c | _ -> assert false in
           let var_k, var_v = extract_vars dict in

           man.exec (mk_assign (mk_var var_k ~mode:(Some WEAK) range) key range) flow >>%
           man.exec (mk_assign (mk_var var_v ~mode:(Some WEAK) range) default range) >>%
           man.eval (mk_var var_v range)
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.items" as n, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.keys" as n, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.values" as n, _))}, _)}, args, []) ->
      let viewname = match n with
        | "dict.items" -> "dict_items"
        | "dict.keys" -> "dict_keys"
        | "dict.values" -> "dict_values"
        | _ -> assert false in
      Utils.check_instances n man flow range args ["dict"]
        (fun args flow ->
           let dict = List.hd args in
           let a = mk_alloc_addr (A_py_dict_view viewname) range in
           man.eval   a flow >>$
 (fun addr_it flow ->
               let addr_it = match ekind addr_it with E_addr a -> a | _ -> assert false in
               man.exec   (mk_assign (mk_var (viewseq_of_addr addr_it) range) dict range) flow >>%
               Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_items.__iter__" as n, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_values.__iter__" as n, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_keys.__iter__" as n, _))}, _)}, args, []) ->
      let case, itname = match n with
        | "dict_items.__iter__" -> "dict_items", "dict_itemiterator"
        | "dict_values.__iter__" -> "dict_values", "dict_valueiterator"
        | "dict_keys.__iter__" -> "dict_keys", "dict_keyiterator"
        | _ -> assert false in
      Utils.check_instances n man flow range args [case]
        (fun args flow ->
          man.eval   (mk_var (viewseq_of_addr @@ addr_of_eobj @@ List.hd args) range) flow >>$
 (fun dict_eobj flow ->
                let a = mk_alloc_addr (Py_list.A_py_iterator (itname, None)) range in
                man.eval   a flow >>$
 (fun addr_it flow ->
                      let addr_it = match ekind addr_it with E_addr a -> a | _ -> assert false in
                      man.exec   (mk_assign (mk_var (Py_list.Domain.itseq_of_addr addr_it) range) dict_eobj range) flow >>%
                        Eval.singleton (mk_py_object (addr_it, None) range)
                    )
              )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_keyiterator.__next__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["dict_keyiterator"]
        (fun args flow ->
          man.eval   (mk_var (Py_list.Domain.itseq_of_eobj @@ List.hd args) range) flow >>$
 (fun dict_eobj flow ->
                let var_k = kvar_of_addr @@ addr_of_eobj dict_eobj in
                let els = man.eval (mk_var var_k ~mode:(Some WEAK) range) flow in

                let flow = Flow.set_ctx (Cases.get_ctx els) flow in
                let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty_singleton in
                Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Cases.copy_ctx stopiteration els :: stopiteration :: [])
              )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_valueiterator.__next__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["dict_valueiterator"]
        (fun args flow ->
          man.eval   (mk_var (Py_list.Domain.itseq_of_eobj @@ List.hd args) range) flow >>$
 (fun dict_eobj flow ->
                let var_v = vvar_of_addr @@ addr_of_eobj dict_eobj in
                let els = man.eval (mk_var var_v ~mode:(Some WEAK) range) flow in

                let flow = Flow.set_ctx (Cases.get_ctx els) flow in
                let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty_singleton in
                Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Cases.copy_ctx stopiteration els :: stopiteration :: [])
              )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_itemiterator.__next__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["dict_itemiterator"]
        (fun args flow ->
          man.eval   (mk_var (Py_list.Domain.itseq_of_eobj @@ List.hd args) range) flow >>$
 (fun dict_eobj flow ->
                let var_k, var_v = var_of_addr @@ addr_of_eobj dict_eobj in
                let els = man.eval (mk_expr ~etyp:(T_py None) (E_py_tuple [mk_var var_k ~mode:(Some WEAK) range;
                                                         mk_var var_v ~mode:(Some WEAK) range]) range) flow in
                let flow = Flow.set_ctx (Cases.get_ctx els) flow in
                let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty_singleton in
                Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow) (Cases.copy_ctx stopiteration els :: stopiteration :: [])
              )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_dict_of", _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          let dict, type_k, type_v = match eargs with [d;e;f] -> d,e,f | _ -> assert false in
          assume (mk_py_isinstance_builtin dict "dict" range) man flow
            ~fthen:(fun flow ->
                let var_k, var_v = extract_vars dict in
                Libs.Py_mopsa.check man
                  (Utils.mk_builtin_call "bool" [
                      (mk_binop ~etyp:(T_py None)
                         (mk_py_isinstance (mk_var ~mode:(Some WEAK) var_k range) type_k range)
                         O_py_and
                         (mk_py_isinstance (mk_var ~mode:(Some WEAK) var_v range) type_v range)
                         range)
                    ] range)
                  range flow
              )
            ~felse:(Libs.Py_mopsa.check man (mk_py_false range) range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["dict"]
        (fun args flow ->
           man.eval   (mk_py_top T_int range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_items.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["dict_items"]
        (fun args flow ->
           man.eval   (mk_py_top T_int range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_keys.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["dict_keys"]
        (fun args flow ->
           man.eval   (mk_py_top T_int range) flow
        )
      |> OptionExt.return



    | E_py_annot {ekind = E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) } when get_orig_vname c.py_cls_a_var = "Dict" ->
      let addr_dict = mk_alloc_addr A_py_dict range in
      let ty_key, ty_value = match ekind i with
        | E_py_tuple (a::b::[]) -> a, b
        | _ -> assert false in
      man.eval   addr_dict flow >>$
 (fun eaddr_dict flow ->
          let addr_dict = addr_of_expr eaddr_dict in
          let keys_var, values_var = var_of_addr addr_dict in
          let stmts = mk_block (
              List.map (fun (var, annot) ->
                  mk_stmt (S_py_annot
                             (mk_var ~mode:(Some WEAK) var range,
                              mk_expr ~etyp:(T_py None) (E_py_annot annot) range)
                          ) range
                ) [(keys_var, ty_key); (values_var, ty_value)]) range in
          man.exec   stmts flow >>%
          Eval.singleton (mk_py_object (addr_dict, None) range)
        )
      |> OptionExt.return

    | _ -> None


  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_rename ({ekind = E_addr ({addr_kind = A_py_dict} as a)}, {ekind = E_addr a'}) ->
      let kva, vva = var_of_addr a in
      let kva', vva' = var_of_addr a' in
      debug "renaming %a into %a@\n" pp_var kva pp_var kva';
      let flow = man.exec   (mk_rename_var kva kva' range) flow  in
      debug "renaming %a into %a@\n" pp_var vva pp_var vva';
      flow >>% man.exec   (mk_rename_var vva vva' range)
      |> OptionExt.return

    | S_fold ({ekind = E_addr ({addr_kind = A_py_dict} as a)}, addrs) ->
       let kva, vva = var_of_addr a in
       let kvas, vvas = List.split @@ List.map (fun ea' -> match ekind ea' with
                                      | E_addr ({addr_kind = A_py_dict} as a') -> var_of_addr a'
                                      | _ -> assert false) addrs in
       flow |>
         man.exec   (mk_fold_var kva kvas range) >>%
         man.exec   (mk_fold_var vva vvas range) |>
         OptionExt.return

    | S_expand ({ekind = E_addr ({addr_kind = A_py_dict} as a)}, addrs) ->
       let kva, vva = var_of_addr a in
       let kvas, vvas = List.split @@ List.map (fun ea' -> match ekind ea' with
                                                           | E_addr ({addr_kind = A_py_dict} as a') -> var_of_addr a'
                                                           | _ -> assert false) addrs in
       flow |>
         man.exec   (mk_expand_var kva kvas range) >>%
         man.exec   (mk_expand_var vva vvas range) |>
         OptionExt.return

    | S_remove {ekind = E_addr ({addr_kind = A_py_dict} as a)} ->
       let kva, vva = var_of_addr a in
       flow |>
         man.exec   (mk_remove_var kva range) >>%
         man.exec   (mk_remove_var vva range) |>
         OptionExt.return

    | S_invalidate {ekind = E_addr ({addr_kind = A_py_dict} as a)} ->
       let kva, vva = var_of_addr a in
       flow |>
         man.exec   (mk_remove_var kva range) >>%
         man.exec   (mk_remove_var vva range) |>
         OptionExt.return

    | S_remove {ekind = E_addr ({addr_kind = A_py_dict_view _} as a)} ->
       let va = viewseq_of_addr a in
       flow |> man.exec   (mk_remove_var va range) |> OptionExt.return

    | S_rename ({ekind = E_addr ({addr_kind = A_py_dict_view _} as a)}, {ekind = E_addr a'}) ->
       let va = viewseq_of_addr a in
       let va' = viewseq_of_addr a' in
       man.exec   (mk_rename_var va va' range) flow |> OptionExt.return

    | S_fold ({ekind = E_addr ({addr_kind = A_py_dict_view _} as a)}, addrs) ->
       let va = viewseq_of_addr a in
       let vas = List.map (fun ea' -> match ekind ea' with
                                      | E_addr ({addr_kind = A_py_dict_view _} as a') -> viewseq_of_addr a'
                                      | _ -> assert false) addrs in
       man.exec   (mk_fold_var va vas range) flow |> OptionExt.return


    | S_expand ({ekind = E_addr ({addr_kind = A_py_dict_view _} as a)}, addrs) ->
       let va = viewseq_of_addr a in
       let vas = List.map (fun ea' -> match ekind ea' with
                                      | E_addr ({addr_kind = A_py_dict_view _} as a') -> viewseq_of_addr a'
                                      | _ -> assert false) addrs in
       man.exec   (mk_expand_var va vas range) flow |> OptionExt.return

    | S_invalidate {ekind = E_addr ({addr_kind = A_py_dict_view _} as a)} ->
       let va = viewseq_of_addr a in
       man.exec   (mk_remove_var va range) flow |> OptionExt.return


    | _ -> None

  let ask : type r. ('a, r) query -> ('a, unit) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Universal.Ast.Q_debug_addr_value ({addr_kind = A_py_dict} as addr) ->
       let open Framework.Engines.Interactive in
       let keys_dict = man.ask (Q_debug_variable_value (kvar_of_addr addr)) flow in
       let values_dict = man.ask (Q_debug_variable_value (vvar_of_addr addr)) flow in
       Some {var_value = None;
             var_value_type = T_any;
             var_sub_value = Some (Named_sub_value
                                     (("keys", keys_dict)::
                                        ("values", values_dict)::[]))
         }

    | _ -> None

end

let () =
  register_stateless_domain (module Domain)
