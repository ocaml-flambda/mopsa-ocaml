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

(* A general smashing abstraction for Python dicts, (hopefully)
   irrelevant of the value/type domain *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast
open Data_container_utils

type addr_kind +=
  | A_py_dict of Rangeset.t * Rangeset.t
  (* variables where the smashed elements are stored (one for the keys and one for the values *)
  | A_py_dict_view of string (* name *) * addr (* addr of the dictionary *)


let () =
  register_join_akind (fun default ak1 ak2 ->
      match ak1, ak2 with
      | A_py_dict (k1, v1), A_py_dict (k2, v2) -> A_py_dict ((Rangeset.union k1 k2), (Rangeset.union v1 v2))
      | _ -> default ak1 ak2);
  register_is_data_container (fun default ak -> match ak with
      | A_py_dict _ -> true
      | _ -> default ak)


let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_dict (keys, values) -> fprintf fmt "dict[%a, %a]" (fun fmt -> Rangeset.iter (fun ra -> pp_range fmt ra)) keys (fun fmt -> Rangeset.iter (fun ra -> pp_range fmt ra)) values
          | A_py_dict_view (s, a) -> fprintf fmt "%s[%a]" s pp_addr a
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_dict (k1, v1), A_py_dict (k2, v2) ->
            Compare.compose [
              (fun () -> Rangeset.compare k1 k2);
              (fun () -> Rangeset.compare v1 v2);
            ]
          | A_py_dict_view (s1, a1), A_py_dict_view (s2, a2) ->
            Compare.compose [
              (fun () -> Pervasives.compare s1 s2);
              (fun () -> compare_addr a1 a2);
            ]
          | _ -> default a1 a2);})


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.dict"
    end)

  let interface = {
    iexec = {provides = [Zone.Z_py_obj]; uses = [Zone.Z_py_obj]};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any]}
  }

  let init (prog:program) man flow = flow

  let kvar_of_addr a = match akind a with
    | A_py_dict _ -> mk_addr_attr a "dict_key" T_any
    | _ -> assert false

  let vvar_of_addr a = match akind a with
    | A_py_dict _ -> mk_addr_attr a "dict_val" T_any
    | _ -> assert false

  let var_of_addr a = match akind a with
    | A_py_dict _ -> mk_addr_attr a "dict_key" T_any,
                     mk_addr_attr a "dict_val" T_any
    | _ -> assert false

  (* let var_of_eobj e = match ekind e with
   *   | E_py_object (a, _) -> var_of_addr a
   *   | _ -> assert false *)

  let addr_of_expr exp = match ekind exp with
    | E_addr a -> a
    | _ -> Exceptions.panic "%a@\n" pp_expr exp

  let extract_vars dictobj =
    match ekind dictobj with
    | E_py_object ({addr_kind = A_py_dict _} as addr, _) ->
      mk_addr_attr addr "dict_key" T_any,
      mk_addr_attr addr "dict_val" T_any
    | _ -> assert false


  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_dict (ks, vs) ->
      debug "Skipping dict.__new__, dict.__init__ for now@\n";

      let addr_dict = mk_alloc_addr (A_py_dict (Rangeset.singleton range, Rangeset.singleton range)) range in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_dict flow |>
      Eval.bind (fun eaddr_dict flow ->
          let addr_dict = addr_of_expr eaddr_dict in
          let els_keys, els_vals = var_of_addr addr_dict in
          let flow = List.fold_left2 (fun acc key valu ->
              acc |>
              man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_keys range) key range) |>
              man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_vals range) valu range)
            ) flow ks vs in
          Eval.singleton (mk_py_object (addr_dict, None) range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.__new__")}, _)}, cls :: _, []) ->
      man.eval (mk_expr (E_py_dict ([],[])) range) flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.__init__")}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.update")}, _)}, args, []) ->
      (* FIXME: it's actually a bit more complicated than that, as you can pass **kwargs *)
      Utils.check_instances man flow range args
        ["dict"; "dict"]
        (fun eargs flow ->
           let dict, snddict = match eargs with l::s::[] -> l, s | _ -> assert false in
           let var_keys, var_values = extract_vars dict in
           let var_sndkeys, var_sndvalues = extract_vars dict in
           man.exec (mk_assign (mk_var var_keys ~mode:WEAK range) (mk_var var_sndkeys ~mode:WEAK range) range) flow |>
           man.exec (mk_assign (mk_var var_values ~mode:WEAK range) (mk_var var_sndvalues ~mode:WEAK range) range) |>
           man.eval (mk_py_none range)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.__setitem__")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:2 man flow range args
        ["dict"]
        (fun args flow ->
           let dict, key, value = match args with [d;k;v] -> d,k,v | _ -> assert false in
           let var_keys, var_values = extract_vars dict in
           man.exec (mk_assign (mk_var ~mode:WEAK var_keys range) key range) flow |>
           man.exec (mk_assign (mk_var ~mode:WEAK var_values range) value range) |>
           man.eval (mk_py_none range)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.copy")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict"]
        (fun args flow ->
           (* FIXME: to test *)
           let var_k, var_v = extract_vars (List.hd args) in
           man.eval (mk_expr (E_py_dict ([mk_var ~mode:WEAK var_k range], [mk_var ~mode:WEAK var_v range])) range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.clear")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in
           man.exec (mk_remove_var var_k range) flow |>
           man.exec (mk_remove_var var_v range) |>
           man.eval (mk_py_none range)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.pop")}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.__getitem__")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in

           let keyerror_f = man.exec (Utils.mk_builtin_raise "KeyError" range) flow in
           let keyerror = Eval.empty_singleton keyerror_f in

           let flow = Flow.copy_ctx keyerror_f flow in
           let evals = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var var_v range) flow in

           Eval.join_list ~empty:(Eval.empty_singleton flow) (evals :: Eval.copy_ctx evals keyerror :: [])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.get")}, _)} as call, dict :: key :: [], []) ->
      (* we specify that default = none *)
      man.eval {exp with ekind=E_py_call(call, dict::key::(mk_py_none range)::[], [])} flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.get")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:2 man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in

           let eval_r = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var var_v range) flow in

           let flow = Flow.set_ctx (Eval.get_ctx eval_r) flow in
           let default = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (List.nth args 2) flow  in

           Eval.join_list~empty:(Eval.empty_singleton flow) (default :: (Eval.copy_ctx default eval_r) :: [])

        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.popitem")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in

           let eval_r = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_tuple [mk_var var_k range; mk_var var_v range]) range) flow in

           let flow = Flow.set_ctx (Eval.get_ctx eval_r) flow in

           let empty = man.exec (Utils.mk_builtin_raise "KeyError" range) flow |> Eval.empty_singleton in

           Eval.join_list ~empty:(Eval.empty_singleton flow) ( empty ::  Eval.copy_ctx empty eval_r :: [])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.__contains__")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args ["dict"]
        (fun args flow ->
           man.eval (mk_py_top T_bool range) flow)
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.__iter__")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict"]
        (fun args flow ->
           let dict = List.hd args in
           let dict_addr = match ekind dict with
             | E_py_object ({addr_kind = A_py_dict _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (Py_list.A_py_iterator ("dict_keyiterator", [dict_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun addr_it flow ->
               let addr_it = match ekind addr_it with E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.items" as n)}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.keys" as n)}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.values" as n)}, _)}, args, []) ->
      let viewname = match n with
        | F_builtin "dict.items" -> "dict_items"
        | F_builtin "dict.keys" -> "dict_keys"
        | F_builtin "dict.values" -> "dict_values"
        | _ -> assert false in
      Utils.check_instances man flow range args ["dict"]
        (fun args flow ->
           let dict = List.hd args in
           let dict_addr = match ekind dict with
             | E_py_object ({addr_kind = A_py_dict _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (A_py_dict_view (viewname, dict_addr)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun addr_it flow ->
               let addr_it = match ekind addr_it with E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_items.__iter__" as n)}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_values.__iter__" as n)}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_keys.__iter__" as n)}, _)}, args, []) ->
      let case, itname = match n with
        | F_builtin "dict_items.__iter__" -> "dict_items", "dict_itemiterator"
        | F_builtin "dict_values.__iter__" -> "dict_values", "dict_valueiterator"
        | F_builtin "dict_keys.__iter__" -> "dict_keys", "dict_keyiterator"
        | _ -> assert false in
      Utils.check_instances man flow range args [case]
        (fun args flow ->
           let dict_addr = match ekind @@ List.hd args with
             | E_py_object ({addr_kind = A_py_dict_view (case, a)}, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (Py_list.A_py_iterator (itname, [dict_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun addr_it flow ->
               let addr_it = match ekind addr_it with E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_keyiterator.__next__")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict_keyiterator"]
        (fun args flow ->
           let dict_addr = match ekind @@ List.hd args with
             | E_py_object ({addr_kind = Py_list.A_py_iterator ("dict_keyiterator", [a], _)}, _) -> a
             | _ -> assert false in
           let var_k = kvar_of_addr dict_addr in
           let els = man.eval (mk_var var_k ~mode:WEAK range) flow in

           let flow = Flow.set_ctx (Eval.get_ctx els) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list ~empty:(Eval.empty_singleton flow) (Eval.copy_ctx stopiteration els :: stopiteration :: [])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_valueiterator.__next__")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict_valueiterator"]
        (fun args flow ->
           let dict_addr = match ekind @@ List.hd args with
             | E_py_object ({addr_kind = Py_list.A_py_iterator ("dict_valueiterator", [a], _)}, _) -> a
             | _ -> assert false in
           let var_v = vvar_of_addr dict_addr in
           let els = man.eval (mk_var var_v ~mode:WEAK range) flow in

           let flow = Flow.set_ctx (Eval.get_ctx els) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list ~empty:(Eval.empty_singleton flow) (Eval.copy_ctx stopiteration els :: stopiteration :: [])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_itemiterator.__next__")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict_itemiterator"]
        (fun args flow ->
           let dict_addr = match ekind @@ List.hd args with
             | E_py_object ({addr_kind = Py_list.A_py_iterator ("dict_itemiterator", [a], _)}, _) -> a
             | _ -> assert false in
           let var_k, var_v = var_of_addr dict_addr in
           let els = man.eval (mk_expr (E_py_tuple [mk_var var_k ~mode:WEAK range;
                                                    mk_var var_v ~mode:WEAK range]) range) flow in
           let flow = Flow.set_ctx (Eval.get_ctx els) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list ~empty:(Eval.empty_singleton flow) (Eval.copy_ctx stopiteration els :: stopiteration :: [])
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_dict_of")}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          let dict, type_k, type_v = match eargs with [d;e;f] -> d,e,f | _ -> assert false in
          assume (mk_py_isinstance_builtin dict "dict" range) man flow
            ~fthen:(fun flow ->
                let var_k, var_v = extract_vars dict in
                Libs.Py_mopsa.check man
                  (Utils.mk_builtin_call "bool" [
                      (mk_binop
                         (mk_py_isinstance (mk_var ~mode:WEAK var_k range) type_k range)
                         O_py_and
                         (mk_py_isinstance (mk_var ~mode:WEAK var_v range) type_v range)
                         range)
                    ] range)
                  range flow
              )
            ~felse:(Libs.Py_mopsa.check man (mk_py_false range) range)
        )
      |> Option.return

    | _ -> None


  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_rename ({ekind = E_addr ({addr_kind = A_py_dict _} as a)}, {ekind = E_addr a'}) ->
      let kva, vva = var_of_addr a in
      let kva', vva' = var_of_addr a' in
      debug "renaming %a into %a@\n" pp_var kva pp_var kva';
      let flow = man.exec ~zone:Zone.Z_py (mk_rename_var kva kva' range) flow  in
      debug "renaming %a into %a@\n" pp_var vva pp_var vva';
      man.exec ~zone:Zone.Z_py (mk_rename_var vva vva' range) flow
      |> Post.return |> Option.return

    | _ -> None

  let ask _ _ _ = None
end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
