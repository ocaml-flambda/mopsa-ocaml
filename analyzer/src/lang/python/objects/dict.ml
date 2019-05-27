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

type addr_kind +=
  | A_py_dict of var * var (* variables where the smashed elements are stored (on for the keys and one for the values *)
  | A_py_dict_view of string (* name *) * addr (* addr of the dictionary *)

let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_dict (keys, values) -> fprintf fmt "dict[%a, %a]" pp_var keys pp_var values
          | A_py_dict_view (s, a) -> fprintf fmt "%s[%a]" s pp_addr a
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_dict (k1, v1), A_py_dict (k2, v2) ->
            Compare.compose [
              (fun () -> compare_var k1 k2);
              (fun () -> compare_var v1 v2);
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


  module VarsInfo =
  struct
    type t = var * var
    let compare (k1, v1) (k2, v2) =
      Compare.compose
        [(fun () -> compare_var k1 k2);
         (fun () -> compare_var v1 v2);
        ]
    let print fmt (k, v) = Format.fprintf fmt "%a, %a" pp_var k pp_var v
  end

  module DictInfo = struct
    type t = Callstack.cs * range
    let compare (cs, r) (cs', r') =
      Compare.compose
        [
          (fun () -> Callstack.compare cs cs');
          (fun () -> compare_range r r')
        ]
    let print fmt (cs, r) =
      Format.fprintf fmt "(%a, %a)"
        Callstack.pp_call_stack cs
        pp_range r
  end

  module Equiv = Equiv.Make(DictInfo)(VarsInfo)

  let ctx_key =
    let module K = Context.GenUnitKey(
      struct
        type t = Equiv.t
        let print fmt m =
          Format.fprintf fmt "Dict annots: @[%a@]" (Equiv.print ?pp_sep:None) m
      end
      )
    in
    K.key

  let fresh_smashed_vars () =
    let k = mk_fresh_uniq_var "$d_k*" T_any () in
    let v = mk_fresh_uniq_var "$d_v*" T_any () in
    k, v

  let get_vars_equiv (info: DictInfo.t) (e: Equiv.t) =
    try
      Equiv.find_l info e, e
    with Not_found ->
      let vars = fresh_smashed_vars () in
      let new_eq = Equiv.add (info, vars) e in
      vars, new_eq

  let get_var_flow (info: DictInfo.t) (f: 'a flow) : (var * var) * 'a flow =
        let a = Flow.get_ctx f |>
            Context.find_unit ctx_key
    in
    let vars, a = get_vars_equiv info a in
    vars, Flow.set_ctx (Flow.get_ctx f |> Context.add_unit ctx_key a) f

  let interface = {
    iexec = {provides = []; uses = [Zone.Z_py_obj]};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any]}
  }

  let init (prog:program) man flow =
    Flow.set_ctx (
      Flow.get_ctx flow |>
      Context.add_unit ctx_key Equiv.empty
    ) flow


  let extract_vars dictobj =
    match ekind dictobj with
    | E_py_object ({addr_kind = A_py_dict (a, b)}, _) -> (a, b)
    | _ -> assert false


  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_dict (ks, vs) ->
      debug "Skipping dict.__new__, dict.__init__ for now@\n";
      let (els_keys, els_vals), flow = get_var_flow (Callstack.get flow, range) flow in
      let flow = List.fold_left2 (fun acc key valu ->
          acc |>
          man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_keys range) key range) |>
          man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_vals range) valu range)
        ) flow ks vs in
      let addr_dict = mk_alloc_addr (A_py_dict (els_keys, els_vals)) range in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_dict flow |>
      Eval.bind (fun addr_dict flow ->
          let addr_dict = match ekind addr_dict with E_addr a -> a | _ -> assert false in
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

           Eval.join_list (evals :: Eval.copy_ctx evals keyerror :: [])
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

           let flow = Flow.set_ctx (Eval.choose_ctx eval_r) flow in
           let default = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (List.nth args 2) flow  in

           Eval.join_list (default :: (Eval.copy_ctx default eval_r) :: [])

        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict.popitem")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict"]
        (fun args flow ->
           let var_k, var_v = extract_vars (List.hd args) in

           let eval_r = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_tuple [mk_var var_k range; mk_var var_v range]) range) flow in

           let flow = Flow.set_ctx (Eval.choose_ctx eval_r) flow in

           let empty = man.exec (Utils.mk_builtin_raise "KeyError" range) flow |> Eval.empty_singleton in

           Eval.join_list ( empty ::  Eval.copy_ctx empty eval_r :: [])
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
           let var_k = match akind dict_addr with
             | A_py_dict (k, v) -> k
             | _ -> assert false in
           let els = man.eval (mk_var var_k ~mode:WEAK range) flow in

           let flow = Flow.set_ctx (Eval.choose_ctx els) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list (Eval.copy_ctx stopiteration els :: stopiteration :: [])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_valueiterator.__next__")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict_valueiterator"]
        (fun args flow ->
           let dict_addr = match ekind @@ List.hd args with
             | E_py_object ({addr_kind = Py_list.A_py_iterator ("dict_valueiterator", [a], _)}, _) -> a
             | _ -> assert false in
           let var_v = match akind dict_addr with
             | A_py_dict (k, v) -> v
             | _ -> assert false in
           let els = man.eval (mk_var var_v ~mode:WEAK range) flow in

           let flow = Flow.set_ctx (Eval.choose_ctx els) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list (Eval.copy_ctx stopiteration els :: stopiteration :: [])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dict_itemiterator.__next__")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["dict_itemiterator"]
        (fun args flow ->
           let dict_addr = match ekind @@ List.hd args with
             | E_py_object ({addr_kind = Py_list.A_py_iterator ("dict_itemiterator", [a], _)}, _) -> a
             | _ -> assert false in
           let var_k, var_v = match akind dict_addr with
             | A_py_dict (k, v) -> k, v
             | _ -> assert false in
           let els = man.eval (mk_expr (E_py_tuple [mk_var var_k ~mode:WEAK range;
                                                    mk_var var_v ~mode:WEAK range]) range) flow in
           let flow = Flow.set_ctx (Eval.choose_ctx els) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list (Eval.copy_ctx stopiteration els :: stopiteration :: [])
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_dict_of")}, _)}, args, []) ->
      Eval.eval_list man.eval args flow |>
      Eval.bind (fun eargs flow ->
          let dict, type_k, type_v = match eargs with [d;e;f] -> d,e,f | _ -> assert false in
          assume_eval (mk_py_isinstance_builtin dict "dict" range) man flow
            ~fthen:(fun flow ->
                let var_k, var_v = extract_vars dict in
                Libs.Py_mopsa.check man
                  (Utils.mk_builtin_call "bool" [
                      (mk_binop
                         (mk_py_isinstance (mk_var ~mode:WEAK var_k range) type_k range)
                         O_py_or
                         (mk_py_isinstance (mk_var ~mode:WEAK var_v range) type_v range)
                         range)
                    ] range)
                  range flow
              )
            ~felse:(Libs.Py_mopsa.check man (mk_py_false range) range)
        )
      |> Option.return

    | _ -> None


  let exec zone stmt man flow = None

  let ask _ _ _ = None
end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
