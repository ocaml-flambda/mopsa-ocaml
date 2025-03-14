(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2022 The MOPSA Project.                               *)
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

(** A constant dictionary abstraction. Useful to be precise when passing named arguments to functions *)

(* I guess this is completely broken... d = {}. x = 2. d[2x+1] = 3. x += 1. d[2]? d[3]? *)
open Mopsa
open Sig.Abstraction.Domain
open Ast
open Addr
open Universal.Ast
open Data_container_utils


type ('a, _) query += Q_py_dict_items : expr -> ('a, (expr * expr) list) query

let () = register_query {
    join = (let f : type a r. query_pool -> (a, r) query -> r -> r -> r =
              fun next query a b ->
                match query with
                | Q_py_dict_items _ -> (a @ b)
                | _ -> next.pool_join query a b in
            f
           );
    meet = (let f : type a r. query_pool -> (a, r) query -> r -> r -> r =
              fun next query a b ->
                match query with
                | Q_py_dict_items _ -> assert false
                | _ -> next.pool_meet query a b in
            f)
  }



module Domain =
struct

  module Dicts =
    struct
      module DictSet = Framework.Lattices.Powerset.Make(struct
                           type t = (py_object * py_object) list
                           let compare = Compare.list (Compare.pair compare_py_object compare_py_object)
                           let print = unformat (fun fmt l -> Format.fprintf fmt "%a" (Format.pp_print_list (fun fmt (k, v) -> Format.fprintf fmt "%a ~> %a" Pp.pp_py_object k Pp.pp_py_object v)) l)
                         end)

      include DictSet

      let max_size = 1
      let bound (x:t) : t =
        match x with
        | Nt s when Set.cardinal s <= max_size -> x
        | _ -> TOP

      let join a1 a2 = DictSet.join a1 a2 |> bound

      let add v t =
        add v t |> bound
    end

  module DictMap = Framework.Lattices.Partial_map.Make(Addr)(Dicts)
  module DictAddrs = Framework.Lattices.Powerset.Make(Addr)

  type t = {dict: DictMap.t; addrs: DictAddrs.t}

  let empty = {dict = DictMap.empty; addrs = DictAddrs.empty}

  let widen ctx a1 a2 = {dict = DictMap.widen ctx a1.dict a2.dict; addrs = DictAddrs.join a1.addrs a2.addrs}

  let lift_binop fd fa a1 a2 = {dict = fd a1.dict a2.dict; addrs = fa a1.addrs a2.addrs}

  let join = lift_binop DictMap.join DictAddrs.join
  let meet = lift_binop DictMap.meet DictAddrs.meet

  let subset a1 a2 = DictMap.subset a1.dict a2.dict && DictAddrs.subset a1.addrs a2.addrs
  let top = {dict = DictMap.top; addrs = DictAddrs.top}
  let bottom = {dict = DictMap.bottom; addrs = DictAddrs.bottom}
  let is_bottom c = DictMap.is_bottom c.dict && DictAddrs.is_bottom c.addrs

  let find a c = DictMap.find a c.dict
  let add addr dict c = {dict = DictMap.add addr dict c.dict;
                         addrs = Dicts.fold (fun dict addrs ->
                                     List.fold_left (fun addrs ((ka, _), (va, _)) ->
                                         DictAddrs.add ka addrs |> DictAddrs.add va) addrs dict) dict c.addrs}
  let rename a a' c = {dict=DictMap.rename a a' c.dict; addrs = c.addrs}
  let remove a c = {dict=DictMap.remove a c.dict; addrs=c.addrs}

  include Framework.Core.Id.GenDomainId(struct
              type nonrec t = t
              let name = "python.objects.constant_dict"
            end)

  let checks = []

  let init _ man flow =
    set_env T_cur empty man flow |>
    Option.some

  let eval exp man flow =
    (***** FIXME: expressions are evaluated, but then the resulting python object (esp the addr) may be modified without this domain taking it into account... *****)
    let range = erange exp in
    match ekind exp with
    | E_py_dict (ks, vs) ->
       let addr_dict = mk_alloc_addr Dict.A_py_dict range in
       man.eval addr_dict flow >>$ (fun eaddr_dict flow ->
         let addr_dict = Addr.from_expr eaddr_dict in
         (* evaluate all keys and values and add them to dict *)
         Cases.bind_list ks man.eval flow >>$ fun eks flow ->
         Cases.bind_list vs man.eval flow >>$ fun evs flow ->
         get_env T_cur man flow >>$ fun cur flow ->
         let oks = List.map object_of_expr eks in
         let ovs = List.map object_of_expr evs in
         let ncur = add addr_dict (Dicts.singleton (List.combine oks ovs)) cur in
         debug "dict ok, ncur %a" (format DictMap.print) ncur.dict;
         set_env T_cur ncur man flow >>% fun flow ->
         Eval.singleton (mk_py_object (addr_dict, None) range) flow
       )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__new__", _))}, _)}, cls :: _, []) ->
      Utils.new_wrapper man range flow "dict" cls
        ~fthennew:(man.eval (mk_expr ~etyp:(T_py None) (E_py_dict ([],[])) range))

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__contains__" as f, _))}, _)}, args, []) ->
       Utils.check_instances ~arguments_after_check:1 f man flow range args ["dict"]
         (fun _ flow ->
           man.eval (mk_py_top T_bool range) flow
         )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__getitem__" as f, _))}, _)}, args, []) ->
       Utils.check_instances ~arguments_after_check:1 f man flow range args ["dict"]
         (fun args flow ->
           let dict, key = match args with [d; k] -> d, k | _ -> assert false in
           let dict_addr = addr_of_object @@ object_of_expr dict in
           let key_obj = object_of_expr key in
           get_env T_cur man flow >>$ fun cur flow ->
           let ds = find dict_addr cur in
           (* ok as ds should have <= 1 element. Otherwise need to fold *)
           let dict = Dicts.choose ds in
           (* debug "searching for %a, dict = %a, result = %b" Pp.pp_py_object key_obj (format Dicts.print ds (List.exists (fun (ko, _) -> compare_py_object_lax key_obj ko = 0) dict); *)
           let is_builtin_value o = match akind (addr_of_object o) with
             | A_py_instance {addr_kind = A_py_class (C_builtin ("int" | "bool" | "float" | "str" | "bytes"), _)} -> true
             | _ -> false  in
           (* FIXME: still an issue with weak addrs *)
           let found = List.fold_left (fun acc (ko, vo) ->
                           if is_builtin_value ko && is_builtin_value key_obj then
                             if compare_addr_kind (akind @@ fst ko) (akind @@ fst key_obj) = 0 then
                               let flow_eq =
                                 man.exec (mk_assume (eq (OptionExt.none_to_exn @@ snd ko) (OptionExt.none_to_exn @@ snd key_obj) range) range) flow |> post_to_flow man in
                               let flow_ne =
                                 man.exec (mk_assume (ne (OptionExt.none_to_exn @@ snd ko) (OptionExt.none_to_exn @@ snd key_obj) range) range) flow |> post_to_flow man in
                               match man.lattice.is_bottom (Flow.get T_cur man.lattice flow_eq), man.lattice.is_bottom (Flow.get T_cur man.lattice flow_ne) with
                               | true, _ -> acc
                               | false, true ->
                                  (* precise *)
                                  (true, Flow.add_safe_check Alarms.CHK_PY_KEYERROR range flow_eq |> Cases.singleton (mk_py_object vo range)) :: acc
                               | false, false ->
                                  (* unprecise *)
                                  (false, Flow.add_safe_check Alarms.CHK_PY_KEYERROR range flow_eq |> Cases.singleton (mk_py_object vo range)) :: acc
                             else
                               acc
                           else
                             if compare_addr (fst ko) (fst key_obj) == 0 then
                               ((fst ko).addr_mode = STRONG, Flow.add_safe_check Alarms.CHK_PY_KEYERROR range flow |>
                                 Cases.singleton (mk_py_object vo range)) :: acc
                             else acc
                         ) [] dict in
           if found = [] then man.exec (Utils.mk_builtin_raise "KeyError" range) flow >>% Eval.empty
           else
             let precise_list, found = List.split found in
             let exists_precise = List.exists (fun x -> x) precise_list in
             if exists_precise then
               Eval.join_list ~empty:(fun () -> assert false) found
             else
               Eval.join_list ~empty:(fun () -> assert false) ((man.exec (Utils.mk_builtin_raise "KeyError" range) flow >>% Eval.empty)::found)

         )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["dict"]
        (fun args flow ->
          let dict = List.hd args in
          let dict_addr = addr_of_object @@ object_of_expr dict in
          get_env T_cur man flow >>$ fun cur flow ->
          let ds = find dict_addr cur in
          let dict = Dicts.choose ds in
          man.eval (mk_int ~typ:(T_py None) (List.length dict) range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__iter__" as f, _))}, _)}, args, []) ->
         Utils.check_instances f man flow range args ["dict"]
        (fun args flow ->
           let dict = List.hd args in
           let a = mk_alloc_addr (Py_list.A_py_iterator ("dict_keyiterator", None)) range in
           man.eval   a flow >>$
 (fun addr_it flow ->
                 let addr_it = Addr.from_expr addr_it in
                 man.exec   (mk_assign (mk_var (Py_list.Domain.itseq_of_addr addr_it) range) dict range) flow >>%
                   Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_keyiterator.__next__" as f, _))}, _)}, args, []) ->
       (* we return everything at once. For more precision we may want to have a counter related to the iterator *)
       (* FIXME: in its current state, this should be handled by the reduced product with the smashing dictionnary *)
      Utils.check_instances f man flow range args ["dict_keyiterator"]
        (fun args flow ->
          man.eval   (mk_var (Py_list.Domain.itseq_of_eobj @@ List.hd args) range) flow >>$
            (fun dict_eobj flow ->
              let dict_addr = addr_of_object @@ object_of_expr dict_eobj in
              get_env T_cur man flow >>$ fun cur flow ->
              let ds = find dict_addr cur in
              (* ok as ds should have <= 1 element. Otherwise need to fold *)
              let dict = Dicts.choose ds in

              let els =
                let flow = Flow.add_safe_check Alarms.CHK_PY_STOPITERATION range flow in
                List.map (fun (_, obj) -> Eval.singleton (mk_py_object obj range) flow) dict in

              let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty in
              Eval.join_list ~empty:(fun () -> Eval.empty flow) (stopiteration :: els)
            )
        )
      |> OptionExt.return

    | E_py_annot {ekind = E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) } when get_orig_vname c.py_cls_a_var = "Dict" ->
      let addr_dict = mk_alloc_addr Dict.A_py_dict range in
      let ty_key, ty_value = match ekind i with
        | E_py_tuple (a::b::[]) -> a, b
        | _ -> assert false in
      man.eval   addr_dict flow >>$
 (fun eaddr_dict flow ->
          let addr_dict = Addr.from_expr eaddr_dict in
          let keys_var, values_var = Dict.Domain.var_of_addr addr_dict in
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
    match skind stmt with
    | S_add {ekind = E_addr ({addr_kind = Dict.A_py_dict} as addr_dict, _)} ->
       get_env T_cur man flow >>$? fun cur flow ->
       set_env T_cur (add addr_dict Dicts.empty cur) man flow |> OptionExt.return

    | S_rename ({ekind = E_addr ({addr_kind = Dict.A_py_dict} as a, _)}, {ekind = E_addr (a', _)})
    | S_fold ({ekind = E_addr (a', _)}, [{ekind = E_addr ({addr_kind = Dict.A_py_dict} as a, _)}]) ->
       get_env T_cur man flow >>$? fun cur flow ->
       set_env T_cur (rename a a' cur) man flow |> OptionExt.return

    | S_expand ({ekind = E_addr ({addr_kind = Dict.A_py_dict} as addr_dict, _)}, addrs) ->
       get_env T_cur man flow >>$? fun cur flow ->
       let d = find addr_dict cur in
       set_env T_cur
         (List.fold_left (fun cur ea ->
           let a = Addr.from_expr ea in
           add a d cur
            ) cur addrs) man flow
       |> OptionExt.return

    | S_remove {ekind = E_addr ({addr_kind = Dict.A_py_dict} as a, _)}
    | S_invalidate {ekind = E_addr ({addr_kind = Dict.A_py_dict} as a, _)} ->
       get_env T_cur man flow >>$? fun cur flow ->
       set_env T_cur (remove a cur) man flow |> OptionExt.return

    | _ -> None

  let print_expr _ _ _ _ = ()
  let print_state printer a =
    pprint ~path:[Key "Constant dictionaries"] printer (pbox DictMap.print a.dict)

  let merge _ _ _ = assert false

  let ask : type r. ('a, r) query -> ('a, t) man -> 'a flow -> ('a, r) cases option =
    fun query man flow ->
    match query with
    | Q_py_dict_items dict ->
       let range = erange dict in
       let dict_addr = addr_of_object @@ object_of_expr dict in
       get_env T_cur man flow >>$? fun cur flow ->
       let ds = find dict_addr cur in
       (* ok as ds should have <= 1 element. Otherwise need to fold *)
       let dict = Dicts.choose ds in
       let ret = List.map (fun (k, v) -> mk_py_object k range, mk_py_object v range) dict in
       Some (Cases.singleton ret flow)

    | _ -> None
end

let () = register_standard_domain(module Domain)
