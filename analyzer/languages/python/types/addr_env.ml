(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

(** An environment is a total map from variables to addresses. *)

open Mopsa
open Sig.Abstraction.Domain
open Ast
open Addr
open Universal.Ast
open Data_model.Attribute
open Alarms


(* FIXME: can iterators over addresses be renamed? *)


(*==========================================================================*)
(**                            {2 Addresses}                                *)
(*==========================================================================*)

module PyAddr =
struct
  type t =
    | Def of addr (* defined address *)
    | Undef_local (* initial undefined value of local variables *)
    | Undef_global (* initial undefined value of global variables *)

  (** Address comparator. *)
  let compare a1 a2 =
    match a1, a2 with
    | Def addr1, Def addr2 -> compare_addr addr1 addr2
    | _ -> compare a1 a2

  let print printer =
    function
    | Def a -> unformat pp_addr printer a
    | Undef_local -> pp_string printer "UndefLocal"
    | Undef_global -> pp_string printer "UndefGlobal"
end


type addr_partitioning +=
  | G_py_bool of bool option
  | G_group of addr * addr



let () =
  register_addr_partitioning {
    print = (fun next fmt g ->
        match g with
        | G_py_bool (Some true) -> Format.fprintf fmt "true"
        | G_py_bool (Some false) -> Format.fprintf fmt "false"
        | G_py_bool None -> Format.fprintf fmt "⊤"
        | G_group (a1, a2) -> Format.fprintf fmt "(%a, %a)" pp_addr a1 pp_addr a2
        | _ -> next fmt g
      );
    compare = (fun next g1 g2 ->
        match g1, g2 with
        | G_py_bool b1, G_py_bool b2 -> OptionExt.compare Stdlib.compare b1 b2
        | G_group (a1, b1), G_group (a2, b2) ->
          Compare.compose
            [(fun () -> compare_addr a1 a2);
             (fun () -> compare_addr b1 b2);
            ]
        | _ -> next g1 g2
      );
  }


let addr_none = ref None
let addr_notimplemented = ref None
let addr_ellipsis = ref None
let addr_integers = ref None
let addr_float = ref None
let addr_strings = ref None
let addr_bytes = ref None
let addr_true = ref None
let addr_false = ref None
let addr_bool_top = ref None


module Domain =
struct


  module ASet =
    (struct
      module PS = Framework.Lattices.Powerset.Make(PyAddr)
      include PS
      let undef a = match a with
        | PyAddr.Def _ -> false
        | _ -> true

      let widen ctx = join
    end)

  module AMap = Framework.Lattices.Partial_map.Make
      (struct type t = var let compare = compare_var let print = pp_variable ~path:[] end)
      (ASet)

  include AMap
  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.types.addr_env"
    end)

  let subset = AMap.subset

  let join = AMap.join
  let meet = AMap.meet
  let widen = AMap.widen

  let checks = []

  let merge _ _ _ = assert false

  let init prog man flow =
    addr_none := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "NoneType"); addr_mode = STRONG};
    addr_notimplemented := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "NotImplementedType"); addr_mode = STRONG};
    addr_ellipsis := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "ellipsis"); addr_mode = STRONG};
    addr_integers := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "int"); addr_mode = WEAK};
    addr_float := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "float"); addr_mode = WEAK};
    addr_strings := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "str"); addr_mode = WEAK};
    addr_bytes := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "bytes"); addr_mode = WEAK};
    addr_true := Some {addr_partitioning = G_py_bool (Some true); addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = STRONG};
    addr_false := Some {addr_partitioning = G_py_bool (Some false); addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = STRONG};
    addr_bool_top := Some {addr_partitioning = G_py_bool None; addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = WEAK};
    set_env T_cur empty man flow |>
    Option.some

  let fold_intfloatstr man v flow fstmt =
    get_env T_cur man flow >>$ fun cur flow ->
    let oaset = AMap.find_opt v cur in
    match oaset with
    | None ->
       Post.return flow
    | Some aset ->
       if ASet.is_top aset then Post.return flow else
       let check_baddr a = ASet.mem (Def (OptionExt.none_to_exn !a)) aset in
       let intb = check_baddr addr_integers || check_baddr addr_true || check_baddr addr_false || check_baddr addr_bool_top || ASet.exists (function | Def a -> compare_addr_kind (akind @@ OptionExt.none_to_exn !addr_integers) (akind a) = 0 | _ -> false) aset in
       let float = check_baddr addr_float || ASet.exists (function | Def a -> compare_addr_kind (akind @@ OptionExt.none_to_exn !addr_float) (akind a) = 0 | _ -> false) aset in
       let str = check_baddr addr_strings in
       let bytes = check_baddr addr_bytes in
       (if str then let () = debug "processing %a for strings" pp_stmt (fstmt T_string) in man.exec   (fstmt T_string) flow  else Post.return flow) >>% fun flow ->
       (if bytes && not str then let () = debug "processing %a for bytes" pp_stmt (fstmt T_string) in man.exec   (fstmt T_string) flow  else Post.return flow) >>% fun flow ->
       (if intb then let () = debug "processing %a for int/bools" pp_stmt (fstmt T_int) in man.exec  (fstmt T_int) flow else Post.return flow) >>% fun flow ->
       (if float then let ()  = debug "processing %a for floats" pp_stmt (fstmt (T_float F_DOUBLE)) in man.exec  (fstmt (T_float F_DOUBLE)) flow else Post.return flow)

  let rec exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_add ({ekind = E_var (v, mode)} as e) when is_py_exp e ->
       get_env T_cur man flow >>$? fun cur flow ->
       if mem v cur then
         flow |> Post.return |> OptionExt.return
       else
         let ncur = add v (ASet.singleton Undef_global) cur in
         set_env T_cur ncur man flow |>
         OptionExt.return

    | S_project vs when List.for_all (fun e -> match ekind e with E_var _ -> true | _ -> false) vs ->
       (* Some other domains may create variables depending on vs.
          We first query them using Q_variables_linked_to before doing the real projection *)
       let vars = List.fold_left (fun acc v ->
                      debug "asking for variables linked to %a" pp_expr v;
                      let acc = VarSet.add (match ekind v with E_var (v, _) -> v | _ -> assert false) acc in
                      VarSet.union acc (ask_and_reduce man.ask (Q_variables_linked_to v) flow)
                    ) VarSet.empty vs in
       get_env T_cur man flow >>$? fun cur flow ->
       let addrs = VarSet.fold (fun var acc ->
                       match find_opt var cur with
                       | None -> acc
                       | Some aset -> ASet.join acc aset) vars ASet.empty in
       (* FIXME: heap is not cleaned, is that normal? *)
       (* FIXME: cleaning strings+int/float doesn't work. Should they be in product? *)
       (* FIXME: issue with class and function declarations... *)
       let ncur = VarSet.fold (fun var ncur ->
                      match var.vtyp with
                      | T_py _ ->
                         begin match find_opt var cur with
                         | None -> ncur
                         | Some aset -> add var aset ncur
                         end
                      | _ -> ncur
                    ) vars empty in
       let project_addrs = ASet.fold (fun pya acc -> match pya with
                                                     | Def a -> mk_addr a range :: acc
                                                     | _ -> acc) addrs [] in
       let project_num = VarSet.filter (fun v -> match v.vtyp with
                                                 | T_int | T_float _ -> true
                                                 | _ -> false) vars in
       debug "project_num = %a" (VarSet.fprint SetExt.printer_default pp_var) project_num;
       let project_str = VarSet.filter (fun v -> match v.vtyp with
                                                 | T_string -> true
                                                 | _ -> false) vars in
       debug "project_str = %a" (VarSet.fprint SetExt.printer_default pp_var) project_str;
       flow |>
         (if List.length project_addrs = 0 then fun x -> Post.return x
          else man.exec (mk_project project_addrs range))  |> post_to_flow man |>
         (if VarSet.is_empty project_num then fun x -> Post.return x
          else man.exec ~route:(Below name) (mk_project_vars (VarSet.elements project_num) range)) |> post_to_flow man |>
         (if VarSet.is_empty project_str then fun x -> Post.return x
          else man.exec ~route:(Below name) (mk_project_vars (VarSet.elements project_str) range)) |> post_to_flow man |>
         set_env T_cur ncur man |>
         OptionExt.return

    | S_assign(({ekind = E_var (v, vmode)} as vl), ({ekind = E_var (w, wmode)} as wl)) when is_py_exp vl && var_mode v vmode = WEAK && var_mode w wmode = WEAK ->
       get_env T_cur man flow >>$? fun cur flow ->
       begin match AMap.find_opt w cur with
       | None -> Post.return flow
       | Some aset ->
          (* FIXME: what happens if they are multiple float/... instances? *)
          let flow = ASet.fold
                       (fun pyaddr flow ->
                         let cstmt = fun t -> {stmt with skind = S_assign(Utils.change_evar_type t vl, Utils.change_evar_type t wl)} in
                         match pyaddr with
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "str", _)}} ->
                            flow >>% man.exec ~route:(Semantic "Universal") (cstmt T_string)
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "bytes", _)}} ->
                            flow >>% man.exec ~route:(Semantic "Universal") (cstmt T_string)
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)}}
                           | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}} ->
                            flow >>% man.exec ~route:(Semantic "Universal") (cstmt T_int)
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "float", _)}}  ->
                            flow >>% man.exec ~route:(Semantic "Universal") (cstmt (T_float F_DOUBLE))
                         | _ -> flow
                       ) aset (Post.return flow) in
          flow >>% fun flow ->
          if mem v cur then
            set_env T_cur (add v (ASet.join (find w cur) (find v cur)) cur) man flow
          else
            set_env T_cur (add v (find w cur) cur) man flow
       end
       |> OptionExt.return

    (* S⟦ v = e ⟧ *)
    | S_assign(({ekind = E_var (v, mode)} as evar), e) when is_py_exp e ->
       man.eval e flow |>
         bind_result
           (fun e flow ->
             match ekind e with
             | E_py_undefined true ->
                assign_addr man v PyAddr.Undef_global mode flow

             | E_py_undefined false ->
                assign_addr man v PyAddr.Undef_local mode flow

             | E_py_object (addr, None) ->
                assign_addr man v (PyAddr.Def addr) mode flow

             | E_py_object (addr, Some expr) ->
                assign_addr man v (PyAddr.Def addr) mode flow >>% fun flow ->
                debug "calling values for %a" pp_addr addr;
                begin match akind addr with
                | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                   man.exec ~route:(Semantic "Universal")   (mk_assign (Utils.change_evar_type T_string evar) expr range) flow
                | A_py_instance {addr_kind = A_py_class (C_builtin "bytes", _)} ->
                   man.exec ~route:(Semantic "Universal")  (mk_assign (Utils.change_evar_type T_string evar) expr range) flow
                | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                  | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                   man.exec ~route:(Semantic "Universal")  (mk_assign (Utils.change_evar_type T_int evar) expr range) flow
                | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                   man.exec ~route:(Semantic "Universal")  (mk_assign (Utils.change_evar_type (T_float F_DOUBLE) evar) expr range) flow
                | _ ->
                   Post.return flow
                end

             | E_constant (C_top (T_py None)) ->
                get_env T_cur man flow >>$ fun cur flow ->
                let aset = ASet.top in
                set_env T_cur (add v aset cur) man flow


             | _ -> Exceptions.panic_at range "%a@\n" pp_expr e
           )
       |> OptionExt.return

    | S_py_annot ({ekind = E_var (v, mode)} as evar, e) ->
       (* need to make e E_py_annot here or on the frontend *)
       (* then handle E_py_annot in typing, to perform an allocation? *)
       (* what about non builtins? *)
       man.eval e flow |>
         bind_result (fun e flow -> match ekind e with
                                  | E_py_object (addr, _) ->
                                     begin match akind addr with
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                                        man.exec   (mk_assign (Utils.change_evar_type T_string evar) (mk_top T_string range) range) flow
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "bytes", _)} ->
                                        man.exec   (mk_assign (Utils.change_evar_type T_string evar) (mk_top T_string range) range) flow
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                                       | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                                        man.exec  (mk_assign (Utils.change_evar_type T_int evar) (mk_top T_int range) range) flow
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                                        man.exec  (mk_assign (Utils.change_evar_type (T_float F_DOUBLE) evar) (mk_top (T_float F_DOUBLE) range) range) flow
                                     | _ ->
                                        Post.return flow
                                     end >>% fun flow ->
                                       assign_addr man v (PyAddr.Def addr) mode flow
                                  | _ -> Exceptions.panic_at range "%a@\n" pp_expr e)
       |> OptionExt.return

    | S_remove ({ekind = E_var (v, _)} as var) when is_py_exp var ->
       (fold_intfloatstr man v flow (fun t -> mk_remove_var (Utils.change_var_type t v) range) >>% fun flow ->
       begin
         get_env T_cur man flow >>$ fun cur flow ->
         set_env T_cur (AMap.remove v cur) man flow >>% fun flow ->
         begin match v.vkind with
         | V_uniq _ when not (Hashtbl.mem type_aliases v) ->
            (* FIXME: "remove var, assign to E_py_undefined?"; *)
            (* if the variable maps to a list, we should remove the temporary variable associated, ONLY if it's not used by another list *)
            let flow = man.exec (mk_assign var (mk_expr ~etyp:(T_py None) (E_py_undefined true) range) range) flow in
            flow

         | _ ->
            flow |> Post.return
         end
       end)
       |> OptionExt.return

    | S_assume e when is_py_exp e ->
       man.eval e flow |>
         bind_result (fun expr flow ->
             match ekind expr with
             | E_constant (C_top (T_py (Some Bool)))
               | E_constant (C_bool true)
               -> Post.return flow

             | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_true) = 0 || compare_addr a (OptionExt.none_to_exn !addr_bool_top) = 0
               ->
                Post.return flow

             | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_false) = 0
               ->
                Post.return (Flow.set T_cur man.lattice.bottom man.lattice flow)

             | E_constant (C_bool false) ->
                Post.return (Flow.set T_cur man.lattice.bottom man.lattice flow)

             | _ ->
                Exceptions.panic_at range "todo addr_env/assume on %a@\n" pp_expr e
           )
       |> OptionExt.return

    | S_rename (({ekind = E_var (v, mode)} as l), ({ekind = E_var (v', mode')} as r)) when is_py_exp l ->
       (* FIXME: modes, rename in weak shouldn't erase the old v'? *)
       (fold_intfloatstr man v flow (fun t ->
                      {stmt with skind = S_rename (Utils.change_evar_type t l,
                                                   Utils.change_evar_type t r)}) >>% fun flow ->
       begin
         get_env T_cur man flow >>$ fun cur flow ->
         begin match AMap.find_opt v cur with
         | Some aset ->
            set_env T_cur (AMap.rename v v' cur) man flow
         | None ->
            begin match v.vkind with
            | V_addr_attr(a, _) when Objects.Data_container_utils.is_data_container a.addr_kind ->
               (* because the container abstraction may not bind v to anything if the container is empty *)
               flow |> Post.return
            | _ -> assert false (* shouldn't happen *)
            end
         end
       end) |> OptionExt.return

    | S_fold (({ekind = E_var (v, mode)} as l), vars) when is_py_exp l ->
       get_env T_cur man flow >>$? fun cur flow ->
       let av = OptionExt.default ASet.empty (AMap.find_opt v cur) in
       let new_av, flow = List.fold_left (fun (av, flow) ev' ->
                              let flow = post_to_flow man flow in
                              let v' = match ekind ev' with
                                | E_var (v', _) -> v'
                                | E_py_object(_, Some {ekind = E_var (v', _)}) -> v'
                                | _ -> assert false in
                              match AMap.find_opt v' cur with
                              | None -> av, Post.return flow
                              | Some av' ->
                                 ASet.join av av',
                                 fold_intfloatstr man v' flow (fun t -> mk_fold_var (Utils.change_var_type t v) [(Utils.change_var_type t v')] range) >>% fun flow ->
                                 set_env T_cur (AMap.remove v' cur) man flow
                            ) (av, Post.return flow) vars in
       let flow = post_to_flow man flow in
       let post =
         if ASet.is_empty new_av then Post.return flow
         else map_env T_cur (AMap.add v new_av) man flow in
       Some post
       

    | S_expand (({ekind = E_var (v, mode)} as l), vars) when is_py_exp l ->
       get_env T_cur man flow >>$? fun cur flow ->
       begin match AMap.find_opt v cur with
       | None -> warn_at range "weird expand on %a" pp_var v; Post.return flow
       | Some addrs_v ->
          let varset = VarSet.of_list (List.map (fun evars -> match ekind evars with
                                                              | E_var (v, _) -> v
                                                              | _ -> assert false) vars
                         ) in
          let ncur = AMap.mapi (fun var aset -> if VarSet.mem var varset then ASet.join aset addrs_v else aset) cur in
          let addrs_v = AMap.find v ncur in
          let ncur = VarSet.fold (fun vvarset ncur -> AMap.add vvarset addrs_v ncur) varset ncur in
          set_env T_cur ncur man flow >>% fun flow ->
          fold_intfloatstr man v flow (fun t -> mk_expand_var (Utils.change_var_type t v) (List.map (fun v -> Utils.change_var_type t v) (VarSet.elements varset)) range)
       end
       |> OptionExt.return

    | S_expand (({ekind = E_addr (a, _)} as e1), addrs) ->
       get_env T_cur man flow >>$? fun cur flow ->
       let addrs_aset = List.map (fun eaddr -> match ekind eaddr with
                                               | E_addr (addr, _) -> PyAddr.Def addr
                                               | _ -> assert false) addrs |> ASet.of_list in
       let ncur = AMap.map
                    (fun aset ->
                      if ASet.mem (Def a) aset then
                        ASet.join addrs_aset aset
                      else aset
                    ) cur in
       set_env T_cur ncur man flow >>%? fun flow ->
       let flow = Flow.fold (fun acc tk d ->
              match tk with
              | T_py_exception ({ekind = E_py_object (oa, oe)} as e, s, m, k) when compare_addr a oa = 0 ->
                 List.fold_left (fun acc ea' ->
                     match ekind ea' with
                     | E_addr (a', _) ->
                        Flow.add (T_py_exception ({e with ekind = E_py_object (a', oe)}, s, m, k)) d man.lattice (Flow.add tk d man.lattice acc)
                     | _ -> assert false) acc addrs
              | _ -> Flow.add tk d man.lattice acc) (Flow.bottom (Flow.get_ctx flow) (Flow.get_report flow)) flow in
       begin match akind a with
       | A_py_instance {addr_kind = A_py_class (C_annot _, _)} ->
          let skind = S_expand ({e1 with ekind = E_py_annot e1}, addrs) in
          man.exec ~route:(Below name) stmt flow >>%
            man.exec   {stmt with skind}
       | A_py_instance _ ->
          man.exec ~route:(Below name) stmt flow
       | ak when Objects.Data_container_utils.is_data_container ak ->
          man.exec ~route:(Below name) stmt flow
       | _ -> Post.return flow
       end
       |> OptionExt.return


    | S_fold (({ekind = E_addr (a', om')} as e2), [{ekind = E_addr (a, om)} as e1])
      | S_rename (({ekind = E_addr (a, om)} as e1), ({ekind = E_addr (a', om')} as e2)) ->
       get_env T_cur man flow >>$? fun cur flow ->
       let rename addr = if PyAddr.compare addr (PyAddr.Def a) = 0 then PyAddr.Def a' else addr in
       let ncur = AMap.map (ASet.map rename) cur in
       set_env T_cur ncur man flow >>%? fun flow ->
       let to_rename = Flow.fold (fun acc tk d ->
                           match tk with
                           | T_py_exception ({ekind = E_py_object _}, _, _, _) -> true
                           | _ -> acc) false flow in
       let flow =
         if to_rename then
           Flow.fold (fun acc tk d ->
               match tk with
               | T_py_exception ({ekind = E_py_object (oa, oe)} as e, s, m, k) when compare_addr a oa = 0 ->
                  Flow.add (T_py_exception ({e with ekind = E_py_object (a', oe)}, s, m, k)) d man.lattice acc
               | _ -> Flow.add tk d man.lattice acc) (Flow.bottom (Flow.get_ctx flow) (Flow.get_report flow)) flow
         else
           flow in
       begin match akind a with
       | A_py_instance {addr_kind = A_py_class (C_annot _, _)} ->
          let skind = match skind stmt with
            | S_fold _ -> S_fold ({e2 with ekind = E_py_annot e2}, [e1])
            | S_rename _ -> S_rename ({e1 with ekind = E_py_annot e1}, e2)
            | _ -> assert false in
          man.exec ~route:(Below name) stmt flow >>%
            man.exec   {stmt with skind}
       | A_py_instance _ ->
          debug "instance encountered, renaming methods too";
          (* FIXME: PERF: in function.ml, store inst -> method binding and use it here? / ask function.ml to perform the stmt? *)
          let aaddr = ask_and_reduce man.ask Q_allocated_addresses flow in
          let flow = List.fold_left (fun flow addr ->
                         match akind addr with
                         | A_py_method(func, (ainst, oe), mclass) when compare_addr a ainst = 0 ->
                            let addr' = {addr with addr_kind = A_py_method(func, (a', oe), mclass)} in
                            let skind = match skind stmt with
                              | S_fold _ -> S_fold ({e2 with ekind = E_addr (addr', om')}, [{e1 with ekind = E_addr (addr, om)}])
                              | S_rename _ -> S_rename ({e1 with ekind = E_addr (addr, om)}, {e2 with ekind = E_addr (addr', om')})
                              | _ -> assert false
                            in
                            flow >>% man.exec   {stmt with skind}
                         | _ -> flow
                       ) (Post.return flow) aaddr in
          flow >>% man.exec ~route:(Below name) stmt
       | ak when Objects.Data_container_utils.is_data_container ak ->
          man.exec ~route:(Below name) stmt flow
       | _ -> Post.return flow
       end |> OptionExt.return

    | S_py_delete {ekind = E_var (v, _)} ->
       man.exec (mk_remove_var v range) flow |> OptionExt.return

    | S_py_delete {ekind = E_py_index_subscript(v, index)} ->
       man.exec (mk_expr_stmt (mk_py_call (mk_py_attr v "__delitem__" range) [index] range) range) flow
       |> OptionExt.return

    | S_py_delete {ekind = E_py_attribute(v, attr)} ->
       man.exec (mk_expr_stmt (mk_py_call (mk_py_attr v "__delattr__" range) [mk_string ~etyp:(T_py None) attr range] range) range) flow
       |> OptionExt.return


    | S_remove {ekind = E_addr ({addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin s, _)}}, _)} when List.mem s ["int"; "float"; "bool"; "NoneType"; "NotImplementedType"; "str"; "bytes"] ->
       flow |> Post.return |> OptionExt.return

    | S_invalidate {ekind = E_addr (a, _)}
    | S_remove {ekind = E_addr (a, _)} ->
       get_env T_cur man flow >>$? fun cur flow ->
       let ncur = AMap.map (ASet.remove (Def a)) cur in
       set_env T_cur ncur man flow >>%? fun flow ->
       begin match akind a with
       | A_py_instance _ ->
          man.exec ~route:(Below name) stmt flow
       | ak when Objects.Data_container_utils.is_data_container ak ->
          man.exec ~route:(Below name) stmt flow
       | _ -> Post.return flow
       end |> OptionExt.return

    | _ -> None

  and assign_addr man v av mode flow =
    get_env T_cur man flow >>$ fun cur flow ->
    let aset = match var_mode v mode with
      | STRONG -> ASet.singleton av
      | WEAK ->
         ASet.add av (OptionExt.default ASet.empty (find_opt v cur))
    in
    set_env T_cur (add v aset cur) man flow


  let get_builtin bltin =
    let obj = find_builtin bltin in
    match kind_of_object obj with
    | A_py_class (c, b) -> (c, b)
    | _ -> assert false

  let bltin_inst bltin =
    let obj = find_builtin bltin in A_py_instance (fst obj)

  let allocate_builtin ?(mode=STRONG) man range flow bltin oe =
    (* allocate addr, and map this addr to inst bltin *)
    let range = tag_range range "alloc_%s" bltin in
    let cls = fst @@ find_builtin bltin in
    man.eval   (mk_alloc_addr ~mode:mode (A_py_instance cls) range) flow >>$
      (fun eaddr flow ->
        let addr = Addr.from_expr eaddr in
        man.exec   (mk_add eaddr range) flow >>%
          Eval.singleton (mk_py_object (addr, oe) range)
      )

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_var (v, mode) when is_py_exp exp ->
      get_env T_cur man flow >>$? fun cur flow ->
      if AMap.mem v cur then
        let aset = AMap.find v cur in
        if ASet.is_top aset then
          Eval.singleton (mk_top (T_py None) range) flow |> OptionExt.return
        else
        let evals, annot = ASet.fold (fun a (acc, annots) ->
            let post = if v.vmode = WEAK then Post.return flow else set_env T_cur (AMap.add v (ASet.singleton a) cur) man flow in
            let post = Cases.set_ctx annots post in
            match a with
            | Undef_global when is_builtin_var v ->
               ( post >>% fun flow ->
                 Flow.add_safe_check Alarms.CHK_PY_NAMEERROR range flow |>
                 Flow.add_safe_check Alarms.CHK_PY_UNBOUNDLOCALERROR range |>
                 Eval.singleton (mk_py_object (find_builtin (get_orig_vname v)) range)) :: acc, annots

            | Undef_local when is_builtin_var v ->
               ( post >>% fun flow ->
                 Flow.add_safe_check Alarms.CHK_PY_NAMEERROR range flow |>
                  Flow.add_safe_check Alarms.CHK_PY_UNBOUNDLOCALERROR range |>
                  Eval.singleton (mk_py_object (find_builtin @@ get_orig_vname v) range)) :: acc, annots

            | Undef_global ->
              let msg = Format.asprintf "name '%a' is not defined" pp_var v in
              let flow = post_to_flow man (post >>% man.exec (Utils.mk_builtin_raise_msg "NameError" msg range)) in
              Eval.empty flow :: acc, Flow.get_ctx flow

            | Undef_local ->
              let msg = Format.asprintf "local variable '%a' referenced before assignment" pp_var v in
              let flow = post_to_flow man (post >>% man.exec (Utils.mk_builtin_raise_msg "UnboundLocalError" msg range)) in
              Eval.empty flow :: acc, Flow.get_ctx flow

            | Def addr ->
              (* first, let's clean numerical/string variables from other addrs *)
              let post = if v.vmode = WEAK then post else ASet.fold (fun a' post ->
                  match a' with
                  | Def addr' when compare_addr_kind (akind addr) (akind addr') <> 0 -> (* only the kind. If two str addrs, we can't remove anything *)
                    debug "removing other numerical vars";
                    begin match akind addr' with
                      | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)}
                      | A_py_instance {addr_kind = A_py_class (C_builtin "bytes", _)} ->
                        begin match akind addr with
                          | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)}
                          | A_py_instance {addr_kind = A_py_class (C_builtin "bytes", _)} ->
                            post 

                          | _ -> post >>% man.exec   (mk_remove_var (Utils.change_var_type T_string v) range)
                        end

                      | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                      | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                        begin match akind addr with
                          | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                          | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                            post

                          | _ ->
                            post >>% man.exec  (mk_remove_var (Utils.change_var_type T_int v) range)
                        end
                      | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                        post >>% man.exec   (mk_remove_var (Utils.change_var_type (T_float F_DOUBLE) v) range)
                      | _ ->
                        post
                    end
                  | _ -> post 
                ) aset post in
              let exp = match akind addr with
                | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)}
                | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)} ->
                  Utils.change_evar_type T_int exp
                | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                  Utils.change_evar_type (T_float F_DOUBLE) exp
                | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                  Utils.change_evar_type T_string exp
                | A_py_instance {addr_kind = A_py_class (C_builtin "bytes", _)} ->
                  Utils.change_evar_type T_string exp
                | _ -> exp in
              let res =
                post >>% fun flow ->
                Flow.add_safe_check Alarms.CHK_PY_NAMEERROR range flow |>
                Flow.add_safe_check Alarms.CHK_PY_UNBOUNDLOCALERROR range |>
                man.eval (mk_py_object (addr, Some exp) range)
              in
              let annots = Cases.get_ctx res in
              res :: acc, annots

          ) aset ([], Flow.get_ctx flow) in
        let evals = List.map (Cases.set_ctx annot) evals in
        evals |> Eval.join_list ~empty:(fun () -> Eval.empty flow)
        |> OptionExt.return
      else if is_builtin_var v then
        let () = debug "is it a builtin?" in
        let obj = find_builtin @@ get_orig_vname v in
        let flow =
          Flow.add_safe_check Alarms.CHK_PY_NAMEERROR range flow |>
            Flow.add_safe_check Alarms.CHK_PY_UNBOUNDLOCALERROR range in
        Eval.singleton (mk_py_object obj range) flow |> OptionExt.return
      else if is_bottom cur then
        let () = debug "cur to bottom, empty singleton" in
        Eval.empty flow |> OptionExt.return
      else
        let () = debug "%a %a not a builtin..." pp_range range pp_expr exp in
        Eval.empty flow |>
        OptionExt.return

    (* todo: should be moved to zone system? useless? *)
    | E_py_object ({addr_kind = A_py_instance _}, _) ->
      Eval.singleton exp flow |> OptionExt.return

    | E_constant C_py_none ->
      Eval.singleton (mk_py_object (OptionExt.none_to_exn !addr_none, None) range) flow |> OptionExt.return

    | E_constant C_py_ellipsis ->
      Eval.singleton (mk_py_object (OptionExt.none_to_exn !addr_ellipsis, None) range) flow |> OptionExt.return

    | E_constant C_py_not_implemented ->
      Eval.singleton (mk_py_object (OptionExt.none_to_exn !addr_notimplemented, None) range) flow |> OptionExt.return

    | E_unop(O_log_not, e') when is_py_exp exp ->
      begin match ekind e' with
      | E_constant (C_top (T_py (Some Bool))) ->
         Eval.singleton e' flow |> OptionExt.return
      | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_true) = 0 ->
         Eval.singleton (mk_py_object (OptionExt.none_to_exn !addr_false, Some (mk_int 0 ~typ:T_int range)) range) flow |> OptionExt.return
      | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_false) = 0 ->
         Eval.singleton (mk_py_object (OptionExt.none_to_exn !addr_true, Some (mk_int 1 ~typ:T_int range)) range) flow |> OptionExt.return
      | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_bool_top) = 0 ->
         Eval.singleton e' flow |> OptionExt.return
      | _ ->
       (* bool is called in desugar/bool *)
       man.eval e' flow >>$
         (fun ee' flow ->
           match ekind ee' with
           (* FIXME: weird cases *)
           | E_constant (C_top (T_py (Some Bool))) ->
              Eval.singleton ee' flow
           | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_true) = 0 ->
              Eval.singleton (mk_py_object (OptionExt.none_to_exn !addr_false, Some (mk_int 0 ~typ:T_int range)) range) flow
           | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_false) = 0 ->
              Eval.singleton (mk_py_object (OptionExt.none_to_exn !addr_true, Some (mk_int 1 ~typ:T_int range)) range) flow
           | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_bool_top) = 0 ->
              Eval.singleton ee' flow
           | _ ->
              panic_at range "o_log_not ni on %a" pp_expr ee'
         )
       |> OptionExt.return
      end

    | E_binop(O_py_is, e1, e2) ->
      bind_list [e1;e2] (man.eval  ) flow |>
      bind_result (fun evals flow ->
          let e1, e2 = match evals with [e1;e2] -> e1, e2 | _ -> assert false in
          begin match ekind e1, ekind e2 with
            | E_py_object (a1, _), E_py_object (a2, _) when
                compare_addr a1 a2 = 0 &&
                  (compare_addr a1 (OptionExt.none_to_exn !addr_notimplemented) = 0 || compare_addr a1 (OptionExt.none_to_exn !addr_none) = 0 || a1.addr_mode = STRONG) ->
              man.eval (mk_py_true range) flow
            | E_py_object (a1, _), E_py_object (a2, _) when compare_addr_kind (akind a1) (akind a2) <> 0 ->
              man.eval (mk_py_false range) flow
            | _ ->
               debug "e1 = %a, e2 = %a" pp_expr e1 pp_expr e2;
               man.eval (mk_py_top (T_py (Some Bool)) range) flow
          end
        )
      |> OptionExt.return

    | _ -> None

  let ask : type r. ('a, r) query -> ('a, t) man -> 'a flow -> ('a, r) cases option =
    fun query man flow ->
      match query with
      | Universal.Heap.Recency.Q_alive_addresses ->
           get_env T_cur man flow >>$? fun cur flow ->
           let aset = AMap.fold (fun var aset acc ->
                          ASet.join aset acc) cur ASet.empty in
           let ret =
             List.rev @@
             ASet.fold (fun pyaddr acc -> match pyaddr with
                                          | Def a -> a :: acc
                                          | _ -> acc) aset []
           in
           Some (Cases.singleton ret flow)

      | Universal.Heap.Recency.Q_alive_addresses_aspset ->
         get_env T_cur man flow >>$? fun cur flow ->
         let aset = AMap.fold (fun _ aset acc ->
                        ASet.join aset acc) cur ASet.empty in
         let ret =
           ASet.fold (fun pyaddr acc -> match pyaddr with
                                      | Def a -> Universal.Heap.Recency.Pool.add a acc
                                      | _ -> acc) aset Universal.Heap.Recency.Pool.empty
         in
         Some (Cases.singleton ret flow)


      | Q_variables_linked_to ({ekind = E_var(v, _)} as e) ->
         get_env T_cur man flow >>$? fun cur flow ->
         let acc = VarSet.add v VarSet.empty in
         begin match AMap.find_opt v cur with
         | None ->
            Some (Cases.singleton acc flow)
         | Some aset ->
             let r =
               ASet.fold
                 (fun pyaddr acc ->
                   match pyaddr with
                   | Def a ->
                      debug "asking for %a" pp_addr a;
                      VarSet.union acc (ask_and_reduce man.ask (Q_variables_linked_to (mk_addr a e.erange)) flow)
                   | _ -> acc) aset VarSet.empty in

             let check_baddr a  = ASet.mem (Def (OptionExt.none_to_exn !a)) aset in
             let intb = check_baddr addr_integers || check_baddr addr_true || check_baddr addr_false || check_baddr addr_bool_top in
             let float = check_baddr addr_float || ASet.exists (function | Def a -> compare_addr_kind (akind @@ OptionExt.none_to_exn !addr_float) (akind a) = 0 | _ -> false) aset  in
             let str = check_baddr addr_strings in
             let bytes = check_baddr addr_bytes in
             let r = if intb then VarSet.add (Utils.change_var_type T_int v) r else r in
             let r = if float then VarSet.add (Utils.change_var_type (T_float F_DOUBLE) v) r else r in
             let r = if str then VarSet.add (Utils.change_var_type T_string v) r else r in
             let r = if bytes then VarSet.add (Utils.change_var_type T_string v) r else r in
             Some (Cases.singleton (VarSet.union acc r) flow)
         end

      | Framework.Engines.Interactive.Query.Q_debug_variable_value var ->
         let open Framework.Engines.Interactive.Query in
         get_env T_cur man flow >>$? fun cur flow ->
         begin match AMap.find_opt var cur with
         | None ->
            None
         | Some aset ->
            let subvalues =
              ASet.fold (fun pyaddr acc ->
                  match pyaddr with
                  | Def addr ->
                     let s = Format.asprintf "%a" (format PyAddr.print) pyaddr in
                     if List.exists (fun a' -> compare_addr  addr (OptionExt.none_to_exn a') = 0)
                          [!addr_none; !addr_notimplemented; !addr_true; !addr_false; !addr_bool_top] then
                       (s, {var_value = None; var_value_type = T_any; var_sub_value = None}) :: acc
                     else if compare_addr_kind (akind @@ OptionExt.none_to_exn !addr_integers) (akind addr) = 0 then
                       let itv = ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query (Utils.change_evar_type T_int (mk_var var (Location.mk_fresh_range ())))) flow in
                       let int_info = {var_value = Some (Format.asprintf "%a" Universal.Numeric.Common.pp_int_interval itv); var_value_type = T_int; var_sub_value = None} in
                       (s, int_info) :: acc
                     else if compare_addr_kind  (akind @@ OptionExt.none_to_exn !addr_float) (akind addr) = 0 then
                       let itv = ask_and_reduce man.ask (Universal.Numeric.Common.mk_float_interval_query (Utils.change_evar_type (T_float F_DOUBLE) (mk_var var (Location.mk_fresh_range ())))) flow in
                       let float_info = {var_value = Some (Format.asprintf "%a" Universal.Numeric.Common.pp_float_interval itv); var_value_type = T_float F_DOUBLE; var_sub_value = None} in
                       (s, float_info) :: acc
                     else if compare_addr_kind (akind @@ OptionExt.none_to_exn !addr_strings) (akind addr) = 0 then
                       let str = ask_and_reduce man.ask (Universal.Strings.Powerset.mk_strings_powerset_query (Utils.change_evar_type T_string (mk_var var (Location.mk_fresh_range ())))) flow in
                       let str_info =
                         {var_value = Some (Format.asprintf "%a" (format Universal.Strings.Powerset.StringPower.print) str);
                          var_value_type = T_string;
                          var_sub_value = None} in
                       (s, str_info) :: acc
                     else if compare_addr_kind (akind @@ OptionExt.none_to_exn !addr_strings) (akind addr) = 0 then
                       let str = ask_and_reduce man.ask (Universal.Strings.Powerset.mk_strings_powerset_query (Utils.change_evar_type T_string (mk_var var (Location.mk_fresh_range ())))) flow in
                       let str_info =
                         {var_value = Some (Format.asprintf "%a" (format Universal.Strings.Powerset.StringPower.print) str);
                          var_value_type = T_string;
                          var_sub_value = None} in
                       (s, str_info) :: acc
                     else
                       let addr_info = ask_and_reduce man.ask (Q_debug_addr_value addr) flow in
                       (s, addr_info) :: acc
                  | _ -> acc
                ) aset []
            in
            let r =
              {var_value = None;
               var_value_type = T_any;
               var_sub_value = Some (Named_sub_value subvalues)} in
            Some (Cases.singleton r flow)
         end

      | _ -> None


  let print_state printer m =
    pprint ~path:[Key "addrs"] printer (pbox AMap.print m)


  let print_expr man flow printer exp =
    if not (is_py_exp exp) then () else
    match ekind exp with
    | E_var (v, m) ->
       Cases.iter_result (fun res flow ->
           match ekind res with
           | E_py_object (addr, oe) ->
              man.print_expr flow printer {res with ekind = E_addr (addr, None)};
              pprint printer ~path:[ Key "environment"; fkey "%a" pp_var v ] (pbox (unformat pp_addr) addr);
              begin match oe with
              | Some e when List.exists (fun a -> compare_addr_kind (akind @@ OptionExt.none_to_exn !a) (akind addr) = 0) [addr_integers; addr_float; addr_strings] ->
                 (* FIXME: class A: pass; a = A() yields <<@Inst{A} :: a>> *)
                 man.print_expr flow printer e
              | _ -> ()
              end
           | _ -> assert false
         ) (man.eval exp flow);


    | _ -> ()
end

let () =
  register_standard_domain (module Domain);
