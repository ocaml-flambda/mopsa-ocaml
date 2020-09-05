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

  let print fmt =
    function
    | Def a -> Universal.Ast.pp_addr fmt a
    | Undef_local -> Format.fprintf fmt "UndefLocal"
    | Undef_global -> Format.fprintf fmt "UndefGlobal"
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
let addr_integers = ref None
let addr_float = ref None
let addr_strings = ref None
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

      let widen = join
    end)

  module AMap = Framework.Lattices.Partial_map.Make
      (struct type t = var let compare = compare_var let print = pp_var end)
      (ASet)

  include AMap
  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.types.addr_env"
    end)

  let subset = AMap.subset

  let join = AMap.join
  let meet = AMap.meet
  let widen uctx = AMap.widen

  let alarms = []

  let merge _ _ _ = assert false

  let print fmt m =
    Format.fprintf fmt "addrs: @[%a@]@\n" AMap.print m

  let init prog man flow =
    addr_none := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "NoneType"); addr_mode = STRONG};
    addr_notimplemented := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "NotImplementedType"); addr_mode = STRONG};
    addr_integers := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "int"); addr_mode = WEAK};
    addr_float := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "float"); addr_mode = WEAK};
    addr_strings := Some {addr_partitioning = G_all; addr_kind = A_py_instance (fst @@ find_builtin "str"); addr_mode = WEAK};
    addr_true := Some {addr_partitioning = G_py_bool (Some true); addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = STRONG};
    addr_false := Some {addr_partitioning = G_py_bool (Some false); addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = STRONG};
    addr_bool_top := Some {addr_partitioning = G_py_bool None; addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = WEAK};
    set_env T_cur empty man flow

  let fold_intfloatstr man v flow fstmt =
    let cur = get_env T_cur man flow in
    let oaset = AMap.find_opt v cur in
    match oaset with
    | None ->
       Post.return flow
    | Some aset ->
       let check_baddr a = ASet.mem (Def (OptionExt.none_to_exn !a)) aset in
       let intb = check_baddr addr_integers || check_baddr addr_true || check_baddr addr_false || check_baddr addr_bool_top in
       let float = check_baddr addr_float in
       let str = check_baddr addr_strings in
       (if str then let () = debug "processing %a for strings" pp_stmt (fstmt T_string) in man.exec   (fstmt T_string) flow  else Post.return flow) >>% fun flow ->
       (if intb then let () = debug "processing %a for int/bools" pp_stmt (fstmt T_int) in man.exec  (fstmt T_int) flow else Post.return flow) >>% fun flow ->
       (if float then let ()  = debug "processing %a for floats" pp_stmt (fstmt (T_float F_DOUBLE)) in man.exec  (fstmt (T_float F_DOUBLE)) flow else Post.return flow)

  let rec exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_add ({ekind = E_var (v, mode)} as e) when is_py_exp e ->
       let cur = get_env T_cur man flow in
       if mem v cur then
         flow |> Post.return |> OptionExt.return
       else
         let ncur = add v (ASet.singleton Undef_global) cur in
         let () = debug "cur was %a@\nncur is %a@\n" print cur print ncur in
         let flow =  set_env T_cur ncur man flow in
         let () = debug "flow is now %a@\n" (Flow.print man.lattice.print) flow in
         flow |> Post.return |> OptionExt.return

    | S_assign(({ekind = E_var (v, vmode)} as vl), ({ekind = E_var (w, wmode)} as wl)) when is_py_exp vl && var_mode v vmode = WEAK && var_mode w wmode = WEAK ->
       let cur = get_env T_cur man flow in
       begin match AMap.find_opt w cur with
       | None -> Post.return flow
       | Some aset ->
          (* FIXME: what happens if they are multiple float/... instances? *)
          let flow = ASet.fold
                       (fun pyaddr flow ->
                         let cstmt = fun t -> {stmt with skind = S_assign(Utils.change_evar_type t vl, Utils.change_evar_type t wl)} in
                         match pyaddr with
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "str", _)}} ->
                            flow >>% man.exec   (cstmt T_string)
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)}}
                           | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}} ->
                            flow >>% man.exec  (cstmt T_int)
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "float", _)}}  ->
                            flow >>% man.exec  (cstmt (T_float F_DOUBLE))
                         | _ -> flow
                       ) aset (Post.return flow) in
          flow >>% fun flow ->
                   Post.return
                     begin
                       if mem v cur then
                         set_env T_cur (add v (ASet.join (find w cur) (find v cur)) cur) man flow
                       else
                         set_env T_cur (add v (find w cur) cur) man flow
                     end
       end
       |> OptionExt.return

    (* S⟦ v = e ⟧ *)
    | S_assign(({ekind = E_var (v, mode)} as evar), e) when is_py_exp e ->
       man.eval e flow |>
         bind_some
           (fun e flow ->
             match ekind e with
             | E_py_undefined true ->
                assign_addr man v PyAddr.Undef_global mode flow |> Post.return

             | E_py_undefined false ->
                assign_addr man v PyAddr.Undef_local mode flow |> Post.return

             | E_py_object (addr, None) ->
                assign_addr man v (PyAddr.Def addr) mode flow |> Post.return

             | E_py_object (addr, Some expr) ->
                let flow = assign_addr man v (PyAddr.Def addr) mode flow in
                debug "calling values for %a" pp_addr addr;
                begin match akind addr with
                | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                   man.exec   (mk_assign (Utils.change_evar_type T_string evar) expr range) flow
                | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                  | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                   man.exec  (mk_assign (Utils.change_evar_type T_int evar) expr range) flow
                | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                   man.exec  (mk_assign (Utils.change_evar_type (T_float F_DOUBLE) evar) expr range) flow
                | _ ->
                   Post.return flow
                end

             | E_constant (C_top (T_py None)) ->
                let cur = get_env T_cur man flow in
                let aset = ASet.top in
                set_env T_cur (add v aset cur) man flow
                |> Post.return


             | _ -> Exceptions.panic_at range "%a@\n" pp_expr e
           )
       |> OptionExt.return

    | S_py_annot ({ekind = E_var (v, mode)} as evar, e) ->
       (* need to make e E_py_annot here or on the frontend *)
       (* then handle E_py_annot in typing, to perform an allocation? *)
       (* what about non builtins? *)
       man.eval e flow |>
         bind_some (fun e flow -> match ekind e with
                                  | E_py_object (addr, _) ->
                                     begin match akind addr with
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                                        man.exec   (mk_assign (Utils.change_evar_type T_string evar) (mk_top T_string range) range) flow
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                                       | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                                        man.exec  (mk_assign (Utils.change_evar_type T_int evar) (mk_top T_int range) range) flow
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                                        man.exec  (mk_assign (Utils.change_evar_type (T_float F_DOUBLE) evar) (mk_top (T_float F_DOUBLE) range) range) flow
                                     | _ ->
                                        Post.return flow
                                     end >>% fun flow ->
                                       assign_addr man v (PyAddr.Def addr) mode flow |> Post.return
                                  | _ -> Exceptions.panic_at range "%a@\n" pp_expr e)
       |> OptionExt.return

    | S_remove ({ekind = E_var (v, _)} as var) when is_py_exp var ->
       (fold_intfloatstr man v flow (fun t -> mk_remove_var (Utils.change_var_type t v) range) >>% fun flow ->
       begin
         let cur = get_env T_cur man flow in
         let flow = set_env T_cur (AMap.remove v cur) man flow in
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
         bind_some (fun expr flow ->
             match ekind expr with
             | E_constant (C_top (T_py (Some Bool)))
               | E_constant (C_bool true)
               -> Post.return flow
             | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_true) = 0 || compare_addr a (OptionExt.none_to_exn !addr_bool_top) = 0
               -> Post.return flow
             | E_py_object (a, _) when compare_addr a (OptionExt.none_to_exn !addr_false) = 0
               -> Post.return (set_env T_cur bottom man flow)
             | E_constant (C_bool false) ->
                Post.return (set_env T_cur bottom man flow)
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
         let cur = get_env T_cur man flow in
         debug "after fold, flow = %a" (Flow.print man.lattice.print) flow;
         begin match AMap.find_opt v cur with
         | Some aset ->
            set_env T_cur (AMap.rename v v' cur) man flow |> Post.return
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
       let cur = get_env T_cur man flow in
       let av = OptionExt.default ASet.empty (AMap.find_opt v cur) in
       let new_av, flow = List.fold_left (fun (av, flow) ev' ->
                              let flow = post_to_flow man flow in
                              let ncur = get_env T_cur man flow in
                              Debug.debug ~channel:"addrenv" "av = %a@.ncur = %a" ASet.print av AMap.print ncur;
                              let v' = match ekind ev' with
                                | E_var (v', _) -> v'
                                | _ -> assert false in
                              match AMap.find_opt v' cur with
                              | None -> av, Post.return flow
                              | Some av' ->
                                 ASet.join av av',
                                 fold_intfloatstr man v' flow (fun t -> mk_fold_var (Utils.change_var_type t v) [(Utils.change_var_type t v')] range) >>% fun flow ->
                                 set_env T_cur (AMap.remove v' cur) man flow |> Post.return
                            ) (av, Post.return flow) vars in
       let flow = post_to_flow man flow in
       let flow =
         if ASet.is_empty new_av then flow
         else map_env T_cur (AMap.add v new_av) man flow in
       flow |> Post.return |> OptionExt.return

    | S_expand (({ekind = E_var (v, mode)} as l), vars) when is_py_exp l ->
       let cur = get_env T_cur man flow in
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
          let flow = set_env T_cur ncur man flow in
          fold_intfloatstr man v flow (fun t -> mk_expand_var (Utils.change_var_type t v) (List.map (fun v -> Utils.change_var_type t v) (VarSet.elements varset)) range)
       end
       |> OptionExt.return

    | S_expand (({ekind = E_addr a} as e1), addrs) ->
       let cur = get_env T_cur man flow in
       let addrs_aset = List.map (fun eaddr -> match ekind eaddr with
                                               | E_addr addr -> PyAddr.Def addr
                                               | _ -> assert false) addrs |> ASet.of_list in
       let ncur = AMap.map
                    (fun aset ->
                      if ASet.mem (Def a) aset then
                        ASet.join addrs_aset aset
                      else aset
                    ) cur in
       let flow = set_env T_cur ncur man flow in
       let flow = Flow.fold (fun acc tk d ->
              match tk with
              | T_py_exception ({ekind = E_py_object (oa, oe)} as e, s, k) when compare_addr a oa = 0 ->
                 List.fold_left (fun acc ea' ->
                     match ekind ea' with
                     | E_addr a' ->
                        Flow.add (T_py_exception ({e with ekind = E_py_object (a', oe)}, s, k)) d man.lattice (Flow.add tk d man.lattice acc)
                     | _ -> assert false) acc addrs
              | _ -> Flow.add tk d man.lattice acc) (Flow.bottom (Flow.get_ctx flow) (Flow.get_alarms flow)) flow in
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


    | S_fold (({ekind = E_addr a'} as e2), [{ekind = E_addr a} as e1])
      | S_rename (({ekind = E_addr a} as e1), ({ekind = E_addr a'} as e2)) ->
       let cur = get_env T_cur man flow in
       let rename addr = if PyAddr.compare addr (PyAddr.Def a) = 0 then PyAddr.Def a' else addr in
       let ncur = AMap.map (ASet.map rename) cur in
       let flow = set_env T_cur ncur man flow in
       let to_rename = Flow.fold (fun acc tk d ->
                           match tk with
                           | T_py_exception ({ekind = E_py_object _}, _, _) -> true
                           | _ -> acc) false flow in
       let flow =
         if to_rename then
           Flow.fold (fun acc tk d ->
               match tk with
               | T_py_exception ({ekind = E_py_object (oa, oe)} as e, s, k) when compare_addr a oa = 0 ->
                  Flow.add (T_py_exception ({e with ekind = E_py_object (a', oe)}, s, k)) d man.lattice acc
               | _ -> Flow.add tk d man.lattice acc) (Flow.bottom (Flow.get_ctx flow) (Flow.get_alarms flow)) flow
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
          let aaddr = man.ask Universal.Heap.Recency.Q_allocated_addresses flow in
          let flow = List.fold_left (fun flow addr ->
                         match akind addr with
                         | A_py_method(func, ({ekind = E_py_object(ainst, oe)} as inst), mclass) when compare_addr a ainst = 0 ->
                            let addr' = {addr with addr_kind = A_py_method(func, {inst with ekind = E_py_object(a', oe)}, mclass)} in
                            let skind = match skind stmt with
                              | S_fold _ -> S_fold ({e2 with ekind = E_addr addr'}, [{e1 with ekind = E_addr addr}])
                              | S_rename _ -> S_rename ({e1 with ekind = E_addr addr}, {e2 with ekind = E_addr addr'})
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
       Soundness.warn_at range "%a not properly supported" pp_stmt stmt;
       man.exec   (mk_remove_var v range) flow |> OptionExt.return

    | S_py_delete _ ->
       Soundness.warn_at range "%a not supported, ignored" pp_stmt stmt;
       flow |> Post.return |> OptionExt.return


    | S_remove {ekind = E_addr {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin s, _)}}} when List.mem s ["int"; "float"; "bool"; "NoneType"; "NotImplementedType"; "str"] ->
       flow |> Post.return |> OptionExt.return

    | S_invalidate ({ekind = E_addr a})
    | S_remove ({ekind = E_addr a}) ->
       let cur = get_env T_cur man flow in
       let ncur = AMap.map (ASet.remove (Def a)) cur in
       let flow = set_env T_cur ncur man flow in
       begin match akind a with
       | A_py_instance _ ->
          man.exec ~route:(Below name) stmt flow
       | ak when Objects.Data_container_utils.is_data_container ak ->
          man.exec ~route:(Below name) stmt flow
       | _ -> Post.return flow
       end |> OptionExt.return

    | _ -> None

  and assign_addr man v av mode flow =
    let cur = get_env T_cur man flow in
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
        let addr = match ekind eaddr with
          | E_addr a -> a
          | _ -> assert false in
        man.exec   (mk_add eaddr range) flow >>%
        Eval.singleton (mk_py_object (addr, oe) range)
      )

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_var (v, mode) when is_py_exp exp ->
      let cur = get_env T_cur man flow in
      if AMap.mem v cur then
        let aset = AMap.find v cur in
        let evals, annot = ASet.fold (fun a (acc, annots) ->
            let flow = if v.vmode = WEAK then flow else set_env T_cur (AMap.add v (ASet.singleton a) cur) man flow in
            let flow = Flow.set_ctx annots flow in
            match a with
            | Undef_global when is_builtin_var v ->
              Eval.singleton (mk_py_object (find_builtin (get_orig_vname v)) range) flow :: acc, annots

            | Undef_local when is_builtin_var v ->
              Eval.singleton (mk_py_object (find_builtin @@ get_orig_vname v) range) flow :: acc, annots

            | Undef_global ->
              debug "Incoming NameError, on var %a, range %a, cs = %a @\n" pp_var v pp_range range pp_callstack (Flow.get_callstack flow);
              let msg = Format.asprintf "name '%a' is not defined" pp_var v in
              let flow = post_to_flow man (man.exec (Utils.mk_builtin_raise_msg "NameError" msg range) flow) in
              Eval.empty_singleton flow :: acc, Flow.get_ctx flow

            | Undef_local ->
              debug "Incoming UnboundLocalError, on var %a, range %a, cs = %a @\ncur = %a@\n" pp_var v pp_range range pp_callstack (Flow.get_callstack flow) man.lattice.print (Flow.get T_cur man.lattice flow);
              let msg = Format.asprintf "local variable '%a' referenced before assignment" pp_var v in
              let flow = post_to_flow man (man.exec (Utils.mk_builtin_raise_msg "UnboundLocalError" msg range) flow) in
              Eval.empty_singleton flow :: acc, Flow.get_ctx flow

            | Def addr ->
              (* first, let's clean numerical/string variables from other addrs *)
              let flow = if v.vmode = WEAK then flow else ASet.fold (fun a' flow ->
                  match a' with
                  | Def addr' when compare_addr_kind (akind addr) (akind addr') <> 0 -> (* only the kind. If two str addrs, we can't remove anything *)
                    debug "removing other numerical vars";
                    let f = begin match akind addr' with
                      | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                        man.exec   (mk_remove_var (Utils.change_var_type T_string v) range) flow
                      | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                      | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                         begin match akind addr with
                         | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                           | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                            Post.return flow
                         | _ ->
                            man.exec  (mk_remove_var (Utils.change_var_type T_int v) range) flow
                         end
                      | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                        man.exec   (mk_remove_var (Utils.change_var_type (T_float F_DOUBLE) v) range) flow
                      | _ ->
                        Post.return flow
                    end in
                    debug "done"; post_to_flow man f
                  | _ -> flow
                ) aset flow in
              let exp = match akind addr with
                | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)}
                | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)} ->
                  Utils.change_evar_type T_int exp
                | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                  Utils.change_evar_type (T_float F_DOUBLE) exp
                | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                  Utils.change_evar_type T_string exp
                | _ -> exp in
              let res = man.eval (mk_py_object (addr, Some exp) range) flow in
              let annots = Cases.get_ctx res in
              res :: acc, annots

          ) aset ([], Flow.get_ctx flow) in
        let evals = List.map (Cases.set_ctx annot) evals in
        evals |> Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow)
        |> OptionExt.return
      else if is_builtin_var v then
        let () = debug "is it a builtin?" in
        let obj = find_builtin @@ get_orig_vname v in
        Eval.singleton (mk_py_object obj range) flow |> OptionExt.return
      else if is_bottom cur then
        let () = debug "cur to bottom, empty singleton" in
        Eval.empty_singleton flow |> OptionExt.return
      else
        let () = debug "not a builtin..." in
        Eval.empty_singleton flow |>
        OptionExt.return

    (* todo: should be moved to zone system? useless? *)
    | E_py_object ({addr_kind = A_py_instance _}, _) ->
      Eval.singleton exp flow |> OptionExt.return

    | E_constant C_py_none ->
      Eval.singleton (mk_py_object (OptionExt.none_to_exn !addr_none, None) range) flow |> OptionExt.return

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
      bind_some (fun evals flow ->
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

  let ask : type r. ('a, r) query -> ('a, t) man -> 'a flow -> r option =
    fun query man flow ->
      match query with
      | Universal.Heap.Recency.Q_alive_addresses ->
           let cur = get_env T_cur man flow in
           let aset = AMap.fold (fun var aset acc ->
                          ASet.join aset acc) cur ASet.empty in
           List.rev @@
             ASet.fold (fun pyaddr acc -> match pyaddr with
                                          | Def a -> a :: acc
                                          | _ -> acc) aset []
         |> OptionExt.return

      | Universal.Heap.Recency.Q_alive_addresses_aspset ->
         let cur = get_env T_cur man flow in
         let aset = AMap.fold (fun _ aset acc ->
                        ASet.join aset acc) cur ASet.empty in
         ASet.fold (fun pyaddr acc -> match pyaddr with
                                      | Def a -> Universal.Heap.Recency.Pool.add a acc
                                      | _ -> acc) aset Universal.Heap.Recency.Pool.empty
         |> OptionExt.return


      | Framework.Engines.Interactive.Q_debug_variable_value var ->
         let open Framework.Engines.Interactive in
         let cur = get_env T_cur man flow in
         let aset = AMap.find var cur in
         let subvalues = ASet.fold (fun pyaddr acc ->
                             match pyaddr with
                             | Def addr ->
                                let s = Format.asprintf "%a" PyAddr.print pyaddr in
                                if List.exists (fun a' -> compare_addr  addr (OptionExt.none_to_exn a') = 0)
                                     [!addr_none; !addr_notimplemented; !addr_true; !addr_false; !addr_bool_top] then
                                  (s, {var_value = None; var_value_type = T_any; var_sub_value = None}) :: acc
                                else if compare_addr (OptionExt.none_to_exn !addr_integers) addr = 0 then
                                    let itv = man.ask (Universal.Numeric.Common.Q_int_interval (Utils.change_evar_type T_int (mk_var var (Location.mk_fresh_range ())))) flow in
                                      let int_info = {var_value = Some (Format.asprintf "%a" Universal.Numeric.Common.pp_int_interval itv); var_value_type = T_int; var_sub_value = None} in
                                      (s, int_info) :: acc
                                else if compare_addr (OptionExt.none_to_exn !addr_float) addr = 0 then
                                  let itv = man.ask (Universal.Numeric.Common.Q_float_interval (Utils.change_evar_type (T_float F_DOUBLE) (mk_var var (Location.mk_fresh_range ())))) flow in
                                  let float_info = {var_value = Some (Format.asprintf "%a" Universal.Numeric.Common.pp_float_interval itv); var_value_type = T_float F_DOUBLE; var_sub_value = None} in
                                  (s, float_info) :: acc
                                else if compare_addr (OptionExt.none_to_exn !addr_strings) addr = 0 then
                                  let str =  man.ask (Universal.Strings.Powerset.Q_strings_powerset (Utils.change_evar_type T_string (mk_var var (Location.mk_fresh_range ())))) flow in
                                  let str_info =
                                    {var_value = Some (Format.asprintf "%a" Universal.Strings.Powerset.StringPower.print str);
                                     var_value_type = T_string;
                                     var_sub_value = None} in
                                  (s, str_info) :: acc
                                else
                                  let addr_info = man.ask (Universal.Ast.Q_debug_addr_value addr) flow in
                                  (s, addr_info) :: acc
                             | _ -> acc
                           ) aset [] in
         {var_value = None;
          var_value_type = T_any;
          var_sub_value = Some (Named_sub_value subvalues)}
         |> OptionExt.return

      | _ -> None

end

let () =
  register_standard_domain (module Domain);
