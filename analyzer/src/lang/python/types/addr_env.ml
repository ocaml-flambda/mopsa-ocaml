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
open Sig.Stacked.Intermediate
open Ast
open Addr
open Universal.Ast
open Data_model.Attribute
open Alarms


(*FIXME: can iterators over addresses be renamed? *)


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


type addr_group +=
  | G_py_bool of bool option
  | G_group of addr * addr



let () =
  register_addr_group {
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


let addr_none () = {addr_group = G_all; addr_kind = A_py_instance (fst @@ find_builtin "NoneType"); addr_mode = STRONG}
let addr_notimplemented () = {addr_group = G_all; addr_kind = A_py_instance (fst @@ find_builtin "NotImplementedType"); addr_mode = STRONG}
let addr_integers () = {addr_group = G_all; addr_kind = A_py_instance (fst @@ find_builtin "int"); addr_mode = WEAK}
let addr_float () = {addr_group = G_all; addr_kind = A_py_instance (fst @@ find_builtin "float"); addr_mode = WEAK}
let addr_true () = {addr_group = G_py_bool (Some true); addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = STRONG}
let addr_false () = {addr_group = G_py_bool (Some false); addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = STRONG}
let addr_bool_top () = {addr_group = G_py_bool None; addr_kind = A_py_instance (fst @@ find_builtin "bool"); addr_mode = WEAK}


module Domain =
struct

  module ASet =
    (struct
      module PS = Framework.Lattices.Powerset.Make(PyAddr)
      include PS
      let undef a = match a with
        | PyAddr.Def _ -> false
        | _ -> true

      let widen ctx a b = join a b
        (* Top.top_absorb2 (fun a b ->
         *     if Set.cardinal b - Set.cardinal a = 1
         *     && Set.exists undef b && Set.exists undef a then
         *       Top.Nt (Set.union a b)
         *     else
         *       PS.widen annot at bt) at bt *)
    end)

  module AMap = Framework.Lattices.Partial_map.Make
      (struct type t = var let compare = compare_var let print = pp_var end)
      (ASet)

  include AMap
  let subset _ _ (l1, r1) (l2, r2)  = AMap.subset l1 l2, r1, r2
  let join _ _ (l1, r1) (l2, r2) = AMap.join l1 l2, r1, r2
  let meet _ _ (l1, r1) (l2, r2) = AMap.meet l1 l2, r1, r2
  let widen _ uctx (l1, r1) (l2, r2) = AMap.widen uctx l1 l2, r1, r2, true

  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.types.addr_env"
    end)

  let interface = {
    iexec = { provides = [Zone.Z_py]; uses = [Zone.Z_py; Zone.Z_py_obj; Universal.Zone.Z_u_int; Universal.Zone.Z_u_float; Universal.Zone.Z_u_string]; };
    ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]; }
  }

  let alarms = []

  let merge _ _ _ = assert false

  let print fmt m =
    Format.fprintf fmt "addrs: @[%a@]@\n" AMap.print m

  let init prog man flow =
    set_env T_cur empty man flow

  let fold_intfloatstr man v flow fstmt =
    let cur = get_env T_cur man flow in
    let oaset = AMap.find_opt v cur in
    match oaset with
    | None ->
       flow
    | Some aset ->
       (* FIXME: if we explicitly define things below, we could use ASet.mem *)
       let str, intb, float = ASet.fold (fun pyaddr (str, intb, float) ->
                                  match pyaddr with
                                  | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "str", _)}} ->
                                     (true, intb, float)
                                  | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)}}
                                    | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}} ->
                                     (str, true, float)
                                  | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "float", _)}} ->
                                     (str, intb, true)
                                  | _ -> (str, intb, float)
                                ) aset (false, false, false) in
       let flow = if str then man.exec ~zone:Universal.Zone.Z_u_string (fstmt T_string) flow  else flow in
       let flow = if intb then man.exec ~zone:Universal.Zone.Z_u_int (fstmt T_int) flow else flow in
       let flow = if float then man.exec ~zone:Universal.Zone.Z_u_float (fstmt (T_float F_DOUBLE)) flow else flow in
       flow

  let rec exec zone stmt man flow =
    debug "exec %a@\n" pp_stmt stmt;
    let range = srange stmt in
    match skind stmt with
    | S_add ({ekind = E_var (v, mode)}) ->
       let cur = get_env T_cur man flow in
       if mem v cur then
         flow |> Post.return |> OptionExt.return
       else
         let ncur = add v (ASet.singleton Undef_global) cur in
         let () = debug "cur was %a@\nncur is %a@\n" print cur print ncur in
         let flow =  set_env T_cur ncur man flow in
         let () = debug "flow is now %a@\n" (Flow.print man.lattice.print) flow in
         flow |> Post.return |> OptionExt.return

    | S_assign(({ekind = E_var (v, vmode)} as vl), ({ekind = E_var (w, wmode)} as wl)) when var_mode v vmode = WEAK && var_mode w wmode = WEAK ->
       let cur = get_env T_cur man flow in
       begin match AMap.find_opt w cur with
       | None -> flow |> Post.return |> OptionExt.return
       | Some aset ->
          (* FIXME FIXME FIXME FIXME: what happens if multiple float/... instances? *)
          let flow = ASet.fold
                       (fun pyaddr flow ->
                         let cstmt = fun t -> {stmt with skind = S_assign(Utils.change_evar_type t vl, Utils.change_evar_type t wl)} in
                         match pyaddr with
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "str", _)}} ->
                            man.exec ~zone:Universal.Zone.Z_u_string (cstmt T_string) flow
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)}}
                           | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}} ->
                            man.exec ~zone:Universal.Zone.Z_u_int (cstmt T_int) flow
                         | Def {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "float", _)}}  ->
                            man.exec ~zone:Universal.Zone.Z_u_float (cstmt (T_float F_DOUBLE)) flow
                         | _ -> flow
                       ) aset flow in
          if mem v cur then
            set_env T_cur (add v (ASet.join (find w cur) (find v cur)) cur) man flow |> Post.return |> OptionExt.return
          else
            set_env T_cur (add v (find w cur) cur) man flow |> Post.return |> OptionExt.return
       end

    (* S⟦ v = e ⟧ *)
    | S_assign(({ekind = E_var (v, mode)} as evar), e) ->
       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
         bind_some
           (fun e flow ->
             Debug.debug ~channel:"debugrecency" "assign_addr %a, e = %a" pp_var v pp_expr e;
             match ekind e with
             | E_py_undefined true ->
                assign_addr man v PyAddr.Undef_global mode flow |> Post.return

             | E_py_undefined false ->
                assign_addr man v PyAddr.Undef_local mode flow |> Post.return

             | E_py_object (addr, None) ->
                Debug.debug ~channel:"debugrecency" "assign_addr %a %a" pp_var v pp_addr addr;
                assign_addr man v (PyAddr.Def addr) mode flow |> Post.return

             | E_py_object (addr, Some expr) ->
                let flow = assign_addr man v (PyAddr.Def addr) mode flow in
                debug "calling values for %a" pp_addr addr;
                begin match akind addr with
                | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                   man.exec ~zone:Universal.Zone.Z_u_string (mk_assign (Utils.change_evar_type T_string evar) expr range) flow
                | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                  | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                   man.exec ~zone:Universal.Zone.Z_u_int (mk_assign (Utils.change_evar_type T_int evar) expr range) flow
                | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                   man.exec ~zone:Universal.Zone.Z_u_float (mk_assign (Utils.change_evar_type (T_float F_DOUBLE) evar) expr range) flow
                | _ ->
                   flow
                end |>
                  Post.return

             | E_constant (C_top T_any) ->
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
       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
         bind_some (fun e flow -> match ekind e with
                                  | E_py_object (addr, _) ->
                                     begin match akind addr with
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                                        man.exec ~zone:Universal.Zone.Z_u_string (mk_assign (Utils.change_evar_type T_string evar) (mk_py_top T_string range) range) flow
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                                       | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                                        man.exec ~zone:Universal.Zone.Z_u_int (mk_assign (Utils.change_evar_type T_int evar) (mk_py_top T_int range) range) flow
                                     | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                                        man.exec ~zone:Universal.Zone.Z_u_float (mk_assign (Utils.change_evar_type (T_float F_DOUBLE) evar) (mk_py_top (T_float F_DOUBLE) range) range) flow
                                     | _ ->
                                        flow
                                     end |>
                                       assign_addr man v (PyAddr.Def addr) mode
                                     |> Post.return
                                  | _ -> Exceptions.panic_at range "%a@\n" pp_expr e)
       |> OptionExt.return



    | S_assign({ekind = E_py_attribute(lval, attr)}, rval) ->
       (* TODO: setattr *)
       bind_list [lval; rval] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
         bind_some (fun args flow ->
             let elval, erval = match args with [e1;e2] -> e1, e2 | _ -> assert false in
             man.exec ~zone:Zone.Z_py_obj (mk_assign (mk_py_attr elval attr range) erval range) flow |> Post.return
           )
       |> OptionExt.return


    | S_remove ({ekind = E_var (v, _)} as var) ->
       let flow = fold_intfloatstr man v flow (fun t -> mk_remove_var (Utils.change_var_type t v) range) in
       let cur = get_env T_cur man flow in
       let flow = set_env T_cur (AMap.remove v cur) man flow in
       begin match v.vkind with
       | V_uniq _ when not (Hashtbl.mem type_aliases v) ->
          (* if the variable maps to a list, we should remove the temporary variable associated, ONLY if it's not used by another list *)
          let flow = man.exec (mk_assign var (mk_expr (E_py_undefined true) range) range) flow in
          flow |> Post.return |> OptionExt.return

       | _ ->
          flow |> Post.return |> OptionExt.return
       end

    | S_assume e ->
       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
         bind_some (fun expr flow ->
             match ekind expr with
             | E_constant (C_top T_bool)
               | E_constant (C_bool true)
               -> Post.return flow
             | E_py_object (a, _) when compare_addr a (addr_true ()) = 0 || compare_addr a (addr_bool_top ()) = 0
               -> Post.return flow
             | E_py_object (a, _) when compare_addr a (addr_false ()) = 0
               -> Post.return (set_env T_cur bottom man flow)
             | E_constant (C_bool false) ->
                Post.return (set_env T_cur bottom man flow)
             | _ ->
                Exceptions.panic_at range "todo addr_env/assume on %a@\n" pp_expr e
           )
       |> OptionExt.return

    | S_rename (({ekind = E_var (v, mode)} as l), ({ekind = E_var (v', mode')} as r)) ->
       (* FIXME: modes, rename in weak shouldn't erase the old v'? *)
       let flow = fold_intfloatstr man v flow (fun t ->
                      {stmt with skind = S_rename (Utils.change_evar_type t l,
                                                   Utils.change_evar_type t r)}) in
       let cur = get_env T_cur man flow in
       begin match AMap.find_opt v cur with
       | Some aset ->
          set_env T_cur (AMap.rename v v' cur) man flow |>
            Post.return |> OptionExt.return
       | None ->
          begin match v.vkind with
          | V_addr_attr(a, _) when Objects.Data_container_utils.is_data_container a.addr_kind ->
             (* because the container abstraction may not bind v to anything if the container is empty *)
             flow |> Post.return |> OptionExt.return
          | _ -> assert false (* shouldn't happen *) end
       end

    | S_rename (({ekind = E_addr a} as e1), ({ekind = E_addr a'} as e2)) ->
       let cur = get_env T_cur man flow in
       let ncur = AMap.map (ASet.map (fun addr -> if addr = Def a then Def a' else addr)) cur in
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
          man.exec ~zone:Zone.Z_py_obj stmt flow |>
            man.exec ~zone:Zone.Z_py_obj {stmt with skind = S_rename ({e1 with ekind = E_py_annot e1}, e2)}
       | A_py_instance _ ->
          man.exec ~zone:Zone.Z_py_obj stmt flow
       | ak when Objects.Data_container_utils.is_data_container ak ->
          man.exec ~zone:Zone.Z_py_obj stmt flow
       | _ -> flow
       end
       |> Post.return |> OptionExt.return

    | S_remove {ekind = E_addr {addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin s, _)}}} when List.mem s ["int"; "float"; "bool"; "NoneType"; "NotImplementedType"; "str"] ->
       flow |> Post.return |> OptionExt.return

    | S_remove {ekind = E_addr a} ->
       let cur = get_env T_cur man flow in
       let ncur = AMap.map (ASet.remove (Def a)) cur in (* FIXME: if Aset = empty, remove it? *)
       let flow = set_env T_cur ncur man flow in
       begin match akind a with
       | A_py_instance _ ->
          man.exec ~zone:Zone.Z_py_obj stmt flow
       | ak when Objects.Data_container_utils.is_data_container ak ->
          man.exec ~zone:Zone.Z_py_obj stmt flow
       | _ -> flow
       end |> Post.return |> OptionExt.return

    | S_py_delete {ekind = E_var (v, _)} ->
       Soundness.warn_at range "%a not properly supported" pp_stmt stmt;
       man.exec ~zone:Zone.Z_py (mk_remove_var v range) flow |> Post.return |> OptionExt.return

    | S_py_delete _ ->
       Soundness.warn_at range "%a not supported, ignored" pp_stmt stmt;
       flow |> Post.return |> OptionExt.return

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
    man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr ~mode:mode (A_py_instance cls) range) flow |>
    Eval.bind (fun eaddr flow ->
        let addr = match ekind eaddr with
          | E_addr a -> a
          | _ -> assert false in
        man.exec ~zone:Zone.Z_py_obj (mk_add eaddr range) flow |>
        Eval.singleton (mk_py_object (addr, oe) range)
      )




  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_var (v, mode) ->
      let cur = get_env T_cur man flow in
      if AMap.mem v cur then
        let aset = AMap.find v cur in
        let evals, annot = ASet.fold (fun a (acc, annots) ->
            let flow = set_env T_cur (AMap.add v (ASet.singleton a) cur) man flow in
            let flow = Flow.set_ctx annots flow in
            match a with
            | Undef_global when is_builtin_var v ->
              (Eval.singleton (mk_py_object (find_builtin (get_orig_vname v)) range) flow :: acc, annots)

            | Undef_local when is_builtin_var v ->
              (Eval.singleton (mk_py_object (find_builtin @@ get_orig_vname v) range) flow :: acc, annots)

            | Undef_global ->
              debug "Incoming NameError, on var %a, range %a, cs = %a @\n" pp_var v pp_range range Callstack.print (Flow.get_callstack flow);
              let msg = Format.asprintf "name '%a' is not defined" pp_var v in
              let flow = man.exec (Utils.mk_builtin_raise_msg "NameError" msg range) flow in
              (Eval.empty_singleton flow :: acc, Flow.get_ctx flow)

            | Undef_local ->
              debug "Incoming UnboundLocalError, on var %a, range %a, cs = %a @\ncur = %a@\n" pp_var v pp_range range Callstack.print (Flow.get_callstack flow) man.lattice.print (Flow.get T_cur man.lattice flow);
              let msg = Format.asprintf "local variable '%a' referenced before assignment" pp_var v in
              let flow = man.exec (Utils.mk_builtin_raise_msg "UnboundLocalError" msg range) flow in
              (Eval.empty_singleton flow :: acc, Flow.get_ctx flow)

            | Def addr ->
              (* first, let's clean numerical/string variables from other addrs *)
              let flow = ASet.fold (fun a' flow ->
                  match a' with
                  | Def addr' when compare_addr_kind (akind addr) (akind addr') <> 0 -> (* only the kind. If two str addrs, we can't remove anything *)
                    debug "removing other numerical vars";
                    let f = begin match akind addr' with
                      | A_py_instance {addr_kind = A_py_class (C_builtin "str", _)} ->
                        man.exec ~zone:Universal.Zone.Z_u_string (mk_remove_var (Utils.change_var_type T_string v) range) flow
                      | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                      | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                         begin match akind addr with
                         | A_py_instance {addr_kind = A_py_class (C_builtin "int", _)}
                           | A_py_instance {addr_kind = A_py_class (C_builtin "bool", _)} ->
                            flow
                         | _ ->
                            man.exec ~zone:Universal.Zone.Z_u_int (mk_remove_var (Utils.change_var_type T_int v) range) flow
                         end
                      | A_py_instance {addr_kind = A_py_class (C_builtin "float", _)} ->
                        man.exec ~zone:Universal.Zone.Z_u_float (mk_remove_var (Utils.change_var_type (T_float F_DOUBLE) v) range) flow
                      | _ ->
                        flow
                    end in
                    debug "done"; f
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
              let annots = Eval.get_ctx res in
              res :: acc, annots

          ) aset ([], Flow.get_ctx flow) in
        let evals = List.map (Eval.set_ctx annot) evals in
        evals |> Eval.join_list ~empty:(fun () -> Eval.empty_singleton flow)
        |> OptionExt.return
      else if is_builtin_var v then
        (* let () = debug "bla %s %s %d" v.org_vname v.uniq_vname v.vuid in *)
        (* man.eval (mk_py_object (find_builtin v.org_vname) range) flow |> OptionExt.return *)
        let () = debug "is it a builtin?" in
        let obj = find_builtin @@ get_orig_vname v in
        Eval.singleton (mk_py_object obj range) flow |> OptionExt.return
      else if is_bottom cur then
        let () = debug "cur to bottom, empty singleton" in
        Eval.empty_singleton flow |> OptionExt.return
      else
        (* let () = warn_at range "NameError on %a that shouldn't happen. Todo: use partial envs and add_var %a" pp_var v (Flow.print man.lattice.print) flow in
         * let () = Format.fprintf Format.str_formatter "name '%s' is not defined" (get_orig_vname v) in
         * man.exec (Utils.mk_builtin_raise_msg "NameError" (Format.flush_str_formatter ()) range) flow |> *)
        (* panic_at range "%a not a builtin, cur = %a" pp_var v print cur *)
        let () = debug "not a builtin..." in
        Eval.empty_singleton flow |>
        OptionExt.return

    (* todo: should be moved to zone system? useless? *)
    | E_py_object ({addr_kind = A_py_instance _}, _) ->
      Eval.singleton exp flow |> OptionExt.return

    (* FIXME: clean *)
    | E_constant C_py_none ->
      Eval.singleton (mk_py_object (addr_none (), None) range) flow |> OptionExt.return

    | E_constant C_py_not_implemented ->
      Eval.singleton (mk_py_object (addr_notimplemented (), None) range) flow |> OptionExt.return

    | E_unop(O_log_not, e') ->
      (* bool is called in desugar/bool *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e' (*(Utils.mk_builtin_call "bool" [e'] range)*) flow |>
      Eval.bind
        (fun ee' flow ->
           match ekind ee' with
           (* FIXME: weird cases *)
           | E_constant (C_top T_bool) ->
             Eval.singleton ee' flow
           | E_constant (C_bool true) ->
             Eval.singleton (mk_py_false range) flow
           | E_constant (C_bool false) ->
             Eval.singleton (mk_py_true range) flow
           | E_py_object (a, _) when compare_addr a (addr_true ()) = 0 ->
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_false range) flow
           | E_py_object (a, _) when compare_addr a (addr_false ()) = 0 ->
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_true range) flow
           | E_py_object (a, _) when compare_addr a (addr_bool_top ()) = 0 ->
             Eval.singleton ee' flow
           | _ ->
             panic_at range "o_log_not ni on %a" pp_expr ee'
        )
      |> OptionExt.return

    | E_binop(O_py_is, e1, e2) ->
      bind_list [e1;e2] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let e1, e2 = match evals with [e1;e2] -> e1, e2 | _ -> assert false in
          begin match ekind e1, ekind e2 with
            | E_py_object (a1, _), E_py_object (a2, _) when
                compare_addr a1 a2 = 0 &&
                (compare_addr a1 (addr_notimplemented ()) = 0 || compare_addr a1 (addr_none ()) = 0 ||
                 compare_addr a2 (addr_notimplemented ()) = 0 || compare_addr a2 (addr_none ()) = 0) ->
              man.eval (mk_py_true range) flow
            | E_py_object (a1, _), E_py_object (a2, _) when compare_addr_kind (akind a1) (akind a2) <> 0 ->
              man.eval (mk_py_false range) flow
            | _ -> man.eval (mk_py_top T_bool range) flow
          end
        )
      |> OptionExt.return

    | _ -> None

  let ask : type r. r query -> ('a, t, 's) man -> 'a flow -> r option =
    fun query man flow ->
      match query with
      | Framework.Engines.Interactive.Q_print_var ->
        (* FIXME: values printing *)
        OptionExt.return @@
        fun fmt var_as_string ->
        let cur = get_env T_cur man flow in
        let cur_v = AMap.filter (fun var _ -> get_orig_vname ~warn:false var = var_as_string) cur in
        Format.fprintf fmt "%a@\n" AMap.print cur_v;
        AMap.fold (fun var aset () ->
            ASet.fold (fun addr () ->
                match addr with
                | Def a ->
                  (man.ask (Q_print_addr_related_info a) flow) fmt
                | _ -> ()
              ) aset ()
          ) cur_v ()

      | Universal.Heap.Recency.Q_alive_addresses ->
           let cur = get_env T_cur man flow in
           let aset = AMap.fold (fun var aset acc ->
                          ASet.join aset acc) cur ASet.empty in
           List.rev @@
             ASet.fold (fun pyaddr acc -> match pyaddr with
                                          | Def a -> a :: acc
                                          | _ -> acc) aset []
         |> OptionExt.return

      (* | Universal.Heap.Recency.Q_alive_addresses_aspset ->
       *    let cur = get_env T_cur man flow in
       *    let aset = AMap.fold (fun _ aset acc ->
       *                   ASet.join aset acc) cur ASet.empty in
       *    ASet.fold (fun pyaddr acc -> match pyaddr with
       *                                 | Def a -> Universal.Heap.Recency.Pool.add a acc
       *                                 | _ -> acc) aset Universal.Heap.Recency.Pool.empty
       *    |> OptionExt.return *)

      | Universal.Heap.Recency.Q_alive_addresses_aspset ->
         let open Universal.Heap.Recency in
         let cur = get_env T_cur man flow in
         let usualmap, addrattrsmap = AMap.partition (fun v _ -> match vkind v with
                                                                 | V_addr_attr _ -> false
                                                                 | _ -> true) cur in
         let vars_to_check = List.map fst (AMap.bindings usualmap) in
         let pool_of_aset aset start = ASet.fold (fun pyaddr acc -> match pyaddr with
                                                                    | Def a -> Pool.add a acc
                                                                    | _ -> acc) aset start in
         let rec find_alive_addrs_usualvars alive_addrs to_check =
           match to_check with
           | [] -> alive_addrs
           | hd :: tl ->
              let aset = AMap.find hd usualmap in
              find_alive_addrs_usualvars (pool_of_aset aset alive_addrs) tl in
         let find_alive_addrs_attrs alive_addrs =
           AMap.fold (fun var aset new_alive ->
               match vkind var with
               | V_addr_attr (a, _) ->
                  if Pool.mem a alive_addrs then
                    pool_of_aset aset new_alive
                  else new_alive
               | _ -> assert false
             ) addrattrsmap alive_addrs in
         let alive_addrs = find_alive_addrs_usualvars Pool.empty vars_to_check in
         let rec lfp f init =
           let r = f init in
           if Pool.equal r init then init
           else lfp f r in
         lfp find_alive_addrs_attrs alive_addrs
         |> OptionExt.return

      | _ -> None

  let refine channel man flow = Channel.return flow

end

let () =
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Domain);
