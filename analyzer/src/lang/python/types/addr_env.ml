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
open Sig.Domain.Intermediate
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
    | Undef_local -> Format.fprintf fmt "unbound"
    | Undef_global -> Format.fprintf fmt "undef"
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
        | G_py_bool b1, G_py_bool b2 -> Option.compare Pervasives.compare b1 b2
        | G_group (a1, b1), G_group (a2, b2) ->
          Compare.compose
            [(fun () -> compare_addr a1 a2);
             (fun () -> compare_addr b1 b2);
            ]
        | _ -> next g1 g2
      );
  }


(* FIXME: I'm pretty sure this is going to fail as builtins won't be registered at this point *)
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

      let widen a b = join a b
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

  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.types.addr_env"
    end)

  let interface = {
    iexec = { provides = [Zone.Z_py]; uses = [Zone.Z_py; Zone.Z_py_obj]; };
    ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]; }
  }

  let merge _ _ _ = assert false

  let widen ctx = widen

  let print fmt m =
    Format.fprintf fmt "addrs: @[%a@]@\n" AMap.print m

  let init prog man flow =
    set_env T_cur empty man flow

  let rec exec zone stmt man flow =
    debug "exec %a@\n" pp_stmt stmt;
    let range = srange stmt in
    match skind stmt with
    | S_add ({ekind = E_var (v, mode)}) ->
      let cur = get_env T_cur man flow in
      if mem v cur then
        flow |> Post.return |> Option.return
      else
        let ncur = add v (ASet.singleton Undef_global) cur in
        let () = debug "cur was %a@\nncur is %a@\n" print cur print ncur in
        let flow =  set_env T_cur ncur man flow in
        let () = debug "flow is now %a@\n" (Flow.print man.lattice.print) flow in
        flow |> Post.return |> Option.return

    | S_assign({ekind = E_var (v, WEAK)}, {ekind = E_var (w, WEAK)}) ->
      let cur = get_env T_cur man flow in
      if mem w cur then
        if mem v cur then
          set_env T_cur (add v (ASet.join (find w cur) (find v cur)) cur) man flow |> Post.return |> Option.return
        else
          set_env T_cur (add v (find w cur) cur) man flow |> Post.return |> Option.return
      else
        flow |> Post.return |> Option.return

    (* S⟦ v = e ⟧ *)
    | S_assign(({ekind = E_var (v, mode)} as evar), e) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
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
                begin match akind addr with
                | A_py_instance s when compare_addr s (fst @@ find_builtin "str") = 0 ->
                  man.exec ~zone:Zone.Z_py_obj (mk_assign evar e range) flow
                | _ ->
                  flow
                end |>
                Post.return

              | _ -> Exceptions.panic_at range "%a@\n" pp_expr e
          )
      |> Option.return

    | S_py_annot ({ekind = E_var (v, mode)}, e) ->
      (* need to make e E_py_annot here or on the frontend *)
      (* then handle E_py_annot in typing, to perform an allocation? *)
      (* what about non builtins? *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
      bind_some (fun e flow -> match ekind e with
          | E_py_object (addr, _) ->
            assign_addr man v (PyAddr.Def addr) mode flow
            |> Post.return
          | _ -> Exceptions.panic_at range "%a@\n" pp_expr e)
      |> Option.return



    | S_assign({ekind = E_py_attribute(lval, attr)}, rval) ->
      (* TODO: setattr *)
      bind_list [lval; rval] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun args flow ->
          let elval, erval = match args with [e1;e2] -> e1, e2 | _ -> assert false in
          man.exec ~zone:Zone.Z_py_obj (mk_assign (mk_py_attr elval attr range) erval range) flow |> Post.return
        )
      |> Option.return


    | S_remove ({ekind = E_var (v, _)} as var) ->
      let flow = map_env T_cur (remove v) man flow in
      let flow = man.exec ~zone:Zone.Z_py_obj (mk_remove_var v range) flow in
      begin match v.vkind with
        | V_uniq _ when not (Hashtbl.mem type_aliases v) ->
          (* if the variable maps to a list, we should remove the temporary variable associated, ONLY if it's not used by another list *)
          let flow = man.exec (mk_assign var (mk_expr (E_py_undefined true) range) range) flow in
          flow |> Post.return |> Option.return

        | _ ->
          Post.return flow |> Option.return
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
      |> Option.return

    | S_rename ({ekind = E_var (v, mode)}, {ekind = E_var (v', mode')}) ->
      (* FIXME: modes, rename in weak shouldn't erase the old v'? *)
      let cur = get_env T_cur man flow in
      if AMap.mem v cur then
        set_env T_cur (AMap.rename v v' cur) man flow |>
        Post.return |> Option.return
      else
        begin match v.vkind with
        | V_addr_attr(a, _) when Objects.Data_container_utils.is_data_container a.addr_kind ->
          flow |> Post.return |> Option.return
        | _ -> assert false (* shouldn't happen *) end

    | S_rename ({ekind = E_addr a}, {ekind = E_addr a'}) ->
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
        | A_py_instance _ -> man.exec ~zone:Zone.Z_py_obj stmt flow |> Post.return |> Option.return
        | ak when Objects.Data_container_utils.is_data_container ak ->
          man.exec ~zone:Zone.Z_py_obj stmt flow |> Post.return |> Option.return
        | _ -> flow |> Post.return |> Option.return
      end

    | _ -> None

  and assign_addr man v av mode flow =
    let cur = get_env T_cur man flow in
    let aset = match mode with
      | STRONG -> ASet.singleton av
      | WEAK ->
        ASet.add av (Option.default ASet.empty (find_opt v cur))
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
            | Undef_global when is_builtin_name (get_orig_vname v) ->
              (Eval.singleton (mk_py_object (find_builtin (get_orig_vname v)) range) flow :: acc, annots)

            | Undef_local when is_builtin_name (get_orig_vname v) ->
              (Eval.singleton (mk_py_object (find_builtin @@ get_orig_vname v) range) flow :: acc, annots)

            | Undef_global ->
              debug "Incoming NameError, on var %a, range %a, cs = %a @\n" pp_var v pp_range range Callstack.print (Flow.get_callstack flow);
              Format.fprintf Format.str_formatter "name '%a' is not defined" pp_var v;
              let flow = man.exec (Utils.mk_builtin_raise_msg "NameError" (Format.flush_str_formatter ()) range) flow in
              (Eval.empty_singleton flow :: acc, Flow.get_ctx flow)

            | Undef_local ->
              debug "Incoming UnboundLocalError, on var %a, range %a, cs = %a @\n" pp_var v pp_range range Callstack.print (Flow.get_callstack flow);
              Format.fprintf Format.str_formatter "local variable '%a' referenced before assignment" pp_var v;
              let flow = man.exec (Utils.mk_builtin_raise_msg "UnboundLocalError" (Format.flush_str_formatter ()) range) flow in
              (Eval.empty_singleton flow :: acc, Flow.get_ctx flow)

            | Def addr ->
              let res = man.eval (mk_py_object (addr, Some exp) range) flow in
              let annots = Eval.get_ctx res in
              res :: acc, annots

          ) aset ([], Flow.get_ctx flow) in
        let evals = List.map (Eval.set_ctx annot) evals in
        evals |> Eval.join_list ~empty:(Eval.empty_singleton flow)
        |> Option.return
      else if is_builtin_name @@ get_orig_vname v then
        (* let () = debug "bla %s %s %d" v.org_vname v.uniq_vname v.vuid in *)
        (* man.eval (mk_py_object (find_builtin v.org_vname) range) flow |> Option.return *)
        let obj = find_builtin @@ get_orig_vname v in
        Eval.singleton (mk_py_object obj range) flow |> Option.return
      else
        (* let () = warn_at range "NameError on %a that shouldn't happen. Todo: use partial envs and add_var %a" pp_var v (Flow.print man.lattice.print) flow in
         * let () = Format.fprintf Format.str_formatter "name '%s' is not defined" (get_orig_vname v) in
         * man.exec (Utils.mk_builtin_raise_msg "NameError" (Format.flush_str_formatter ()) range) flow |> *)
        Eval.empty_singleton flow |>
        Option.return

    (* todo: should be moved to zone system? useless? *)
    | E_py_object ({addr_kind = A_py_instance _}, _) ->
      Eval.singleton exp flow |> Option.return

    (* FIXME: clean *)
    | E_constant C_py_none ->
      Eval.singleton (mk_py_object (addr_none (), None) range) flow |> Option.return

    | E_constant C_py_not_implemented ->
      Eval.singleton (mk_py_object (addr_notimplemented (), None) range) flow |> Option.return

    | E_unop(O_log_not, e') ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e' (*(Utils.mk_builtin_call "bool" [e'] range)*) flow |>
      Eval.bind
        (fun exp flow ->
           match ekind exp with
           | E_constant (C_top T_bool) ->
             Eval.singleton exp flow
           | E_constant (C_bool true) ->
             Eval.singleton (mk_py_false range) flow
           | E_constant (C_bool false) ->
             Eval.singleton (mk_py_true range) flow
           | E_py_object (a, _) when compare_addr a (addr_true ()) = 0 ->
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_false range) flow
           | E_py_object (a, _) when compare_addr a (addr_false ()) = 0 ->
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_true range) flow
           | E_py_object (a, _) when compare_addr a (addr_bool_top ()) = 0 ->
             Eval.singleton exp flow
           | _ ->
             Exceptions.panic "not ni on %a@\n" pp_expr exp
        )
      |> Option.return

    | E_binop(O_py_is, e1, e2) ->
      bind_list [e1;e2] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
      bind_some (fun evals flow ->
          let e1, e2 = match evals with [e1;e2] -> e1, e2 | _ -> assert false in
          begin match ekind e1, ekind e2 with
            | E_py_object (a1, _), E_py_object (a2, _) when
                compare_addr a1 a2 = 0 &&
                (compare_addr a1 (addr_notimplemented ()) = 0 || compare_addr a1 (addr_none ()) = 0) ->
              man.eval (mk_py_true range) flow
            | _ -> man.eval (mk_py_top T_bool range) flow
          end
        )
      |> Option.return



    | _ -> None

  let ask _ _ _ = None

  let refine channel man flow = Channel.return flow


end

let () =
  Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain);
