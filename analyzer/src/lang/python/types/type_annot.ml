(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
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

(** Definition of python functions annotations as well as polymorphism *)

open Mopsa
open Sig.Domain.Intermediate
open Ast
open Addr
open Universal.Ast


module Domain =
struct

  module ESet = Framework.Lattices.Powerset.Make(struct type t = expr let compare = compare_expr let print = pp_expr end)

  module Keys = (struct
    type t = Global of string | Class of var
    let compare t1 t2 =
      match t1, t2 with
      | Global s1, Global s2 -> Pervasives.compare s1 s2
      | Class c1, Class c2 -> compare_var c1 c2
      | _ -> Pervasives.compare t1 t2
    let print fmt t  = match t with
      | Global s -> Format.pp_print_string fmt s
      | Class v -> pp_var fmt v
  end)


  module TVMap = Framework.Lattices.Partial_map.Make(Keys)(ESet)

  include TVMap
  let widen ctx = widen
  let print fmt m =
    Format.fprintf fmt "TypeVar annotations: @[%a@]@\n" TVMap.print m

  include GenDomainId(struct
      type nonrec t = t
      let name = "python.types.type_annot"
    end)

  let interface = {
    iexec = {provides = [Zone.Z_py_obj]; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
  }

  let init prog man flow =
    set_env T_cur empty man flow

  let collect_typevars ?(base=TVMap.empty) self signature =
    List.fold_left (fun acc oty ->
        match oty with
        | None -> acc
        | Some ty ->
          Visitor.fold_expr
            (fun acc expr -> match ekind expr with
               | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::types, []) ->
                 let set = if types = [] then ESet.top else ESet.of_list types in
                 debug "in %a, set = %a" pp_expr expr ESet.print set;
                 let var = match self with
                   | None ->
                     Keys.Global s
                   | Some {ekind = E_py_object (addr, _)} ->
                     Keys.Class (mk_addr_attr addr s T_any)
                   | _ -> assert false in
                 begin match TVMap.find_opt var acc with
                   | None ->
                     (begin match var with
                        | Keys.Class _ -> Keep acc
                        | Keys.Global _ -> Keep (TVMap.add var set acc)
                      end)
                   | Some set2 ->
                     if ESet.equal set set2 then Keep acc
                     else Exceptions.panic_at (erange expr) "conflict for typevar %s, sets %a and %a differ" s ESet.print set ESet.print set2
                 end
               | _ ->
                 VisitParts acc)
            (fun acc stmt -> VisitParts acc)
            acc ty) base signature.py_funcs_types_in

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function(F_annot pyannot)}, _)}, args, kwargs) when get_orig_vname pyannot.py_funca_var = "Generic.__new__"->
      let cls = List.hd args in
      man.eval cls flow |>
      Eval.bind (fun ecls flow ->
          match ekind ecls with
          | E_py_object ({addr_kind = A_py_class (C_annot c, mro)}, _) ->
            let process_tyvar e = match ekind e with
              | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::types, []) ->
                (s, types)
              | _ -> assert false in
            let typevars = match ekind @@ List.hd c.py_cls_a_abases with
              | E_py_index_subscript (_, vars) ->
                begin match ekind vars with
                  | E_py_call _ -> (process_tyvar vars)::[]
                  | E_py_tuple vs -> List.rev @@ List.fold_left (fun acc var ->  (process_tyvar var)::acc) [] vs
                  | _ -> assert false
                end
              | _ -> assert false in
            let nextinmro =
              let rec search aftergeneric mro = match mro with
                | [] -> assert false
                | (hda, hdoe) :: tl ->
                  if aftergeneric then (hda, hdoe)
                  else
                    match akind hda with
                    | A_py_class (C_annot c, _) when get_orig_vname c.py_cls_a_var = "Generic" ->
                      search true tl
                    | _ ->
                      search aftergeneric tl
              in search false mro
            in
            man.eval (mk_py_call (mk_py_attr (mk_py_object nextinmro range)  "__new__" range) args range) flow |>
            Eval.bind (fun eobj flow ->
                let addr_eobj = match ekind eobj with
                  | E_py_object (a, _) -> a
                  | _ -> assert false in
                let cur = get_env T_cur man flow in
                let ncur = List.fold_left (fun cur (tyname, types) ->
                    let etypes = ESet.of_list types in
                    let tyvar = mk_addr_attr addr_eobj tyname T_any in
                    match TVMap.find_opt (Class tyvar) cur with
                    | None -> TVMap.add (Class tyvar) etypes cur
                    | Some set ->
                      if ESet.equal set etypes then
                        cur
                      else
                        Exceptions.panic_at range "conflict for typevar %a, sets %a and %a differ" pp_var tyvar ESet.print set ESet.print etypes
                  ) cur typevars in
                let flow = set_env T_cur ncur man flow in
                Eval.singleton eobj flow
              )
          | _ -> assert false
        )
      |> Option.return
      (* bind_list args (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
       * bind_some (fun args flow ->
       *     match args with
       *     | [] ->
       *       debug "Error during creation of a new instance@\n";
       *       man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton
       *     | cls :: tl ->
       *       let c = fst @@ object_of_expr cls in
       *       man.eval  ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr (A_py_instance c) range) flow |>
       *       Eval.bind (fun eaddr flow ->
       *           let addr = match ekind eaddr with
       *             | E_addr a -> a
       *             | _ -> assert false in
       *           man.exec ~zone:Zone.Z_py_obj (mk_add eaddr range) flow |>
       *           Eval.singleton (mk_py_object (addr, None) range)
       *         )
       *   )
       * |> Option.return *)


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function(F_annot pyannot)}, _)}, args, kwargs) ->
      bind_list args man.eval flow |>
      bind_some (fun args flow ->
          let sigs = List.filter (fun sign ->
              let ndefaults = List.fold_left (fun count el -> if el then count + 1 else count) 0 sign.py_funcs_defaults in
              debug "filter %a -> [%d; %d]; |args| = %d, |kwargs| = %d" pp_py_func_sig sign (List.length sign.py_funcs_types_in - ndefaults) (List.length sign.py_funcs_types_in) (List.length args) (List.length kwargs);
              List.length sign.py_funcs_types_in - ndefaults <= List.length args + List.length kwargs &&
              List.length args + List.length kwargs <= List.length sign.py_funcs_types_in) pyannot.py_funca_sig in
          debug "|sigs| = %d" (List.length sigs);
          let is_method = split_dot_name @@ get_orig_vname pyannot.py_funca_var <> None in
          let filter_sig in_types in_args flow =
            List.iter2 (fun ty args -> debug "arg = %a, ty = %a" pp_expr args (Option.print pp_expr) ty) in_types in_args;
            List.fold_left2 (fun (flow_in, flow_notin) arg annot ->
                match annot with
                | None -> (flow_in, flow_notin)
                | Some ant ->
                  let e =
                    match ekind ant with
                    | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)} as tv, {ekind = E_constant (C_string s)}::types, []) ->
                      let nant = if is_method then
                          {ant with ekind = E_py_call (tv,
                                                       (mk_var
                                                          (mk_addr_attr (match ekind @@ List.hd args with
                                                               | E_py_object (a, _) -> a
                                                               | _ -> assert false) s T_any)
                                                          range)::types
                                                      , [])}
                        else ant in
                      mk_expr (E_py_check_annot (arg, nant)) range
                    | _ ->
                      mk_expr (E_py_check_annot (arg, ant)) range in
                  man.exec (mk_assume e range) flow_in,
                  Flow.join man.lattice (man.exec (mk_assume (mk_not e range) range) flow_in) flow_notin
              )  (flow, Flow.bottom_from flow) in_args in_types in
          let apply_sig flow signature =
            debug "[%a] apply_sig %a" pp_var pyannot.py_funca_var pp_py_func_sig signature;
            let cur = get_env T_cur man flow in
            let new_typevars = collect_typevars (if is_method then Some (List.hd args) else None) signature in
            (* il faut enelver des trucs là, je veux pas enlever les variables de classe *)
            debug "new_typevars: %a" TVMap.print new_typevars;
            debug "cur: %a" TVMap.print cur;
            let ncur = TVMap.fold2o
                TVMap.add
                TVMap.add
                (fun s tycur tynew acc ->
                   if ESet.equal tycur tynew then TVMap.add s tycur acc else
                     Exceptions.panic_at range "s = %a, tycur = %a, tynew = %a, acc = %a" Keys.print s ESet.print tycur ESet.print tynew TVMap.print acc)
                cur new_typevars TVMap.empty in
            let flow = set_env T_cur ncur man flow in
            let in_types, in_args =
              (* remove types having a default parameter and no argument *)
              (* FIXME: kwargs actually depend on the chosen py_func_sig...  *)
              (* we need to add kwargs to args smartly here *)
              (* failwith "otodkwargs"; *)
              let rec filter names types defaults args (acctypes, accargs) =
                match names, defaults, args with
                | nhd::ntl, dhd::dtl, ahd::atl ->
                  begin match List.find_opt (fun (vo, expr) -> match vo with
                      | None -> false
                      | Some v -> v = get_orig_vname nhd) kwargs with
                  | None ->
                    if dhd then
                      (* if the argument is optional and not replaced in the kwargs *)
                      (* the default argument has its default value, we don't check anything *)
                      filter ntl (List.tl types) dtl args (acctypes, accargs)
                    else
                      filter ntl (List.tl types) dtl atl (List.hd types :: acctypes, ahd :: accargs)
                  | Some (vo, expr) ->
                    if dhd then
                      (* the argument is optional and replaced in the kwargs *)
                      filter ntl (List.tl types) dtl args (List.hd types :: acctypes, expr :: accargs)
                    else
                      (* if the argument is not a default one, we keep both types and arguments *)
                      filter ntl (List.tl types) dtl atl (List.hd types :: acctypes, expr ::accargs)
                  end
                | _, _, [] -> List.rev acctypes, List.rev accargs
                | _ -> assert false
              in
              filter signature.py_funcs_parameters signature.py_funcs_types_in signature.py_funcs_defaults args ([], [])
            in
            let flow_ok, flow_notok = filter_sig in_types in_args flow in
            let annot_out =
              let e = Option.none_to_exn signature.py_funcs_type_out in
              {e with ekind =
                        match ekind e with
                        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)} as tv, {ekind = E_constant (C_string s)}::types, []) when is_method ->
                          E_py_call (tv, (
                              mk_var
                                (mk_addr_attr (match ekind @@ List.hd args with
                                     | E_py_object (a, _) -> a
                                     | _ -> assert false) s T_any)
                                range
                            )::types, [])
                        | k -> k
              } in
            let ret_var = mk_range_attr_var range "ret_var" T_any in
            man.exec (mk_add_var ret_var range) flow_ok |>
            man.exec (mk_stmt (S_py_annot (mk_var ret_var range,
                                           mk_expr (E_py_annot annot_out) range))
                        range), flow_notok, new_typevars, ret_var
          in
          Eval.join_list ~empty:(
            fun () ->
              let () = Format.fprintf Format.str_formatter "%a does not match any signature provided in the stubs" pp_var pyannot.py_funca_var in
              man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |> Eval.empty_singleton)
            (let evals, remaining =
               (List.fold_left (fun (acc, remaining_flow) sign ->
                    let nflow, flow_notok, ntypevars, ret_var = apply_sig remaining_flow sign in
                    debug "nflow after apply_sig = %a@\n" (Flow.print man.lattice.print) nflow;
                    let cur = get_env T_cur man nflow in
                    let ncur = TVMap.filter (fun tyvar _ -> not (TVMap.mem tyvar ntypevars && match tyvar with | Global _ -> true | Class _ -> false)) cur in
                    let nflow = set_env T_cur ncur man nflow in
                    debug "nflow = %a@\n" (Flow.print man.lattice.print) nflow;
                    if Flow.is_bottom man.lattice nflow then (acc, flow_notok)
                    else
                      (Eval.singleton (mk_var ret_var range) nflow ~cleaners:([mk_remove_var ret_var range]) |> Eval.bind (man.eval)) :: acc, flow_notok
                  ) ([], flow) sigs) in
             let () = Format.fprintf Format.str_formatter "%a does not match any signature provided in the stubs" pp_var pyannot.py_funca_var in
             (man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) remaining |> Eval.empty_singleton) :: evals)
        )
      |> Option.return

    | E_py_annot e ->
      begin match ekind e with
        | E_var (v, mode) when is_builtin_name @@ get_orig_vname v ->
          let name = get_orig_vname v in
          begin match name with
          | "int" ->
            (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_integers
          | "float" ->
            (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_float
          | "NotImplementedType" ->
            (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_notimplemented
          | "NoneType" ->
            (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_none
          | _ ->
            Addr_env.Domain.allocate_builtin ~mode:WEAK man range flow (get_orig_vname v) (Some e)
          end
          |> Option.return

        | E_var (v, mode) ->
          debug "E_annot %s" v.vname;
          begin try
              let e = Hashtbl.find type_aliases v in
              debug "found type alias, replacing %a by %a" pp_var v pp_expr e;
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_annot e) range) flow |> Option.return
            with Not_found ->
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call e [] range) flow |> Option.return
          end

        | E_py_attribute ({ekind = E_var (v, _)}, s) ->
          debug "searching %a in the type aliases..." pp_expr e;
          begin
            try
              (* FIXME ouch, not found in man.eval would also get caught... *)
              (* FIXME: this also means that if a.pyi defines alias b and b.pyi too, we'll encounter some trouble *)
              let r = find_type_alias_by_name s in
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_annot r) range) flow |> Option.return
            with Not_found ->
              debug "not found, trying usual evaluation";
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |> Option.return
          end

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)} as e, i) when get_orig_vname c.py_cls_a_var = "Pattern" ->
          debug "Pattern!";
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call e [] range) flow
          |> Eval.bind (fun ee flow ->
              match ekind ee with
              | E_py_object (addr, _) ->
                debug "coucou";
                man.exec (mk_stmt (S_py_annot (mk_var (mk_addr_attr addr "typ" T_any) range, (mk_expr (E_py_annot i) range))) range) flow |>
                Eval.singleton ee
              | _ -> assert false
            )
          |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)} as e, i) when get_orig_vname c.py_cls_a_var <> "List" && get_orig_vname c.py_cls_a_var <> "Tuple" ->
          begin match c.py_cls_a_abases with
          | [] ->
            man.eval (mk_py_call (mk_py_object (find_builtin "object.__new__") range) [e] range) flow

          | abase :: _ ->
            man.eval (mk_py_call (mk_py_object (find_builtin "object.__new__") range) [e] range) flow |>
            Eval.bind (fun eobj flow ->
                let addr = match ekind eobj with
                  | E_py_object (a, _) -> a
                  | _ -> assert false in
                let tname = match ekind abase with
                  | E_py_index_subscript ({ekind = E_var (v, _)}, {ekind = E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::_, [])}) when get_orig_vname v = "Generic" -> s
                  | _ -> Exceptions.panic_at range "tname %a" pp_expr (List.hd c.py_cls_a_abases) in
                let flow = set_env T_cur (TVMap.add (Class (mk_addr_attr addr tname T_any)) (ESet.singleton i) (get_env T_cur man flow)) man flow in
                debug "after %a, cur = %a" pp_expr exp TVMap.print (get_env T_cur man flow);
                (* man.exec (mk_stmt (S_py_annot (mk_var (mk_addr_attr addr tname T_any) range, mk_expr (E_py_annot i) range)) range) flow |> *)
                Eval.singleton eobj flow
              )
          end
            |> Option.return

        | E_py_index_subscript ({ekind = E_py_object _} as e1, e2) ->
          warn_at range "E_py_annot subscript e1=%a e2=%a now in the wild" pp_expr e1 pp_expr e2;
          None

        | E_py_index_subscript (e1, e2) ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e1 flow |>
          bind_some (fun e1 flow ->
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot {e with ekind = E_py_index_subscript(e1, e2)}} flow
            )
          |> Option.return


        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::[], []) ->
          Exceptions.panic_at range "generic typevar annot"

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::types, []) ->
          let cur = get_env T_cur man flow in
          let tycur = TVMap.find (Global s) cur in
          debug "tycur = %a@\n" ESet.print tycur;
          begin match ESet.cardinal tycur with
          | 0 ->
            Flow.bottom_from flow |>
            Eval.empty_singleton
          | _ ->
            assert (ESet.cardinal tycur = 1);
            let ty = ESet.choose tycur in
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot ty} flow
          end |> Option.return


        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_var (v, _)}::types, []) ->
          let cur = get_env T_cur man flow in
          let tycur = TVMap.find (Class v) cur in
          debug "tycur = %a@\n" ESet.print tycur;
          begin match ESet.cardinal tycur with
          | 0 ->
            Flow.bottom_from flow |>
            Eval.empty_singleton
          | _ ->
            assert (ESet.cardinal tycur = 1);
            let ty = ESet.choose tycur in
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot ty} flow
          end |> Option.return

        | E_constant C_py_none ->
          (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_none |> Option.return

        | _ ->
          Exceptions.panic_at range "Unsupported type annotation %a@\n" pp_expr e
      end

    | E_py_check_annot (e, annot) ->
      begin match ekind annot with
        | E_var (v, mode) when is_builtin_name @@ get_orig_vname v ->
          man.eval (mk_py_isinstance_builtin e (get_orig_vname v) range) flow
          |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_user c, _)}, _)} as pattern, i) when get_orig_vname c.py_cls_var = "Pattern" ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
          Eval.bind (fun ee flow ->
              assume (mk_py_isinstance ee pattern range) man flow
                ~fthen:(fun flow ->
                    let ee_addr = match ekind ee with
                      | E_py_object (a, _) -> a
                      | _ -> assert false in
                    man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_check_annot (mk_var (mk_addr_attr ee_addr "typ" T_any) range, i)) range) flow
                  )
                ~felse:(fun flow ->
                    Eval.empty_singleton (Flow.bottom_from flow))
            )
          |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)} as pattern, i) when get_orig_vname c.py_cls_a_var = "Union" ->
          failwith "ok"

        | E_py_index_subscript ({ekind = E_py_object _} as e1, e2) ->
          warn_at range "E_py_check_annot subscript e1=%a e2=%a now in the wild" pp_expr e1 pp_expr e2;
          None

        | E_py_index_subscript (e1, e2) ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e1 flow |>
          bind_some (fun e1 flow ->
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) ({exp with ekind = E_py_check_annot (e, {annot with ekind = E_py_index_subscript (e1, e2)})}) flow
            )
          |> Option.return

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::[], []) ->
          Exceptions.panic_at range "Spycheckannot typevar"

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, (({ekind = E_constant (C_string _)} | {ekind = E_var (_, _)}) as v)::types, []) ->
          let key = match ekind v with
            | E_constant (C_string s) -> Keys.Global s
            | E_var (v, _) -> Class v
            | _ -> assert false in
          let flows_ok = List.fold_left (fun flows_caught typ ->
              let cur = get_env T_cur man flow in
              if not @@ TVMap.mem key cur then Exceptions.panic_at range "undef in cur";
              let flow = set_env T_cur (TVMap.add key (ESet.singleton typ) cur) man flow in
              man.exec (mk_assume {exp with ekind = E_py_check_annot (e, typ)} range) flow :: flows_caught)
              [] types in
          Eval.join_list ~empty:(fun () -> man.eval (mk_py_false range) flow)
            (List.map (man.eval (mk_py_true range)) flows_ok) |> Option.return

        | _ -> Exceptions.panic_at range "E_py_check_annot: %a not supported" pp_expr annot
      end

    | _ -> None

  let exec zone stmt man flow =
    match skind stmt with
    | S_rename ({ekind = E_py_annot {ekind = (E_addr a)}}, {ekind = E_addr a'}) ->
      debug "rename %a %a" pp_addr a pp_addr a';
      let cur = get_env T_cur man flow in
      let ncur = TVMap.map_p (fun (k, v) ->
          match k with
          | Class ({vkind = V_addr_attr (av, s) } as var) ->
            (* FIXME: we should probably change vname too *)
            (* FIXME: and this hold for all renames of V_addr_attr *)
            if compare_addr av a = 0 then
              (Class {var with vkind = V_addr_attr (a', s)}, v)
            else
              (k, v)
          | Global s -> (k, v)
          | _ -> assert false
        ) cur in
      debug "ncur = %a" TVMap.print ncur;
      set_env T_cur ncur man flow
      |> Post.return |> Option.return

    | _ -> None

  let ask _ _ _ = None
  let refine channel man flow = Channel.return flow
  let merge _ _ _ = assert false

end

let () =
  Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain)
