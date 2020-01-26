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
      | Global s1, Global s2 -> Stdlib.compare s1 s2
      | Class c1, Class c2 -> compare_var c1 c2
      | _ -> Stdlib.compare t1 t2
    let print fmt t  = match t with
      | Global s -> Format.fprintf fmt "Global %a" Format.pp_print_string s
      | Class v -> Format.fprintf fmt "Class %a" pp_var v
  end)


  module TVMap = Framework.Lattices.Partial_map.Make(Keys)(ESet)

  include TVMap

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

  let alarms = []

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
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function(F_annot pyannot)}, _)}, args, kwargs) when get_orig_vname pyannot.py_funca_var = "Generic.__new__" ->
      let cls = List.hd args in
      man.eval cls flow |>
      Eval.bind (fun ecls flow ->
          match ekind ecls with
          | E_py_object ({addr_kind = A_py_class (C_annot c, mro)}, _) ->
            let process_tyvar e = match ekind e with
              | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::types, []) ->
                (s, types)
              | _ -> Exceptions.panic_at range "process_tyvar %a" pp_expr e in
            let typevars =
              if c.py_cls_a_abases = [] then [] else
              match ekind @@ List.hd c.py_cls_a_abases with
              | E_py_index_subscript (_, vars) ->
                begin match ekind vars with
                  | E_py_call _ -> (process_tyvar vars)::[]
                  | E_py_tuple vs -> List.rev @@ List.fold_left (fun acc var ->  (process_tyvar var)::acc) [] vs
                  | E_var _ -> []
                  | _ -> panic_at range "vars = %a" pp_expr vars
                end
              | _ -> assert false in
            debug "typevars = %a" (Format.pp_print_list Format.pp_print_string) (List.map fst typevars);
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
                    | None ->
                      begin match TVMap.find_opt (Global tyname) cur with
                      | None ->
                        TVMap.add (Class tyvar) etypes cur
                      | Some set ->
                        if ESet.subset set etypes then
                          TVMap.add (Class tyvar) set cur
                        else
                          Exceptions.panic_at range "conflict for typevar %a, global set %a and class set %a differ" pp_var tyvar ESet.print set ESet.print etypes
                      end
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
      let exception Invalid_sig in
      (* FIXME: handle decorators... *)
      bind_list args man.eval flow |>
      bind_some (fun args flow ->
          let sigs = List.filter (fun sign ->
              let ndefaults  = List.fold_left (fun count el -> if el then count + 1 else count) 0 sign.py_funcs_defaults in
              debug "filter %a at range %a -> [%d; %d]; |args| = %d, |kwargs| = %d" pp_py_func_sig sign pp_range range (List.length sign.py_funcs_types_in - ndefaults) (List.length sign.py_funcs_types_in) (List.length args) (List.length kwargs);
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
                  Flow.join man.lattice (man.exec (mk_assume (mk_py_not e range) range) flow_in) flow_notin
              )  (flow, Flow.bottom_from flow) in_args in_types in
          let apply_sig flow signature =
            debug "[%a] apply_sig %a" pp_var pyannot.py_funca_var pp_py_func_sig signature;
            let cur = get_env T_cur man flow in
            let new_typevars = collect_typevars (if is_method then Some (List.hd args) else None) signature in
            (* il faut enelver des trucs là, je veux pas enlever les variables de classe *)
            debug "new_typevars: %a" TVMap.print new_typevars;
            debug "cur: %a" TVMap.print cur;
            debug "kwargs: %a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (fun fmt (argn, argv) -> Format.fprintf fmt "(%a, %a)" (Option.print Format.pp_print_string) argn pp_expr argv)) kwargs;
            let ncur = TVMap.fold2zo
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
                debug "calling filter names=%a; types=_; defaults=%a; args=%a"
                  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var) names
                  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_bool) defaults
                  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) args;
                match names, defaults, args with
                | nhd::ntl, dhd::dtl, ahd::atl ->
                  begin match List.find_opt (fun (vo, expr) -> match vo with
                      | None -> false
                      | Some v -> v = get_orig_vname nhd) kwargs with
                  | None ->
                    if dhd && List.length args = 0 (* List.length names > List.length args *) then
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
                | _ ->
                  debug "%d %d %d; raising Invalid_sig" (List.length names) (List.length defaults) (List.length args);
                  raise Invalid_sig
                  (* ;
                   * panic_at range "filter names=%a defaults=%a args=%a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var) names
                   *        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_bool) defaults
                   *        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) args *)
              in
              filter signature.py_funcs_parameters signature.py_funcs_types_in signature.py_funcs_defaults args ([], [])
            in
            let flow_ok, flow_notok = filter_sig in_types in_args flow in
            let ret_var = mk_range_attr_var range "ret_var" T_any in
            begin match signature.py_funcs_type_out with
              | Some e ->
                debug "out = %a" pp_expr e;
                let annot_out = Visitor.map_expr
                  (fun expr -> match ekind expr with
                     | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)} as tv, {ekind = E_constant (C_string s)}::types, []) when is_method ->
                       Keep {expr with ekind = E_py_call (tv, (
                           mk_var
                             (mk_addr_attr (match ekind @@ List.hd args with
                                  | E_py_object (a, _) -> a
                                  | _ -> assert false) s T_any)
                             range
                         )::types, [])}
                     | _ -> VisitParts expr)
                  (fun stmt -> VisitParts stmt)
                  e in
              man.exec (mk_add_var ret_var range) flow_ok |>
              man.exec (mk_stmt (S_py_annot (mk_var ret_var range,
                                             mk_expr (E_py_annot annot_out) range))
                          range), flow_notok, new_typevars, ret_var
              | None -> assert false
            end
          in
          let msg = Format.fprintf Format.str_formatter "%a(%a) does not match any signature provided in the stubs" pp_var pyannot.py_funca_var (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp_expr) args;
            Format.flush_str_formatter () in
          Eval.join_list ~empty:(
            fun () ->
              man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow |> Eval.empty_singleton)
            (let evals, remaining =
               (List.fold_left (fun (acc, remaining_flow) sign ->
                    let is_noreturn =
                      match sign.py_funcs_type_out with
                      | Some {ekind = E_var (v, _)} -> get_orig_vname v = "NoReturn"
                      | _ -> false in
                    try
                      let nflow, flow_notok, ntypevars, ret_var = apply_sig remaining_flow sign in
                      debug "nflow after apply_sig = %a@\n" (Flow.print man.lattice.print) nflow;
                      let cur = get_env T_cur man nflow in
                      let ncur = TVMap.filter (fun tyvar _ -> not (TVMap.mem tyvar ntypevars && match tyvar with | Global _ -> true | Class _ -> false)) cur in
                      let nflow = set_env T_cur ncur man nflow in
                      debug "nflow = %a@\n" (Flow.print man.lattice.print) nflow;
                      if Flow.is_bottom man.lattice nflow then (acc, flow_notok)
                      else
                        let raised_exn = List.map (fun exn ->
                            man.exec (mk_stmt (S_py_raise (Some exn)) range) flow |>
                            Eval.empty_singleton
                          ) sign.py_funcs_exceptions in
                        let () = debug "raised_exn %d" (List.length sign.py_funcs_exceptions) in
                        if is_noreturn then
                          raised_exn @ acc, flow_notok
                        else
                          let ret = (Eval.singleton (mk_var ret_var range) nflow ~cleaners:([mk_remove_var ret_var range]) |> Eval.bind (man.eval)) in
                          ret::raised_exn @ acc, flow_notok
                    with Invalid_sig ->
                      (acc, remaining_flow)
                  ) ([], flow) sigs) in
             (man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) remaining |> Eval.empty_singleton) :: evals)
        )
      |> Option.return

    | E_py_annot e ->
      begin match ekind e with
        | E_var (v, mode) when is_builtin_var v ->
          let name = get_orig_vname v in
          begin match name with
            | "bool" ->
              (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_bool_top
            | "int" ->
              (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_integers
            | "float" ->
              (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_float
            | "NotImplementedType" ->
              (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_notimplemented
            | "NoneType" ->
              (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_none
            (* | "Any" ->
             *   warn_at range "any annot";
             *   (\* FIXME man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_any range) flow *\)
             *   Addr_env.Domain.allocate_builtin ~mode:WEAK man range flow "object" (Some e) *)
            | "object" ->
              warn_at range "Any transformed into object here";
              T_string.Domain.allocate_builtin ~mode:WEAK man range flow (get_orig_vname v) (Some e)
            | _ ->
              T_string.Domain.allocate_builtin ~mode:WEAK man range flow (get_orig_vname v) (Some e)
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

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Optional" ->
          Eval.join_list
            ~empty:(fun () -> assert false)
            (List.map (fun e -> man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow)
               [mk_expr (E_py_annot i) range;
                mk_py_none range])
          |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Union" ->
          let is = match ekind i with
            | E_py_tuple t -> t
            | _ -> assert false in
          Eval.join_list
            ~empty:(fun () -> panic_at range "Union[]")
            (List.map
               (fun e -> man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_annot e) range) flow)
               is)
          |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Type" ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) i flow
          |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)} as e, i) when
            (* issue: if object.__new__(e) renames addresses used in i, this is not caught... *)
            List.for_all (fun n -> get_orig_vname c.py_cls_a_var <> n) ["List"; "Tuple"; "Dict"; "Optional"; "Union"; "Type"] ->
          begin match c.py_cls_a_abases with
            | [] ->
              man.eval (mk_py_call (mk_py_object (find_builtin "object.__new__") range) [e] range) flow
            | abase :: _ ->
              man.eval (mk_py_call (mk_py_object (find_builtin "object.__new__") range) [e] range) flow |>
              Eval.bind (fun eobj flow ->
                  let addr = match ekind eobj with
                    | E_py_object (a, _) -> a
                    | _ -> assert false in
                  let tnames = match ekind abase with
                    | E_py_index_subscript ({ekind = E_var (v, _)},
                                            {ekind = E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)},
                                                                {ekind = E_constant (C_string s)}::_, [])})
                      when get_orig_vname v = "Generic" || get_orig_vname v = "Protocol" -> [s]
                    | E_py_index_subscript ({ekind = E_var (v, _)},
                                            {ekind = E_py_tuple types}) ->
                      List.map (fun ty -> match ekind ty with
                          | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)},
                                       {ekind = E_constant (C_string s)}::_, [])
                            when get_orig_vname v = "Generic" || get_orig_vname v = "Protocol" -> s
                          | _ -> assert false) types
                    | E_py_index_subscript ({ekind = E_var (v, _)},
                                            {ekind = E_var _}) -> []
                    | _ ->
                      Exceptions.panic_at range "tname %a, abase %a" pp_expr (List.hd c.py_cls_a_abases) pp_expr abase in
                  debug "here, tnames=%a, cur = %a" (Format.pp_print_list Format.pp_print_string) tnames TVMap.print (get_env T_cur man flow);
                  let substi =
                    Visitor.map_expr
                      (fun expr -> match ekind expr with
                         (* il faudrait gérer les tuples comme avant donc c'est pas un fold_left qu'on veut sur les listes et probablement pas un Visitor.map_expr. *)
                         (* si c'est un tuple le visiteur devrait trouver chacun des éléments. Maintenant c'est Mapping[Mapping[str, int], int], comment on fait ? KT: Mapping[str, int], VT: int *)
                         | E_py_call (
                             {ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)},
                             {ekind = E_constant (C_string s)}::_, []) ->
                           (* FIXME: égalité entre s et tname à gérer mieux que ça ? *)
                           Keep (try ESet.choose @@ (TVMap.find (Global s) (get_env T_cur man flow)) with Not_found -> expr)
                         | E_py_call (
                             {ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)},
                             {ekind = E_var (vname, _)}::_, []) ->
                           Keep (
                             try ESet.choose @@ match TVMap.find_opt (Class vname) (get_env T_cur man flow) with
                               | Some s -> s
                               | None ->
                                 debug "vname = %a@\n" pp_var vname;
                                 match vkind vname with
                                 (* to re-try on re.finditer *)
                                 (* FIXME: ugly fix to handle:             (* issue: if object.__new__(e) renames addresses used in i, this is not caught... *) *)
                                 | V_addr_attr ({addr_kind = A_py_instance {addr_kind = A_py_class (C_annot c', _)}} as addr, attr) when compare_var c.py_cls_a_var c'.py_cls_a_var = 0 ->
                                   Option.default (ESet.singleton expr) (TVMap.find_opt (Class (mk_addr_attr {addr with addr_mode = WEAK} attr T_any)) (get_env T_cur man flow))
                                 | _ -> ESet.singleton expr
                             with Not_found -> expr)
                         | _ -> VisitParts expr
                      )
                      (fun stmt -> VisitParts stmt)
                      i in
                  let types = match ekind substi with
                    | E_py_tuple t -> t
                    | _ -> [substi] in
                  debug "types=%a" (Format.pp_print_list pp_expr) types;
                  let types = if tnames = [] then
                      let () = warn_at range "something wrong with types/tnames type_annot" in
                      [] else types in
                  let flow =
                    List.fold_left2 (fun flow tname ctype  ->
                        set_env T_cur (TVMap.add (Class (mk_addr_attr addr tname T_any))
                                         (match TVMap.find_opt (Global tname) (get_env T_cur man flow) with
                                          | None ->
                                            debug "tname(%s) not found in cur" tname;
                                            debug "cur = %a" TVMap.print (get_env T_cur man flow);
                                            ESet.singleton ctype

                                          | Some st ->
                                            debug "tname(%s) found in cur" tname;
                                            st)
                                         (get_env T_cur man flow)) man flow) flow tnames types in
                  debug "after %a, cur = %a" pp_expr exp TVMap.print (get_env T_cur man flow);
                  (* man.exec (mk_stmt (S_py_annot (mk_var (mk_addr_attr addr tname T_any) range, mk_expr (E_py_annot i) range)) range) flow |> *)
                  Eval.singleton eobj flow
                )
          end
          |> Option.return


        | E_py_index_subscript ({ekind = E_py_object _}, e2) ->
          None

        | E_py_index_subscript (e1, e2) ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e1 flow |>
          bind_some (fun e1 flow ->
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot {e with ekind = E_py_index_subscript(e1, e2)}} flow
            )
          |> Option.return


        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::[], []) ->
          let cur = get_env T_cur man flow in
          let tycur = TVMap.find (Global s) cur in
          if ESet.cardinal tycur = 0 then
            Flow.bottom_from flow
            |> Eval.empty_singleton
            |> Option.return
          else
            let () = assert (ESet.cardinal tycur = 1) in
            let ty = ESet.choose tycur in
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot ty} flow
            |> Option.return

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::types, []) ->
          let cur = get_env T_cur man flow in
          begin match TVMap.find_opt (Global s) cur with
          | Some tycur ->
            debug "tycur = %a@\n" ESet.print tycur;
            begin match ESet.cardinal tycur with
              | 0 ->
                Flow.bottom_from flow |>
                Eval.empty_singleton
              | _ ->
                assert (ESet.cardinal tycur = 1);
                let ty = ESet.choose tycur in
                man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot ty} flow
            end
          | _ ->
            Eval.join_list ~empty:(fun () -> assert false)
              (List.map (fun t -> man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot t} flow) types)
          end |> Option.return

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_var (v, _)}::types, []) ->
          let cur = get_env T_cur man flow in
          let tycur = try TVMap.find (Class v) cur with Not_found ->
            let () = Soundness.warn_at range "cheating in type annots" in ESet.of_list types in
          debug "tycur = %a@\n" ESet.print tycur;
          begin match ESet.cardinal tycur with
          | 0 ->
            Flow.bottom_from flow |>
            Eval.empty_singleton
          | _ ->
            Eval.join_list
              ~empty:(fun () -> assert false)
              (List.map
                 (fun t ->
                    let flow = set_env T_cur (TVMap.add (Class v) (ESet.singleton t) cur) man flow in
                    man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot t} flow)
                 (ESet.elements tycur))
            (* assert (ESet.cardinal tycur = 1);
             * let ty = ESet.choose tycur in
             * man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot ty} flow *)
          end |> Option.return

        | E_constant C_py_none ->
          (fun s -> Eval.singleton (mk_py_object (s (), None) range) flow) Addr_env.addr_none |> Option.return

        | E_py_object ({addr_kind = A_py_class _}, _) ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call e [] range) flow
          |> Option.return

        | _ ->
          Exceptions.panic_at range "Unsupported type annotation %a@\n" pp_expr e
      end

    | E_py_check_annot (e, annot) ->
      begin match ekind annot with
        | E_var (v, mode) when is_builtin_var v ->
          man.eval (mk_py_isinstance_builtin e (get_orig_vname v) range) flow
          |> Option.return

        | E_constant (C_py_none) ->
          man.eval (mk_py_isinstance_builtin e "NoneType" range) flow |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, cmro)}, _)}, i) when
            List.exists (fun (v, _) ->
                match akind v with
                | A_py_class (C_annot cv, _) -> get_orig_vname cv.py_cls_a_var = "Protocol" (* if Protocol[T], we should check T too, but this isn't done currently *)
                | _ -> false) cmro ->
          let attrs_check_expr =
            if c.py_cls_a_static_attributes = [] then mk_py_true range else
              List.fold_left (fun acc el -> mk_binop (mk_py_hasattr e (get_orig_vname el) range) O_py_and acc range)
                (mk_py_hasattr e (get_orig_vname (List.hd c.py_cls_a_static_attributes)) range)
                (List.tl c.py_cls_a_static_attributes) in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) attrs_check_expr flow |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Type" ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
          Eval.bind (fun ee flow ->
              match ekind ee with
              | E_py_object ({addr_kind = A_py_class _}, _) ->
                assume (mk_py_issubclass ee i range) man flow
                  ~fthen:(fun flow ->
                      man.eval (mk_py_true range) flow)
                  ~felse:(fun flow ->
                      Eval.empty_singleton (Flow.bottom_from flow))
              | _ -> Eval.empty_singleton (Flow.bottom_from flow)
            )
          |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, {ekind = E_py_tuple [args; out]}) when get_orig_vname c.py_cls_a_var = "Callable" ->
          assume (mk_py_hasattr e "__call__" range) man flow
            ~fthen:(fun flow ->
                Soundness.warn_at range "Callable type annotation partially supported";
                  man.eval (mk_py_true range) flow
              )
            ~felse:(man.eval (mk_py_false range))
          |> Option.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)} as pattern, i) when get_orig_vname c.py_cls_a_var = "Pattern" ->
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

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Union" ->
          let types = match ekind i with
            | E_py_tuple t -> t
            | _ -> assert false in
          let mk_cannot a = {exp with ekind = E_py_check_annot(e, a)} in
          let mk_or e1 e2 = mk_binop e1 O_py_or e2 range in
          let conds = List.fold_left (fun acc elu ->
              mk_or acc (mk_cannot elu)
            ) (mk_cannot @@ List.hd types) (List.tl types) in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) conds flow
          |> Option.return
          (* big disjunction on check_annot(e, t) for t in types *)

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Optional" ->
          let mk_cannot a = {exp with ekind = E_py_check_annot(e, a)} in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_binop (mk_cannot i) O_py_or (mk_cannot (mk_py_none range)) range) flow |> Option.return

        | E_py_index_subscript ({ekind = E_py_object _}, e2) ->
          None

        | E_py_index_subscript (e1, e2) ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e1 flow |>
          bind_some (fun e1 flow ->
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) ({exp with ekind = E_py_check_annot (e, {annot with ekind = E_py_index_subscript (e1, e2)})}) flow
            )
          |> Option.return

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, (({ekind = E_constant (C_string _)} | {ekind = E_var (_, _) }) as v)::[], []) ->
          let key = match ekind v with
            | E_constant (C_string s) -> Keys.Global s
            | E_var (v, _) -> Keys.Class v
            | _ -> assert false in
          debug "check_annot typevar string not type";
          let cur = get_env T_cur man flow in
          begin match TVMap.find_opt key cur with
            | Some types ->
              let flows_ok = ESet.fold (fun typ flows_caught ->
                  let flow = set_env T_cur (TVMap.add key (ESet.singleton typ) cur) man flow in
                  man.exec (mk_assume {exp with ekind = E_py_check_annot(e, typ)} range) flow :: flows_caught
                ) types [] in
              Eval.join_list ~empty:(fun () -> man.eval (mk_py_false range) flow)
                (List.map (man.eval (mk_py_true range)) flows_ok) |> Option.return
            | None -> assert false
          end

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, (({ekind = E_constant (C_string _)} | {ekind = E_var (_, _) }) as v)::types, []) ->
          debug "check_annot typevar stringorvar with %d types" (List.length types);
          let key = match ekind v with
            | E_constant (C_string s) -> Keys.Global s
            | E_var (v, _) -> Keys.Class v
            | _ -> assert false in
          debug "key = %a" Keys.print key;
          let flows_ok =
            if Flow.is_bottom man.lattice flow then [] else
            List.fold_left (fun flows_caught typ ->
              let cur = get_env T_cur man flow in
              if not @@ TVMap.mem key cur then
                flows_caught
              else
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
      let cur = get_env T_cur man flow in
      debug "rename %a %a, at %a@\ncur=%a" pp_addr a pp_addr a' pp_range stmt.srange TVMap.print cur;
      let ncur =
        let abasedaddr, other = TVMap.fold (fun k v (acc_a, acc_nota) ->
            match k with
            | Class ({vkind = V_addr_attr (av, s)}) when compare_addr av a = 0 ->
              (TVMap.add (Class (mk_addr_attr a' s T_any)) v acc_a, acc_nota)
            | _ ->
              (acc_a, TVMap.add k v acc_nota)
          ) cur (TVMap.empty, TVMap.empty) in
        TVMap.join abasedaddr other in
      debug "ncur = %a" TVMap.print ncur;
      set_env T_cur ncur man flow
      |> Post.return |> Option.return

    | _ -> None

  let ask _ _ _ = None
  let refine channel man flow = Channel.return flow
  let merge pre (a, log) (a', log') =
    if a == a' then a
    else if Log.is_empty log' then a
    else if Log.is_empty log then a'
    else let () = debug "pre=%a@.a=%alog=%a@.a'=%alog'=%a@." print pre print a Log.print log print a' Log.print log' in assert false
end

let () =
  Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain)
