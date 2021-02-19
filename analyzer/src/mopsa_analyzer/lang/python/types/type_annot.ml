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

(** Definition of python function annotations as well as polymorphism *)
(** This module is still experimental *)

open Mopsa
open Sig.Abstraction.Domain
open Ast
open Addr
open Universal.Ast


module Domain =
struct

  module ESet = Framework.Lattices.Powerset.Make(struct type t = expr let compare = compare_expr let print = unformat pp_expr end)

  module Keys = (struct
    type t = Global of string | Class of var
    let compare t1 t2 =
      match t1, t2 with
      | Global s1, Global s2 -> Stdlib.compare s1 s2
      | Class c1, Class c2 -> compare_var c1 c2
      | _ -> Stdlib.compare t1 t2
    let print printer t  = match t with
      | Global s -> pprint printer (fbox "Global %a" Format.pp_print_string s)
      | Class v  -> pprint printer (fbox "Class %a" pp_var v)
  end)


  module TVMap = Framework.Lattices.Partial_map.Make(Keys)(
                     struct
                       include ESet
                       let widen ctx e1 e2 =
                         if ESet.cardinal e1 > 5 && ESet.cardinal e2 > 5 then ESet.widen ctx e1 e2 else join e1 e2
                     end
                   )

  include TVMap

  include GenDomainId(struct
      type nonrec t = t
      let name = "python.types.type_annot"
    end)

  let checks = []

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
                 let var = match self with
                   | None ->
                     Keys.Global s
                   | Some {ekind = E_py_object (addr, _)} ->
                     Keys.Class (mk_addr_attr addr s (T_py None))
                   | _ -> assert false in
                 debug "var = %a@.acc = %a" (format Keys.print) var (format TVMap.print) acc;
                 begin match TVMap.find_opt var acc with
                   | None ->
                     (begin match var with
                        | Keys.Class _ -> Keep acc
                        | Keys.Global _ -> Keep (TVMap.add var set acc)
                      end)
                   | Some set2 ->
                      if ESet.equal set set2 then Keep acc
                      else Exceptions.panic_at (erange expr) "conflict for typevar %s, sets %a and %a differ" s (format ESet.print) set (format ESet.print) set2
                 end
               | _ ->
                 VisitParts acc)
            (fun acc stmt -> VisitParts acc)
            acc ty) base signature.py_funcs_types_in

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function(F_annot pyannot)}, _)}, args, kwargs) when get_orig_vname pyannot.py_funca_var = "Generic.__new__" ->
      let cls = List.hd args in
      man.eval cls flow >>$
 (fun ecls flow ->
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
            man.eval (mk_py_call (mk_py_attr (mk_py_object nextinmro range)  "__new__" range) args range) flow >>$
 (fun eobj flow ->
                let addr_eobj = match ekind eobj with
                  | E_py_object (a, _) -> a
                  | _ -> assert false in
                let cur = get_env T_cur man flow in
                let ncur = List.fold_left (fun cur (tyname, types) ->
                    let etypes = ESet.of_list types in
                    let tyvar = mk_addr_attr addr_eobj tyname (T_py None) in
                    match TVMap.find_opt (Class tyvar) cur with
                    | None ->
                      begin match TVMap.find_opt (Global tyname) cur with
                      | None ->
                        TVMap.add (Class tyvar) etypes cur
                      | Some set ->
                         if ESet.subset set etypes then
                           TVMap.add (Class tyvar) set cur
                         else
                           Exceptions.panic_at range "conflict for typevar %a, global set %a and class set %a differ" pp_var tyvar (format ESet.print) set (format ESet.print) etypes
                      end
                    | Some set ->
                       if ESet.subset set etypes then
                         cur
                       else
                         Exceptions.panic_at range "conflict for typevar %a, sets %a and %a differ" pp_var tyvar (format ESet.print) set (format ESet.print) etypes
                  ) cur typevars in
                let flow = set_env T_cur ncur man flow in
                Eval.singleton eobj flow
              )
          | _ -> assert false
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function(F_annot pyannot)}, _)}, args, kwargs) ->
      let exception Invalid_sig in
      (* FIXME: handle decorators... *)
      bind_list args man.eval flow |>
      bind_result (fun args flow ->
          let sigs = List.filter (fun sign ->
              let ndefaults  = List.fold_left (fun count el -> if el then count + 1 else count) 0 sign.py_funcs_defaults in
              debug "filter %a at range %a -> [%d; %d]; |args| = %d, |kwargs| = %d" pp_py_func_sig sign pp_range range (List.length sign.py_funcs_types_in - ndefaults) (List.length sign.py_funcs_types_in) (List.length args) (List.length kwargs);
              List.length sign.py_funcs_types_in - ndefaults <= List.length args + List.length kwargs &&
              List.length args + List.length kwargs <= List.length sign.py_funcs_types_in) pyannot.py_funca_sig in
          debug "|sigs| = %d" (List.length sigs);
          let is_method = split_dot_name @@ get_orig_vname pyannot.py_funca_var <> None in
          let filter_sig in_types in_args flow =
            List.iter2 (fun ty args -> debug "arg = %a, ty = %a" pp_expr args (OptionExt.print pp_expr) ty) in_types in_args;
            List.fold_left2 (fun (flow_in, flow_notin) arg annot ->
                match annot with
                | None -> (flow_in, flow_notin)
                | Some ant ->
                   let nant = Visitor.map_expr
                                (fun expr -> match ekind expr with
                                             | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)} as tv, {ekind = E_constant (C_string s)}::types, []) ->
                                                let nant = if is_method then
                                                             {ant with ekind = E_py_call (tv,
                                                                                          (mk_var
                                                                                             (mk_addr_attr (match ekind @@ List.hd args with
                                                                                                            | E_py_object (a, _) -> a
                                                                                                            | _ -> assert false) s (T_py None))
                                                                                             range)::types
                                                                                          , [])}
                                                           else ant in
                                                Keep nant
                                             | _ -> VisitParts expr
                                )
                                (fun stmt -> VisitParts stmt) ant in
                  let e = mk_expr ~etyp:(T_py None) (E_py_check_annot (arg, nant)) range in
                  post_to_flow man (man.exec (mk_assume e range) flow_in),
                  Flow.join man.lattice (post_to_flow man (man.exec (mk_assume (mk_py_not e range) range) flow_in)) flow_notin
              )  (flow, Flow.bottom_from flow) in_args in_types in
          let apply_sig flow signature =
            debug "[%a] apply_sig %a" pp_var pyannot.py_funca_var pp_py_func_sig signature;
            let cur = get_env T_cur man flow in
            let new_typevars = collect_typevars (if is_method then Some (List.hd args) else None) signature in
            debug "new_typevars: %a" (format TVMap.print) new_typevars;
            debug "cur: %a" (format TVMap.print) cur;
            debug "kwargs: %a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (fun fmt (argn, argv) -> Format.fprintf fmt "(%a, %a)" (OptionExt.print Format.pp_print_string) argn pp_expr argv)) kwargs;
            let ncur = TVMap.fold2zo
                TVMap.add
                TVMap.add
                (fun s tycur tynew acc ->
                   if ESet.equal tycur tynew then TVMap.add s tycur acc else
                     Exceptions.panic_at range "s = %a, tycur = %a, tynew = %a, acc = %a" (format Keys.print) s (format ESet.print) tycur (format ESet.print) tynew (format TVMap.print) acc)
                cur new_typevars TVMap.empty in
            let flow = set_env T_cur ncur man flow in
            let in_types, in_args =
              (* remove types having a default parameter and no argument *)
              (* FIXME: kwargs actually depend on the chosen py_func_sig...  *)
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
                    if dhd && List.length args = 0 then
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
              in
              filter signature.py_funcs_parameters signature.py_funcs_types_in signature.py_funcs_defaults args ([], [])
            in
            let flow_ok, flow_notok = filter_sig in_types in_args flow in
            let ret_var = mk_range_attr_var range "ret_var" (T_py None) in
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
                                  | _ -> assert false) s (T_py None))
                             range
                         )::types, [])}
                     | _ -> VisitParts expr)
                  (fun stmt -> VisitParts stmt)
                  e in
              man.exec (mk_add_var ret_var range) flow_ok >>%
              man.exec (mk_stmt (S_py_annot (mk_var ret_var range,
                                             mk_expr ~etyp:(T_py None) (E_py_annot annot_out) range))
                          range), flow_notok, new_typevars, ret_var
              | None -> assert false
            end
          in
          let msg = Format.asprintf "%a(%a) does not match any signature provided in the stubs" pp_var pyannot.py_funca_var (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp_expr) args in
          Eval.join_list ~empty:(
            fun () ->
              man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>% Eval.empty)
            (let evals, remaining =
               (List.fold_left (fun (acc, remaining_flow) sign ->
                    let is_noreturn =
                      match sign.py_funcs_type_out with
                      | Some {ekind = E_var (v, _)} -> get_orig_vname v = "NoReturn"
                      | _ -> false in
                    try
                      let nflow, flow_notok, ntypevars, ret_var = apply_sig remaining_flow sign in
                      let nflow = post_to_flow man nflow in
                      debug "nflow after apply_sig = %a@\n" (format (Flow.print man.lattice.print)) nflow;
                      let cur = get_env T_cur man nflow in
                      let ncur = TVMap.filter (fun tyvar _ -> not (TVMap.mem tyvar ntypevars && match tyvar with | Global _ -> true | Class _ -> false)) cur in
                      let nflow = set_env T_cur ncur man nflow in
                      debug "nflow = %a@\n" (format (Flow.print man.lattice.print)) nflow;
                      if Flow.is_bottom man.lattice nflow then (acc, flow_notok)
                      else
                        let raised_exn = List.map (fun exn ->
                            man.exec (mk_stmt (S_py_raise (Some exn)) range) flow >>%
                            Eval.empty
                          ) sign.py_funcs_exceptions in
                        let () = debug "raised_exn %d" (List.length sign.py_funcs_exceptions) in
                        if is_noreturn then
                          raised_exn @ acc, flow_notok
                        else
                          let ret = (Eval.singleton (mk_var ret_var range) nflow ~cleaners:([mk_remove_var ret_var range]) >>$
 (man.eval)) in
                          ret::raised_exn @ acc, flow_notok
                    with Invalid_sig ->
                      (acc, remaining_flow)
                  ) ([], flow) sigs) in
             (man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) remaining >>% Eval.empty) :: evals)
        )
      |> OptionExt.return

    | E_py_annot e ->
      begin match ekind e with
        | E_var (v, mode) when is_builtin_var v ->
          let name = get_orig_vname v in
          begin match name with
            | "bool" ->
               Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_bool_top, OptionExt.return @@ mk_top T_bool range) range) flow
            | "int" ->
               Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_integers, OptionExt.return @@ mk_top T_int range) range) flow
            | "float" ->
               Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_float, OptionExt.return @@ mk_top (T_float F_DOUBLE) range) range) flow
            | "str" ->
               Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_strings, OptionExt.return @@ mk_top T_string range) range) flow
            | "NotImplementedType" ->
               Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_notimplemented, None) range) flow
            | "NoneType" ->
               Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_none, None) range) flow
            (* | "Any" ->
             *   warn_at range "any annot";
             *   (\* FIXME man.eval   (mk_py_top T_any range) flow *\)
             *   Addr_env.Domain.allocate_builtin ~mode:WEAK man range flow "object" (Some e) *)
            | "object" ->
              warn_at range "Any transformed into object here";
              T_string.Domain.allocate_builtin ~mode:WEAK man range flow (get_orig_vname v) (Some e)
            | _ ->
              T_string.Domain.allocate_builtin ~mode:WEAK man range flow (get_orig_vname v) (Some e)
          end
          |> OptionExt.return

        | E_var (v, mode) ->
          debug "E_annot %s" v.vname;
          begin try
              let e = Hashtbl.find type_aliases v in
              debug "found type alias, replacing %a by %a" pp_var v pp_expr e;
              man.eval   (mk_expr ~etyp:(T_py None) (E_py_annot e) range) flow |> OptionExt.return
            with Not_found ->
              man.eval   (mk_py_call e [] range) flow |> OptionExt.return
          end

        | E_py_attribute ({ekind = E_var (v, _)}, s) ->
          debug "searching %a in the type aliases..." pp_expr e;
          begin
            try
              (* FIXME ouch, not found in man.eval would also get caught... *)
              (* FIXME: this also means that if a.pyi defines alias b and b.pyi too, we'll encounter some trouble *)
              let r = find_type_alias_by_name s in
              man.eval   (mk_expr ~etyp:(T_py None) (E_py_annot r) range) flow |> OptionExt.return
            with Not_found ->
              debug "not found, trying usual evaluation";
              man.eval   e flow |> OptionExt.return
          end

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)} as e, i) when get_orig_vname c.py_cls_a_var = "Pattern" ->
          debug "Pattern!";
          man.eval   (mk_py_call e [] range) flow
          >>$
 (fun ee flow ->
              match ekind ee with
              | E_py_object (addr, _) ->
                debug "coucou";
                man.exec (mk_stmt (S_py_annot (mk_var (mk_addr_attr addr "typ" (T_py None)) range, (mk_expr ~etyp:(T_py None) (E_py_annot i) range))) range) flow >>%
                Eval.singleton ee
              | _ -> assert false
            )
          |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Optional" ->
          Eval.join_list
            ~empty:(fun () -> assert false)
            (List.map (fun e -> man.eval   e flow)
               [mk_expr ~etyp:(T_py None) (E_py_annot i) range;
                mk_py_none range])
          |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Union" ->
          let is = match ekind i with
            | E_py_tuple t -> t
            | _ -> assert false in
          Eval.join_list
            ~empty:(fun () -> panic_at range "Union[]")
            (List.map
               (fun e -> man.eval   (mk_expr ~etyp:(T_py None) (E_py_annot e) range) flow)
               is)
          |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Type" ->
          man.eval   i flow
          |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)} as e, i) when
            (* issue: if object.__new__(e) renames addresses used in i, this is not caught... *)
            List.for_all (fun n -> get_orig_vname c.py_cls_a_var <> n) ["List"; "Tuple"; "Dict"; "Optional"; "Union"; "Type"] ->
          begin match c.py_cls_a_abases with
            | [] ->
              man.eval (mk_py_call (mk_py_object (find_builtin "object.__new__") range) [e] range) flow
            | abase :: _ ->
              man.eval (mk_py_call (mk_py_object (find_builtin "object.__new__") range) [e] range) flow >>$
 (fun eobj flow ->
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
                  debug "here, tnames=%a, cur = %a" (Format.pp_print_list Format.pp_print_string) tnames (format TVMap.print) (get_env T_cur man flow);
                  let substi =
                    Visitor.map_expr
                      (fun expr -> match ekind expr with
                         | E_py_call (
                             {ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)},
                             {ekind = E_constant (C_string s)}::_, []) ->
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
                                 (* FIXME: ugly fix to handle:             (* issue: if object.__new__(e) renames addresses used in i, this is not caught... *) *)
                                 | V_addr_attr ({addr_kind = A_py_instance {addr_kind = A_py_class (C_annot c', _)}} as addr, attr) when compare_var c.py_cls_a_var c'.py_cls_a_var = 0 ->
                                   OptionExt.default (ESet.singleton expr) (TVMap.find_opt (Class (mk_addr_attr {addr with addr_mode = WEAK} attr (T_py None))) (get_env T_cur man flow))
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
                        set_env T_cur (TVMap.add (Class (mk_addr_attr addr tname (T_py None)))
                                         (match TVMap.find_opt (Global tname) (get_env T_cur man flow) with
                                          | None ->
                                            debug "tname(%s) not found in cur" tname;
                                            debug "cur = %a" (format TVMap.print) (get_env T_cur man flow);
                                            ESet.singleton ctype

                                          | Some st ->
                                            debug "tname(%s) found in cur" tname;
                                            st)
                                         (get_env T_cur man flow)) man flow) flow tnames types in
                  debug "after %a, cur = %a" pp_expr exp (format TVMap.print) (get_env T_cur man flow);
                  Eval.singleton eobj flow
                )
          end
          |> OptionExt.return


        | E_py_index_subscript ({ekind = E_py_object _}, e2) ->
          None

        | E_py_index_subscript (e1, e2) ->
          man.eval   e1 flow |>
          bind_result (fun e1 flow ->
              man.eval   {exp with ekind = E_py_annot {e with ekind = E_py_index_subscript(e1, e2)}} flow
            )
          |> OptionExt.return


        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::[], []) ->
          let cur = get_env T_cur man flow in
          let tycur = TVMap.find (Global s) cur in
          if ESet.cardinal tycur = 0 then
            Flow.bottom_from flow
            |> Eval.empty
            |> OptionExt.return
          else
            let () = assert (ESet.cardinal tycur = 1) in
            let ty = ESet.choose tycur in
            man.eval   {exp with ekind = E_py_annot ty} flow
            |> OptionExt.return

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_constant (C_string s)}::types, []) ->
          let cur = get_env T_cur man flow in
          begin match TVMap.find_opt (Global s) cur with
          | Some tycur ->
            debug "tycur = %a@\n" (format ESet.print) tycur;
            begin match ESet.cardinal tycur with
              | 0 ->
                Flow.bottom_from flow |>
                Eval.empty
              | _ ->
                assert (ESet.cardinal tycur = 1);
                let ty = ESet.choose tycur in
                man.eval   {exp with ekind = E_py_annot ty} flow
            end
          | _ ->
            Eval.join_list ~empty:(fun () -> assert false)
              (List.map (fun t -> man.eval   {exp with ekind = E_py_annot t} flow) types)
          end |> OptionExt.return

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, {ekind = E_var (v, _)}::types, []) ->
          let cur = get_env T_cur man flow in
          let tycur = try TVMap.find (Class v) cur with Not_found ->
            (* FIXME: add assumption for:
               let () = Soundness.warn_at range "cheating in type annots" in
            *) ESet.of_list types in
          debug "tycur = %a@\n" (format ESet.print) tycur;
          begin match ESet.cardinal tycur with
          | 0 ->
            Flow.bottom_from flow |>
            Eval.empty
          | _ ->
            Eval.join_list
              ~empty:(fun () -> assert false)
              (List.map
                 (fun t ->
                    let flow = set_env T_cur (TVMap.add (Class v) (ESet.singleton t) cur) man flow in
                    man.eval   {exp with ekind = E_py_annot t} flow)
                 (ESet.elements tycur))
          end |> OptionExt.return

        | E_constant C_py_none ->
           Eval.singleton (mk_py_object (OptionExt.none_to_exn !Addr_env.addr_none, None) range) flow |> OptionExt.return

        | E_py_object ({addr_kind = A_py_class _}, _) ->
          man.eval   (mk_py_call e [] range) flow
          |> OptionExt.return

        | _ ->
          Exceptions.panic_at range "Unsupported type annotation %a@\n" pp_expr e
      end

    | E_py_check_annot (e, annot) ->
      begin match ekind annot with
        | E_var (v, mode) when is_builtin_var v ->
          man.eval (mk_py_isinstance_builtin e (get_orig_vname v) range) flow
          |> OptionExt.return

        | E_constant (C_py_none) ->
          man.eval (mk_py_isinstance_builtin e "NoneType" range) flow |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, cmro)}, _)}, i) when
            List.exists (fun (v, _) ->
                match akind v with
                | A_py_class (C_annot cv, _) -> get_orig_vname cv.py_cls_a_var = "Protocol" (* if Protocol[T], we should check T too, but this isn't done currently *)
                | _ -> false) cmro ->
          let attrs_check_expr =
            if c.py_cls_a_static_attributes = [] then mk_py_true range else
              List.fold_left (fun acc el -> mk_binop ~etyp:(T_py (Some Bool)) (mk_py_hasattr e (get_orig_vname el) range) O_py_and acc range)
                (mk_py_hasattr e (get_orig_vname (List.hd c.py_cls_a_static_attributes)) range)
                (List.tl c.py_cls_a_static_attributes) in
          man.eval   attrs_check_expr flow |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Type" ->
          man.eval   e flow >>$
 (fun ee flow ->
              match ekind ee with
              | E_py_object ({addr_kind = A_py_class _}, _) ->
                assume (mk_py_issubclass ee i range) man flow
                  ~fthen:(fun flow ->
                      man.eval (mk_py_true range) flow)
                  ~felse:(fun flow ->
                      Eval.empty (Flow.bottom_from flow))
              | _ -> Eval.empty (Flow.bottom_from flow)
            )
          |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, {ekind = E_py_tuple [args; out]}) when get_orig_vname c.py_cls_a_var = "Callable" ->
          assume (mk_py_hasattr e "__call__" range) man flow
            ~fthen:(fun flow ->
                let flow =
                  Flow.add_local_assumption
                    (A_ignore_unsupported_expr e)
                    range flow
                in
                (* Soundness.warn_at range "Callable type annotation partially supported"; *)
                man.eval (mk_py_true range) flow
              )
            ~felse:(man.eval (mk_py_false range))
          |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)} as pattern, i) when get_orig_vname c.py_cls_a_var = "Pattern" ->
          man.eval   e flow >>$
 (fun ee flow ->
              assume (mk_py_isinstance ee pattern range) man flow
                ~fthen:(fun flow ->
                    let ee_addr = match ekind ee with
                      | E_py_object (a, _) -> a
                      | _ -> assert false in
                    man.eval   (mk_expr ~etyp:(T_py None) (E_py_check_annot (mk_var (mk_addr_attr ee_addr "typ" (T_py None)) range, i)) range) flow
                  )
                ~felse:(fun flow ->
                    Eval.empty (Flow.bottom_from flow))
            )
          |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Union" ->
          let types = match ekind i with
            | E_py_tuple t -> t
            | _ -> assert false in
          let mk_cannot a = {exp with ekind = E_py_check_annot(e, a)} in
          let mk_or e1 e2 = mk_binop ~etyp:(T_py (Some Bool)) e1 O_py_or e2 range in
          let conds = List.fold_left (fun acc elu ->
              mk_or acc (mk_cannot elu)
            ) (mk_cannot @@ List.hd types) (List.tl types) in
          man.eval   conds flow
          |> OptionExt.return
          (* big disjunction on check_annot(e, t) for t in types *)

        | E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) when get_orig_vname c.py_cls_a_var = "Optional" ->
          let mk_cannot a = {exp with ekind = E_py_check_annot(e, a)} in
          man.eval   (mk_binop ~etyp:(T_py (Some Bool)) (mk_cannot i) O_py_or (mk_cannot (mk_py_none range)) range) flow |> OptionExt.return

        | E_py_index_subscript ({ekind = E_py_object _}, e2) ->
          None

        | E_py_index_subscript (e1, e2) ->
          man.eval   e1 flow |>
          bind_result (fun e1 flow ->
              man.eval   ({exp with ekind = E_py_check_annot (e, {annot with ekind = E_py_index_subscript (e1, e2)})}) flow
            )
          |> OptionExt.return

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
                (List.map (fun f -> f >>% man.eval (mk_py_true range)) flows_ok) |> OptionExt.return
            | None -> assert false
          end

        | E_py_call ({ekind = E_var ({vkind = V_uniq ("TypeVar", _)}, _)}, (({ekind = E_constant (C_string _)} | {ekind = E_var (_, _) }) as v)::types, []) ->
          debug "check_annot typevar stringorvar with %d types" (List.length types);
          let key = match ekind v with
            | E_constant (C_string s) -> Keys.Global s
            | E_var (v, _) -> Keys.Class v
            | _ -> assert false in
          debug "key = %a" (format Keys.print) key;
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
            (List.map (fun f -> f >>% man.eval (mk_py_true range)) flows_ok) |> OptionExt.return

        | _ -> Exceptions.panic_at range "E_py_check_annot: %a not supported" pp_expr annot
      end

    | _ -> None

  let exec stmt man flow =
    match skind stmt with
    | S_py_check_annot(x, e) ->
       let range = srange stmt in
       assume
         (mk_expr (E_py_check_annot(x, e)) range)
         man flow
         ~fthen:(fun flow ->
           Flow.add_safe_check Alarms.CHK_PY_INVALID_TYPE_ANNOTATION range flow |>
           Post.return
         )
         ~felse:(fun flow ->
           let cs = Flow.get_callstack flow in
           let alarm = mk_alarm (Alarms.A_py_invalid_type_annotation(x, e)) cs range in
           Flow.raise_alarm alarm ~bottom:false man.lattice flow |> Post.return
         )
       |> OptionExt.return

    | S_fold ({ekind = E_py_annot {ekind = E_addr (a', _)}}, [{ekind = (E_addr (a, _))}])
    | S_rename ({ekind = E_py_annot {ekind = (E_addr (a, _))}}, {ekind = E_addr (a', _)}) ->
      let cur = get_env T_cur man flow in
      debug "rename %a %a, at %a@\ncur=%a" pp_addr a pp_addr a' pp_range stmt.srange (format TVMap.print) cur;
      let ncur =
        let abasedaddr, other = TVMap.fold (fun k v (acc_a, acc_nota) ->
            match k with
            | Class ({vkind = V_addr_attr (av, s)}) when compare_addr av a = 0 ->
              (TVMap.add (Class (mk_addr_attr a' s (T_py None))) v acc_a, acc_nota)
            | _ ->
              (acc_a, TVMap.add k v acc_nota)
          ) cur (TVMap.empty, TVMap.empty) in
        TVMap.join abasedaddr other in
      debug "ncur = %a" (format TVMap.print) ncur;
      set_env T_cur ncur man flow
      |> Post.return |> OptionExt.return

    | S_expand ({ekind = E_py_annot {ekind = E_addr (a, _)}}, addrs) ->
       let cur = get_env T_cur man flow in
       let cur = TVMap.fold (fun k v cur ->
           match k with
           | Class ({vkind = V_addr_attr (av, s)} as vk) when compare_addr av a = 0 ->
              List.fold_left
                (fun cur eaddr ->
                  match ekind eaddr with
                  | E_addr (addr, _) ->
                     TVMap.add (Class {vk with vkind = V_addr_attr(addr, s)}) v cur
                  | _ -> assert false
                ) cur addrs
           | _ -> cur ) cur cur in
       set_env T_cur cur man flow |> Post.return |> OptionExt.return

    | _ -> None

  let ask _ _ _ = None

  let merge pre (a, e) (a', e') =
    if a == a' then a
    else if is_empty_effect e' then a
    else if is_empty_effect e then a'
    else assert false

  let print_state printer m =
    pprint ~path:[Key "TypeVar annotations"] printer (pbox TVMap.print m)

  let print_expr _ _ _ _ = ()

end

let () =
  register_standard_domain (module Domain)
