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

(* TODO: move S_assume and eval of not into t_bool domain? *)
open Mopsa
open Sig.Domain.Intermediate
open Ast
open Addr
open Data_model.Attribute
open MapExt
open SetExt
open Universal.Ast

type _ query += Q_exn_string_query : expr -> (string * string) query

let () = register_query {
    join = (let f : type r. query_pool -> r query -> r -> r -> r =
              fun next query a b ->
                match query with
                | Q_exn_string_query _ -> (fst a ^ fst b, snd a ^ snd b)
                | _ -> next.join_query query a b in
            f
           );
    meet = (let f : type r. query_pool -> r query -> r -> r -> r =
              fun next query a b ->
                match query with
                | Q_exn_string_query _ -> assert false
                | _ -> next.meet_query query a b in f)
  }


module Domain =
struct

  module AttrSet = Framework.Lattices.Powersetwithunder.Make(
    struct
      type t = string
      let compare = Stdlib.compare
      let print = Format.pp_print_string
    end)

  module AMap = Framework.Lattices.Partial_map.Make
      (struct
        type t = addr
        let compare = compare_addr
        let print = pp_addr
      end)
      (AttrSet)

  include AMap

  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.types.structural_types"
    end)

  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = {provides = [Zone.Z_py_obj]; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any; Universal.Zone.Z_u, Z_any]}
  }

  let alarms = []

  let print fmt d =
    Format.fprintf fmt "attributes: @[%a@]@\n"
      AMap.print d

  let merge pre (a, log) (a', log') =
    if a == a' then a
    else if Log.is_empty log' then a
    else if Log.is_empty log then a'
    else let () = debug "pre=%a@.a=%alog=%a@.a'=%alog'=%a@." print pre print a Log.print log print a' Log.print log' in assert false


  let init progr man flow =
    set_env T_cur empty man flow

  let exec zone stmt man flow =
    let range = stmt.srange in
    match skind stmt with
    | S_assign ({ekind = E_addr ({addr_mode} as la)}, {ekind = E_py_object (a, _)}) ->
      (* si l'adresse est weak et pas dans le store, faire un assign strong *)
      let cur = get_env T_cur man flow in
      if mem a cur then
        let tys = find a cur in
        let cur =
          if addr_mode = STRONG || not (mem la cur) then
            add la tys cur
          else
            let old_tys = find la cur in
            add la (AttrSet.union old_tys tys) cur
        in
        set_env T_cur cur man flow |>
        Post.return |>
        OptionExt.return
      else
        let () = warn_at (srange stmt) "%a => addr %a not in cur.abs_heap, nothing done" pp_stmt stmt pp_addr a in
        Post.return flow |>
        OptionExt.return
    | S_assign ({ekind = E_addr _}, _) ->
      debug "nothing to do@\n";
      Post.return flow |>
      OptionExt.return

    | S_rename ({ekind = E_addr ({addr_kind = A_py_instance _ } as a)}, {ekind = E_addr a'}) ->
      (* | S_rename ({ekind = E_addr ({addr_kind = A_py_var _ } as a)}, {ekind = E_addr a'}) -> *)
      (* TODO: le faire autrepart (addr_env), /!\ zones *)
      let cur = get_env T_cur man flow in
      let old_a = find a cur in
      let to_rename_stmt = mk_block (AttrSet.fold_u (fun attr renames ->
          mk_rename_var
                      (mk_addr_attr a attr T_any)
                      (mk_addr_attr a' attr T_any)
                      range
          :: renames
        ) old_a []) range in
      let ncur =
        let old_va' = find_opt a' cur in
        let new_va' = match old_va' with
          | None -> old_a
          | Some va -> AttrSet.join old_a va in
        remove a cur |>
        add a' new_va' in
      let flow = set_env T_cur ncur man flow in
      man.exec to_rename_stmt flow |>
      Post.return |>
        OptionExt.return

    | S_remove {ekind = E_addr ({addr_kind = A_py_instance _} as a)} ->
       let cur = get_env T_cur man flow in
       let old_a = find a cur in
       let to_remove_stmt = mk_block (AttrSet.fold_u (fun attr removes ->
                                          mk_remove_var (mk_addr_attr a attr T_any) range :: removes) old_a []) range in
       let ncur = remove a cur in
       let flow = set_env T_cur ncur man flow in
       man.exec to_remove_stmt flow |> Post.return |> OptionExt.return

    | S_add ({ekind = E_addr a}) ->
      debug "S_add";
      let cur = get_env T_cur man flow in
      let ncur = add a AttrSet.empty cur in
      debug "S_add ok?";
      set_env T_cur ncur man flow |>
      Post.return |>
      OptionExt.return

    | _ -> None

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_ll_hasattr({ekind = E_py_object (addr, objexpr)} as e, attr) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match akind addr with
        | A_py_module (M_user(name, globals)) ->
          Eval.singleton (mk_py_bool (List.exists (fun v -> get_orig_vname v = attr) globals) range) flow
        | A_py_class (C_builtin _, _)
        | A_py_function (F_builtin _)
        | A_py_module _ ->
          Eval.singleton (mk_py_bool (is_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_class (C_annot c, _) ->
          Eval.singleton (mk_py_bool (
              List.exists (fun v -> get_orig_vname v = attr) c.py_cls_a_static_attributes
              || is_builtin_attribute (object_of_expr e) attr
                                       ) range) flow

        | A_py_function f ->
          Eval.singleton (mk_py_false range) flow

        | A_py_class (C_user c, b) ->
          if (List.exists (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes) then
            Eval.singleton (mk_py_true range) flow
          else
            let cur = get_env T_cur man flow in
            let oaset = AMap.find_opt addr cur in
            begin match oaset with
              | None -> Eval.singleton (mk_py_false range) flow
              | Some aset ->
                if AttrSet.mem_u attr aset then
                  Eval.singleton (mk_py_true range) flow
                else if AttrSet.mem_o attr aset then
                  let cur_t = AMap.add addr (AttrSet.add_u attr aset) cur in
                  let cur_f = AMap.add addr (AttrSet.remove attr aset) cur in
                  let flow_t = set_env T_cur cur_t man flow in
                  let flow_f = set_env T_cur cur_f man flow in
                  Eval.join
                    (Eval.singleton (mk_py_true range) flow_t)
                    (Eval.singleton (mk_py_false range) flow_f)
                else
                  Eval.singleton (mk_py_false range) flow
            end

        | A_py_instance _ ->
          let cur = get_env T_cur man flow in
          let oaset = AMap.find_opt addr cur in
          begin match oaset with
            | None -> Eval.singleton (mk_py_false range) flow
            | Some aset ->
              if AttrSet.mem_u attr aset then
                Eval.singleton (mk_py_true range) flow
              else if AttrSet.mem_o attr aset then
                let cur_t = AMap.add addr (AttrSet.add_u attr aset) cur in
                let cur_f = AMap.add addr (AttrSet.remove attr aset) cur in
                let flow_t = set_env T_cur cur_t man flow in
                let flow_f = set_env T_cur cur_f man flow in
                Eval.join
                  (Eval.singleton (mk_py_true range) flow_t)
                  (Eval.singleton (mk_py_false range) flow_f)
              else
                Eval.singleton (mk_py_false range) flow
          end

        | Objects.Py_list.A_py_list _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Tuple.A_py_tuple _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Py_set.A_py_set _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Dict.A_py_dict _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Py_list.A_py_iterator _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Dict.A_py_dict_view _ ->
          Eval.singleton (mk_py_false range) flow

        | _ ->
          Exceptions.panic_at range "has %a attr %s?@\n" pp_expr e attr
      end
      |> OptionExt.return

    | E_py_ll_getattr({ekind = E_py_object ((addr, objexpr) as obj)} as e, attr) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match akind addr with
        | A_py_module (M_builtin m) ->
          Eval.singleton (mk_py_object (find_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_module (M_user (name, globals)) ->
          let v = List.find (fun x -> get_orig_vname x = attr) globals in
          debug "searching for %s" (name ^ "." ^ attr);
          (* FIXME: is that normal?! used in stub module unittest with builtin unittest.TestCase... *)
          if is_builtin_name (name ^ "." ^ attr) then
            Eval.singleton (mk_py_object (find_builtin_attribute obj attr) range) flow
          (* else if Hashtbl.mem typed_functions attr then
           *   failwith "~ok" *)
          else
            let () = debug "else for var %a" pp_var v in
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var v range) flow

        | A_py_class (C_builtin c, b) ->
          Eval.singleton (mk_py_object (find_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_class (C_annot c, _) ->
          let rec find_annot stmt = match skind stmt with
            | S_block (s, _) -> List.fold_left
                             (fun acc st ->
                                match acc with
                                | None -> find_annot st
                                | Some _ -> acc
                             ) None s
            | S_py_annot({ekind = E_var(v, _)}, annot) when get_orig_vname v = attr -> Some annot
            | _ -> None in
          let obj =
            try
              mk_py_object (find_builtin_attribute (object_of_expr e) attr) range
            with Not_found ->
              match List.find_opt (fun x -> get_orig_vname x = attr) c.py_cls_a_static_attributes with
              | Some f ->
                 begin try Hashtbl.find type_aliases f
                 with Not_found ->
                   let () = debug "body = %a" pp_stmt c.py_cls_a_body in
                   OptionExt.none_to_exn @@ find_annot c.py_cls_a_body
                 end
              | None ->
                 let () = debug "body = %a" pp_stmt c.py_cls_a_body in
                 OptionExt.none_to_exn @@ find_annot c.py_cls_a_body in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) obj  flow


        | A_py_class (C_user c, b) ->
          let f = List.find_opt (fun x -> get_orig_vname x = attr) c.py_cls_static_attributes in
          begin match f with
            | Some f ->
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var f range) flow
            | None ->
              let attr_var = mk_addr_attr addr attr T_any in
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var attr_var range) flow
          end

        | A_py_instance _ ->
          (* there should be a positive hasattr before, so we just evaluate the addr_attr var *)
          let attr_var = mk_addr_attr addr attr T_any in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var attr_var range) flow

        | _ -> Exceptions.panic_at range "ll_getattr: todo %a, attr=%s in@\n%a" pp_addr addr attr (Flow.print man.lattice.print) flow
      end
      |> OptionExt.return

    | E_py_ll_setattr({ekind = E_py_object (alval, objexpr)} as lval, attr, Some rval) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match ekind lval, ekind rval with
      | E_py_object ({addr_kind = A_py_class (C_user c, b)}, _ ), _ when List.exists (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes ->
         let var = List.find (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes in
         let () = debug "using c.py_cls_static_attributes with var = %a" pp_var var in
         man.exec (mk_assign (mk_var var range) rval range) flow
         |> man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_none range)
         |> OptionExt.return
      | E_py_object ({addr_kind = A_py_class (_)}, _ ), E_py_object (arval, _) ->
         Exceptions.panic_at range "Attr assignment on non user-defined classes not supported yet.@\n"
      | E_py_object (alval, _), _ ->
         debug "in here!@\n";
         let cur = get_env T_cur man flow in
         let old_inst =
           try
             AMap.find alval cur
           with Not_found ->
             let () = warn_at range "during setattr over %a, fields not found, put to emptyset by default" pp_expr lval in
             AttrSet.empty
         in
         let cur = AMap.add alval ((if alval.addr_mode = STRONG then AttrSet.add_u else AttrSet.add_o) attr old_inst) cur in
         let flow = set_env T_cur cur man flow in
         (* now we create an attribute var *)
         let attr_var = mk_addr_attr alval attr T_any in
         man.exec ~zone:Zone.Z_py (mk_assign (mk_var attr_var range) rval range) flow
         |> man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_none range)
         |> OptionExt.return

        | _ -> assert false
      end

    | E_py_ll_setattr({ekind = E_py_object (alval, objexpr)}, attr, None) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match akind alval with
        | A_py_class _ ->
          panic_at range "attribute deletion currently unsupported for classes"
        | _ ->
          let cur = get_env T_cur man flow in
          let old_attrset = AMap.find alval cur in
          let flow =
            if AttrSet.mem_u attr old_attrset then
              set_env T_cur (AMap.add alval (AttrSet.remove attr old_attrset) cur) man flow
            else flow in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_none range) flow |>
          OptionExt.return
      end

    | E_py_ll_setattr(e, attr, o) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
      Eval.bind (fun e flow -> man.eval {exp with ekind = E_py_ll_setattr(e, attr, o)} flow) |> OptionExt.return

    | _ ->
      None


  let ask : type r. r query -> ('a, t) man -> 'a flow -> r option =
    fun query man flow ->
      match query with
      | Q_exn_string_query t ->
        let range = erange t in
        let cur = get_env T_cur man flow in
        let iaddr, addr = match ekind t with
          | E_py_object ({addr_kind = A_py_instance a} as ad, _) -> ad, a
          | _ -> assert false in
        let exc, message =
          let name = match akind addr with
            | A_py_class (c, b) ->
              begin match c with
                | C_builtin name | C_unsupported name -> name
                | C_user c -> get_orig_vname ~warn:false c.py_cls_var
                | C_annot c -> get_orig_vname ~warn:false c.py_cls_a_var
              end
            | _ -> assert false
          in
          let message =
            if AttrSet.mem_o "args" (match AMap.find_opt iaddr cur with None -> AttrSet.empty | Some x -> x) then
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var (mk_addr_attr iaddr "args" T_any) range) flow |>
              (* FIXME *)
              Eval.apply (fun etuple flow ->
                  let var = List.hd @@ Objects.Tuple.Domain.var_of_eobj etuple in
                  (* FIXME *)
                  let pset = man.ask (Universal.Strings.Powerset.Q_strings_powerset (mk_var (Utils.change_var_type T_string var) range)) flow in
                  if Universal.Strings.Powerset.Value.is_top pset then "T"
                  else Universal.Strings.Powerset.StringPower.choose pset
                )
                (fun _ _ -> assert false)
                (fun _ _ -> assert false)
                ""
            else
              ""
          in
          name, message in
        let () = debug "answer to query is %s %s@\n" exc message in
        Some (exc, message)

      | Q_print_addr_related_info addr ->
        OptionExt.return @@
        fun fmt ->
        let cur = get_env T_cur man flow in
        let addrs = AMap.filter (fun a _ -> compare_addr a addr = 0) cur in
        if AMap.cardinal addrs <> 0 then
             let () = Format.fprintf fmt "%a@\n" AMap.print addrs in
             AMap.iter
               (fun addr aset ->
                  AttrSet.fold_uo (fun attr () ->
                      Format.fprintf fmt "%a"
                        (man.ask Framework.Engines.Interactive.Q_print_var flow) (mk_addr_attr addr attr T_any).vname
                    ) aset ()
               ) addrs




      | _ -> None


  let refine channel man flow = Channel.return flow

end

let () = Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain)
