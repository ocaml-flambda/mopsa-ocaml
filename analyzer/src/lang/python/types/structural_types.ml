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

let pyvarcounter = ref (-1)

let get_fresh_a_py_var () =
  incr pyvarcounter;
  !pyvarcounter

let greek_of_int =
  let list = ["α"; "β"; "γ"; "δ"; "ε"; "ζ"; "η"; "θ"; "ι"; "κ"; "λ"; "μ"; "ν"; "ξ"; "ο"; "π"; "ρ"; "σ"; "τ"; "υ"; "φ"; "χ"; "ψ"; "ω"] in
  let listn = List.length list in
  fun n ->
    let letter = List.nth list (n mod listn) in
    if n < listn then letter
    else Format.sprintf "%s(%d)" letter (n / listn)

type addr_kind +=
    A_py_var of int

let () =
  Format.(
    register_addr_kind {
      print =
        (fun default fmt a -> match a with
           | A_py_var a -> Format.fprintf fmt "%s" (greek_of_int a)
           | _ -> default fmt a
        );
      compare =
        (fun default a1 a2 ->
           match a1, a2 with
           | A_py_var v1, A_py_var v2 -> Pervasives.compare v1 v2
           | _ -> default a1 a2);
    }
  )

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

  (* module Polytypeset =
   * struct
   *   include Framework.Lattices.Powerset.Make(struct
   *       type t = polytype
   *       let compare = compare_polytype
   *       let print = pp_polytype
   *     end)
   *
   *
   *   exception JoinError
   *
   *   let join t1 t2 =
   *     (\* joins I[A, a, empty] and I[A, empty, a] into the last one *\)
   *     let proceed i1 s2 =
   *       let s, r = Set.fold (fun e2 (sacc, ok) ->
   *           match e2 with
   *           | Instance i2 when compare_polytype i1.classn i2.classn = 0 &&
   *                              (StringMap.compare compare_addr i1.uattrs i2.uattrs <> 0 ||
   *                              StringMap.compare compare_addr i1.oattrs i2.oattrs <> 0) ->
   *             begin try
   *               let ru, o1, o2 = StringMap.fold2o
   *                   (fun k v1 (accr, acc1o, acc2o) -> (accr, StringMap.add k v1 acc1o, acc2o))
   *                   (fun k v2 (accr, acc1o, acc2o) -> (accr, acc1o, StringMap.add k v2 acc2o))
   *                   (fun k v1 v2 (accr, acc1o, acc2o) ->
   *                      if compare_addr v1 v2 = 0 then
   *                        (StringMap.add k v1 accr, acc1o, acc2o)
   *                      else
   *                        raise JoinError
   *                   ) i1.uattrs i2.uattrs (StringMap.empty, i1.oattrs, i2.oattrs) in
   *               let ro = StringMap.fold2o
   *                   StringMap.add
   *                   StringMap.add
   *                   (fun k v1 v2 acc ->
   *                      if compare_addr v1 v2 = 0 then
   *                        StringMap.add k v1 acc
   *                      else
   *                        raise JoinError
   *                   )
   *                   o1 o2 StringMap.empty
   *               in
   *               let ri = {classn = i1.classn; uattrs = ru; oattrs = ro} in
   *               (\* Format.printf "join @[@\n%a@\n%a@\n%a@]@\n@\n" pp_polytype (Instance i1) pp_polytype (Instance i2) pp_polytype (Instance ri); *\)
   *               Set.add (Instance ri) sacc, true
   *             with JoinError ->
   *               Set.add (Instance i2) sacc, ok
   *           end
   *           | _ -> Set.add e2 sacc, ok
   *         ) s2 (Set.empty, false) in
   *       if r then s else Set.add (Instance i1) s in
   *     Top.top_lift2 (fun s1 s2 ->
   *         if cardinal t1 = 1 then
   *           match choose t1 with
   *           | Instance i1 ->proceed i1 s2
   *           | _ -> Set.union s1 s2
   *         else if cardinal t2 = 1 then
   *           match choose t2 with
   *           | Instance i2 -> proceed i2 s1
   *           | _ -> Set.union s1 s2
   *         else
   *           Set.union s1 s2
   *       ) t1 t2
   *
   *   (\* let widen _ = join *\)
   * end *)

  module AttrSet = Framework.Lattices.Powersetwithunder.Make(
    struct
      type t = string
      let compare = Pervasives.compare
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

  let widen ctx = widen

  include Framework.Core.Id.GenDomainId(struct
      type nonrec t = t
      let name = "python.types.structural_types"
    end)

  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = {provides = [Zone.Z_py_obj]; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any; Universal.Zone.Z_u, Z_any]}
  }

  let print fmt d =
    Format.fprintf fmt "attributes: @[%a@]@\n"
      AMap.print d

  (* let subset d d' =
   * (* not supporting correctly top and bottom *)
   *   debug "subset %a %a..." print d print d';
   *   let res = TMap.fold (fun absaddr ptys acc ->
   *       if TMap.mem absaddr d' then
   *         let ptys' = TMap.find absaddr d' in
   *         debug "absaddr = %a, ptys' = %a@\n" pp_addr absaddr Polytypeset.print ptys';
   *         (\* acc && polytype_leq (pty, d.typevar_env) (pty', d'.typevar_env) *\)
   *         acc && (Polytypeset.is_top ptys' || Polytypeset.for_all (fun pty -> Polytypeset.exists (fun pty' -> polytype_leq pty pty') ptys') ptys)
   *       else false
   *     )
   *       d true
   *   in
   *   debug "= %b@\n" res;
   *   res *)


  let merge _ _ _ = assert false

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
        Option.return
      else
        let () = warn_at (srange stmt) "%a => addr %a not in cur.abs_heap, nothing done" pp_stmt stmt pp_addr a in
        Post.return flow |>
        Option.return
    | S_assign ({ekind = E_addr _}, _) ->
      debug "nothing to do@\n";
      Post.return flow |>
      Option.return

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
      Option.return

    | S_add ({ekind = E_addr a}) ->
      let cur = get_env T_cur man flow in
      let ncur = add a AttrSet.empty cur in
      set_env T_cur ncur man flow |>
      Post.return |>
      Option.return

    | S_assign({ekind = E_py_attribute(lval, attr)}, rval) ->
      begin match ekind lval, ekind rval with
        | E_py_object ({addr_kind = A_py_class (C_user c, b)} as alval, _ ), E_py_object (arval, _) when alval.addr_mode = STRONG ->
          if List.exists (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes then
            let var = List.find (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes in
            man.exec (mk_assign (mk_var var range) rval range) flow
            |> Post.return |> Option.return
          else
            Exceptions.panic_at range "Adding an attribute to a *class* is not supported yet"
        (* todo: enelver l'ancien c.py_cls_dec et ajouter le nouveau ave cla bonne variable, avant de faire la même assignation *)
        | E_py_object ({addr_kind = A_py_class (_)}, _ ), E_py_object (arval, _) ->
          Exceptions.panic_at range "Attr assignment on non user-defined classes not supported yet.@\n"
        | E_py_object (alval, _), _ ->
          debug "in here!@\n";
          let cur = get_env T_cur man flow in
          let old_inst = AMap.find alval cur in
          let cur = AMap.add alval ((if alval.addr_mode = STRONG then AttrSet.add_u else AttrSet.add_o) attr old_inst) cur in
          let flow = set_env T_cur cur man flow in
          (* now we create an attribute var *)
          let attr_var = mk_addr_attr alval attr T_any in
          (* FIXME: zone *)
          man.exec (mk_assign (mk_var attr_var range) rval range) flow
          |> Post.return |> Option.return

        | _ -> assert false
      end

    | _ -> None

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    (* | E_py_annot e ->
     *   begin match ekind e with
     *     | E_var (v, mode) when is_builtin_name @@ get_orig_vname v ->
     *       let name = get_orig_vname v in
     *       begin match name with
     *       | "int" ->
     *         process_constant man flow range name addr_integers
     *       | "float" ->
     *         process_constant man flow range name addr_float
     *       | "NotImplementedType" ->
     *         process_constant man flow range name addr_notimplemented
     *       | "NoneType" ->
     *         process_constant man flow range name addr_none
     *       | _ ->
     *         allocate_builtin ~mode:WEAK man range flow (get_orig_vname v) (Some e)
     *       end
     *       |> Option.return
     *
     *     | E_var (v, mode) ->
     *       debug "E_annot %s" v.vname;
     *       begin try
     *           let e = Hashtbl.find type_aliases v in
     *           debug "found type alias, replacing %a by %a" pp_var v pp_expr e;
     *           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_annot e) range) flow |> Option.return
     *         with Not_found ->
     *           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call e [] range) flow |> Option.return
     *       end
     *
     *     | E_py_attribute ({ekind = E_var (v, _)}, s) ->
     *       debug "searching %a in the type aliases..." pp_expr e;
     *       begin
     *         try
     *           (\* FIXME ouch, not found in man.eval would also get caught... *\)
     *           (\* FIXME: this also means that if a.pyi defines alias b and b.pyi too, we'll encounter some trouble *\)
     *           let r = find_type_alias_by_name s in
     *           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_annot r) range) flow |> Option.return
     *         with Not_found ->
     *           debug "not found, trying usual evaluation";
     *           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |> Option.return
     *       end
     *
     *     | E_py_index_subscript ({ekind = E_py_object _} as e1, e2) ->
     *       warn_at range "E_py_annot subscript e1=%a e2=%a now in the wild" pp_expr e1 pp_expr e2;
     *       None
     *
     *     | E_py_index_subscript (e1, e2) ->
     *       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e1 flow |>
     *       bind_some (fun e1 flow ->
     *           warn_at range "trasnlated to e1=%a e2=%a" pp_expr e1 pp_expr e2;
     *           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_annot {e with ekind = E_py_index_subscript(e1, e2)}} flow
     *         )
     *       |> Option.return
     *
     *
     *
     *     | _ ->
     *       Exceptions.panic_at range "Unsupported type annotation %a@\n" pp_expr e
     *   end *)



    (* Je pense pas avoir besoin de ça finalement *)
    (* | E_py_object ({addr_kind = A_py_class (c, b)} as addr, expr) ->
     *   let cur = get_env T_cur man flow in
     *   let abs_heap = TMap.add addr (Polytypeset.singleton (Class (c, b))) cur.abs_heap in
     *   let flow = set_env T_cur {cur with abs_heap} man flow in
     *   Eval.singleton (mk_addr addr range) flow |> Option.return *)

    (* begin match akind with
     * | A_py_method (func, self) ->
     *    man.eval (mk_py_object ({addr_kind = akind; addr_uid = (-1); addr_mode=STRONG}, mk_py_empty range) range) flow
     * | _ ->
     *    let addr = {addr_kind = akind; addr_uid=(-1);addr_mode=STRONG} in
     *    Eval.singleton (mk_addr addr range) flow
     * end
     * |> Option.return *)

    (* | E_unop(O_log_not, {ekind=E_constant (C_bool b)}) ->
     *   Eval.singleton (mk_py_bool (not b) range) flow
     *   |> Option.return *)


    | E_py_ll_hasattr({ekind = E_py_object (addr, objexpr)} as e, attr) ->
      let attr = match ekind attr with
        | E_constant (C_string s) -> s
        | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
        | _ -> assert false in
      begin match akind addr with
        | A_py_module (M_user(name, globals)) ->
          Eval.singleton (mk_py_bool (List.exists (fun v -> get_orig_vname v = attr) globals) range) flow
        | A_py_class (C_builtin _, _)
        | A_py_module _ ->
          Eval.singleton (mk_py_bool (is_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_class (C_user c, b) ->
          Eval.singleton (mk_py_bool (List.exists (fun v -> get_orig_vname v = attr) c.py_cls_static_attributes) range) flow

        (* | A_py_var _ *)
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

        | Objects.Py_set.A_py_set _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Dict.A_py_dict _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Py_list.A_py_iterator _ ->
          Eval.singleton (mk_py_false range) flow

        | Objects.Dict.A_py_dict_view _ ->
          Eval.singleton (mk_py_false range) flow

        | _ ->
          Exceptions.panic_at range "%a@\n" pp_expr e
      end
      |> Option.return

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
          (* FIXME: is that normal?! used in stub module unittest with builtin unittest.TestCase... *)
          if is_builtin_name (name ^ "." ^ attr) then
            let () = warn_at range "using builtin rather than variable when performing %a" pp_expr exp in
            Eval.singleton (mk_py_object (find_builtin_attribute obj attr) range) flow
          else
            man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var v range) flow

        | A_py_class (C_builtin c, b) ->
          Eval.singleton (mk_py_object (find_builtin_attribute (object_of_expr e) attr) range) flow

        | A_py_class (C_user c, b) ->
          let f = List.find (fun x -> get_orig_vname x = attr) c.py_cls_static_attributes in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var f range) flow

        (* | A_py_var _ *)
        | A_py_instance _ ->
          (* there should be a positive hasattr before, so we just evaluate the addr_attr var *)
          let attr_var = mk_addr_attr addr attr T_any in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var attr_var range) flow

        | _ -> Exceptions.panic_at range "ll_getattr: todo %a, attr=%s in@\n%a" pp_addr addr attr (Flow.print man.lattice.print) flow
      end
      |> Option.return
    (* FIXME: rename over addr should rename attr vars *)


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "abs")}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          if List.length eargs <> 1 then
            let () = Format.fprintf Format.str_formatter "abs() takes exactly one argument (%d given)" (List.length args) in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |> Eval.empty_singleton
          else
            let v = List.hd eargs in
            assume (mk_py_isinstance_builtin v "int" range) man flow
              ~fthen:(man.eval (mk_py_top T_int range))
              ~felse:(fun flow ->
                assume (mk_py_isinstance_builtin v "float" range) man flow
                  ~fthen:(man.eval (mk_py_top (T_float F_DOUBLE) range))
                  ~felse:(fun flow ->
                      Format.fprintf Format.str_formatter "bad operand type for abs()";  (* FIXME *)
                      man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow |>
                      Eval.empty_singleton
                    )
              )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("all" as f))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["list"] (fun _ -> man.eval (mk_py_top T_bool range))
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "divmod")}, _)}, args, []) ->
      (* FIXME: error messages etc *)
      let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          if List.length args <> 2 then tyerror flow else
            let argl, argr = match eargs with l::r::[] -> l, r | _ -> assert false in
            assume (mk_py_isinstance_builtin argl "int" range) man flow
              ~fthen:(fun flow ->
                  assume (mk_py_isinstance_builtin argr "int" range) man flow
                    ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top T_int range; mk_py_top T_int range]) range))
                    ~felse:(fun flow ->
                        assume (mk_py_isinstance_builtin argr "float" range) man flow
                          ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                          ~felse:tyerror
                      )
              )
              ~felse:(fun flow ->
                  assume (mk_py_isinstance_builtin argl "float" range) man flow
                    ~fthen:(fun flow ->
                        assume (mk_py_isinstance_builtin argr "int" range) man flow
                          ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                          ~felse:(fun flow ->
                              assume (mk_py_isinstance_builtin argr "float" range) man flow
                                ~fthen:(man.eval (mk_expr (E_py_tuple [mk_py_top (T_float F_DOUBLE) range; mk_py_top (T_float F_DOUBLE) range]) range))
                                ~felse:tyerror
                            )

                      )
                    ~felse:tyerror
                )
        )
      |> Option.return


    | E_py_undefined _ -> Eval.singleton exp flow |> Option.return

    | E_py_object _ -> Eval.singleton exp flow |> Option.return

    | _ ->
      None


  let ask : type r. r query -> ('a, t) man -> 'a flow -> r option =
    fun query man flow ->
      match query with
      | Q_exn_string_query t ->
        let range = erange t in
        let cur = get_env T_cur man flow in
        let addr = match ekind t with
          | E_py_object ({addr_kind = A_py_instance a}, _) -> a
          | _ -> assert false in
        let exc, message =
          let name = match akind addr with
            | A_py_class (c, b) ->
              begin match c with
                | C_builtin name | C_unsupported name -> name
                | C_user c -> get_orig_vname c.py_cls_var
              end
            | _ -> assert false
          in
          let message =
            if AttrSet.mem_o "args" (match AMap.find_opt addr cur with None -> AttrSet.empty | Some x -> x) then
              (* FIXME *)
              let res = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object (find_builtin "tuple.__getitem__") range) [mk_var (mk_addr_attr addr "args" T_any) range] range) flow in
              Eval.apply (fun expr flow ->
                  match ekind expr with
                  | E_py_object (_, Some {ekind = E_constant (C_string s)}) -> s
                  | _ -> assert false
                )
                (fun x y -> x)
                (fun x y -> x)
                "" res
            else
              ""
          in
          name, message in
        let () = debug "answer to query is %s %s@\n" exc message in
        Some (exc, message)

      | _ -> None


  let refine channel man flow = Channel.return flow

end

let () = Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain)
