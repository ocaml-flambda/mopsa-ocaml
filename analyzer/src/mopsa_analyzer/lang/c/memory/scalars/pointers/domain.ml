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

(** Non-relational abstraction of C pointers *)

open Mopsa
open Sig.Abstraction.Domain
open Universal.Ast
open Ast
open Common.Points_to
open Common.Base
open Common.Alarms
open Static_points_to
open Value



module Domain =
struct



  (** {2 Domain header} *)
  (** ================= *)

  (** Map from variables to set of pointer values *)
  module Map = Framework.Lattices.Partial_inversible_map.Make(Var)(PointerValue)

  type t = Map.t

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.memory.scalars.pointer"
    end)

  let bottom = Map.bottom

  let top = Map.top

  let scalar  = Semantic "C/Scalar"
  let numeric = Semantic "U/Numeric"

  let checks = [ CHK_C_INVALID_POINTER_COMPARE;
                 CHK_C_INVALID_POINTER_SUB ]

  (** {2 Lattice operators} *)
  (** ===================== *)

  let is_bottom = Map.is_bottom

  let subset = Map.subset

  let join = Map.join

  let meet = Map.meet

  let widen ctx = Map.join

  let merge pre (a,e) (a',e') =
    let aa,aa' =
      generic_merge (a,e) (a',e')
        ~add:Map.set ~find:Map.find ~remove:Map.remove
        ~custom:(fun stmt ->
            match skind stmt with
            | S_c_declaration (var,init,scope) ->
              Some Effect.{ modified = VarSet.singleton var; removed = VarSet.empty }

            | S_assign ({ekind = E_c_deref _},_)
            | S_forget ({ekind = E_c_deref _}) ->
              (* We can ignore theses statements because we transform then into statements
                 with variables, and we use the manager to execute them. Therefore, we are
                 sure that the statements with variables are logged in the effects. *)
              Some Effect.{ modified = VarSet.empty; removed = VarSet.empty }

            | _ -> None
          ) in
    Map.meet aa aa'


  let add p v mode a =
    if var_mode p mode = STRONG
    then Map.set p v a

    else
      let old = Map.find p a in
      Map.set p (PointerSet.join v old) a



  (** {2 Initialization} *)
  (** ================== *)

  let init prog man flow =
    set_env T_cur Map.empty man flow



  (** {2 Offset variables} *)
  (** ==================== *)

  type var_kind +=
    | V_c_ptr_offset of var


  let pp_offset fmt p =
    Format.fprintf fmt "offset⦃%a⦄" pp_var p


  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_ptr_offset p -> pp_offset fmt p
          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_ptr_offset p1, V_c_ptr_offset p2 -> compare_var p1 p2
          | _ -> next v1 v2
        );
    }


  (** Create the offset variable of a pointer *)
  let mk_offset_var (p:var) : var =
    let name =
      let () = Format.fprintf Format.str_formatter "offset⦃%s⦄" p.vname in
      Format.flush_str_formatter ()
    in
    mkv name (V_c_ptr_offset p) T_int ~mode:p.vmode ~semantic:"U/Numeric"


  (** Create the offset expression of a pointer *)
  let mk_offset (p:var) mode range : expr =
    mk_var (mk_offset_var p) ~mode range



  (** {2 Utility functions for symbolic evaluations} *)
  (** ============================================== *)

  (** Evaluate a static points-to expression *)
  let eval_static_points_to man flow (p:static_points_to) : (PointerSet.t * expr option * (var * mode option) option) =
    match p with
    | AddrOf (b, o, mode) ->
      PointerSet.base b, Some o, None

    | Eval(q, mode, o) ->
      let v = get_env T_cur man flow |>
              Map.find q
      in
      v, (if PointerSet.is_valid v then Some o else None), (Some (q,mode))

    | Null ->
      PointerSet.null, None, None

    | Invalid ->
      PointerSet.invalid, None, None

    | Top ->
      PointerSet.top, None, None

    | Fun _ ->
      panic ~loc:__LOC__ "symbolic_to_value: function pointers not supported"


  (** Set value of an optional pointer *)
  let set_value_opt p v man flow =
    match p with
    | None -> flow
    | Some (p,mode) -> map_env T_cur (add p v mode) man flow


  (** Create the offset expression from optional pointer *)
  let mk_offset_opt p v o range =
    if not (PointerSet.is_valid v)
    then None
    else
      match p, o with
      | Some (pp,mode), Some oo ->
        Some (mk_binop (mk_offset pp mode range) O_plus oo range ~etyp:T_int)

      | None, Some oo ->
        Some oo

      | _ -> None


  (** Offset conditions for comparing two pointers *)
  let mk_offset_constraint_opt op p1 v1 o1 p2 v2 o2 range =
    mk_offset_opt p1 v1 o1 range |> OptionExt.bind @@ fun e1 ->
    mk_offset_opt p2 v2 o2 range |> OptionExt.bind @@ fun e2 ->
    Some (mk_binop e1 op e2 ~etyp:T_int range)


  (** Remove the offset variable when an abstract pointer value changes *)
  let remove_offset_opt p v v' range man flow =
    match p with
    | None -> Post.return flow
    | Some (p,mode) ->
      if PointerSet.is_valid v && not (PointerSet.is_valid v')
      then man.exec ~route:numeric (mk_remove (mk_offset p mode range) range) flow
      else Post.return flow

  (** Evaluation a pointer expression into a points-to expression *)
  let eval_points_to exp man flow =
    man.eval exp flow >>$? fun exp flow ->
    Static_points_to.eval_opt exp |> OptionExt.lift @@ fun ptr ->
    match ptr with
    | AddrOf (base, offset, mode) ->
      (* XXX for backward compatibility, the offset is converted to Universal, but maybe it should be a C expression? *)
      man.eval offset flow ~translate:"Universal" >>$ fun offset flow ->
      Cases.singleton (mk_c_points_to_bloc base offset mode) flow

    | Eval (p, mode, offset) ->
      let o = mk_offset p mode exp.erange in
      let offset' = mk_binop o O_plus offset ~etyp:T_int exp.erange in
      man.eval offset flow ~translate:"Universal" >>$ fun offset flow ->
      man.eval offset' flow ~translate:"Universal" >>$ fun offset' flow ->
      let a = get_env T_cur man flow in
      let values = Map.find p a in
      let evals = PointerSet.fold_points_to (fun v pt acc ->
          let flow = set_env T_cur (Map.set p v a) man flow in
          (
            ( match PointerSet.is_valid values, PointerSet.is_valid v with
              | false, false -> Post.return flow
              | true, true -> Post.return flow
              | false, true ->
                man.exec ~route:numeric (mk_add o exp.erange) flow >>% fun flow ->
                man.exec ~route:numeric (mk_assign (strongify_var_expr o) offset exp.erange) flow
              | true, false -> man.exec ~route:numeric (mk_remove o exp.erange) flow
            ) >>% fun flow ->
            Cases.singleton pt flow
          ) :: acc
        ) values offset' []
      in
      Cases.join_list evals ~empty:(fun () -> Cases.empty flow)

    | Fun f ->
      Cases.singleton (mk_c_points_to_fun f) flow

    | Null ->
      Cases.singleton mk_c_points_to_null flow

    | Invalid ->
      Cases.singleton mk_c_points_to_invalid flow

    | Top ->
      Cases.singleton mk_c_points_to_top flow


  (** {2 Computation of post-conditions} *)
  (** ================================== *)


  (** Assignment abstract transformer *)
  let assign p q mode range man flow =
    man.eval q flow >>$ fun q flow ->
    let a = get_env T_cur man flow in
    let vnew, onew =
      match Static_points_to.eval q with
      | AddrOf (b, offset, mode') ->
        PointerSet.base b, Some offset

      | Eval (q, mode', offset) ->
        let qo = mk_offset q mode' range in
        let offset' = mk_binop qo O_plus offset ~etyp:T_int range in
        let vq = Map.find q a in
        vq, if PointerSet.is_valid vq then Some offset' else None

      | Fun f ->
        PointerSet.cfun f, None

      | Invalid ->
        PointerSet.invalid, None

      | Null ->
        PointerSet.null, None

      | Top ->
        PointerSet.top, Some (mk_top T_int range)
    in
    let vold = Map.find p a in
    let a' = add p vnew mode a in
    let flow = set_env T_cur a' man flow in
    let vnew = Map.find p a' in
    let o = mk_offset p mode range in
    match PointerSet.is_valid vold, PointerSet.is_valid vnew, onew with
    | false, false, _ -> Post.return flow

    | true, true, Some offset ->
      man.eval offset flow ~translate:"Universal" >>$ fun offset flow ->
      man.exec ~route:numeric (mk_assign o offset range) flow

    | true, true, None -> Post.return flow

    | false, true, Some offset ->
      man.exec ~route:numeric (mk_add o range) flow >>% fun flow ->
      man.eval offset flow ~translate:"Universal" >>$ fun offset flow ->
      man.exec ~route:numeric (mk_assign (strongify_var_expr o) offset range) flow

    | true, false, _ -> man.exec ~route:numeric (mk_remove o range) flow

    | _ -> assert false




  (* Declaration of a scalar pointer variable *)
  let declare_pointer_var v init scope range man flow =
    match scope, init with
    (** Uninitialized global variable *)
    | Variable_global, None | Variable_file_static _, None ->
      (* The variable is initialized with NULL (C99 6.7.8.10) *)
      map_env T_cur (Map.set v PointerSet.null) man flow |>
      Post.return

    (** Uninitialized local variable *)
    | Variable_local _, None | Variable_func_static _, None ->
      (* The value of the variable is undetermined (C99 6.7.8.10) *)
      map_env T_cur (Map.set v PointerSet.invalid) man flow |>
      Post.return

    | _, Some (C_init_expr e) ->
      assign v e None range man flow

    | _ -> assert false



  (** Add a pointer variable to the support of the non-rel map *)
  let add_pointer_var p range man flow =
    let o = mk_offset p None range in
    map_env T_cur (Map.set p PointerSet.top) man flow |>
    man.exec ~route:numeric (mk_add o range)


  (** Remove a pointer variable from the support of the non-rel map *)
  let remove_pointer_var p range man flow =
    let flow = map_env T_cur (Map.remove p) man flow in
    let o = mk_offset p None range in
    man.exec ~route:numeric (mk_remove o range) flow


  (** Rename a pointer variable *)
  let rename_pointer_var p1 p2 range man flow =
    let flow' = map_env T_cur (Map.rename p1 p2) man flow in

    (* Rename the offset if present *)
    let a1 = get_env T_cur man flow |> Map.find p1 in
    if PointerSet.is_valid a1 then
      let o1 = mk_offset p1 None range in
      let o2 = mk_offset p2 None range in
      man.exec ~route:numeric (mk_rename o1 o2 range) flow'
    else
      Post.return flow'


  (** Rename a base *)
  let exec_rename_base e e' range man flow =
    let base = expr_to_base e in
    let base' = expr_to_base e' in
    map_env T_cur (fun a ->
        match Map.find_inverse (PointerValue.Base base) a with
        | TOP -> a
        | Nt pset ->
          Map.add_inverse (PointerValue.Base base') pset a |>
          Map.remove_inverse (PointerValue.Base base)
      ) man flow |>
    Post.return


  (** Expand a base *)
  let exec_expand_base e el range man flow =
    let base = expr_to_base e in
    let basel = List.map expr_to_base el in
    map_env T_cur (fun a ->
        match Map.find_inverse (PointerValue.Base base) a with
        | TOP -> a
        | Nt pset ->
          List.fold_left (fun acc base' ->
              Map.add_inverse (PointerValue.Base base') pset acc
            ) a basel
      ) man flow |>
    Post.return

  (** Fold a set of bases *)
  let exec_fold_bases e el range man flow =
    let base = expr_to_base e in
    let basel = List.map expr_to_base el in
    map_env T_cur (fun a ->
        try
          (* get the set pointers pointing to bases in basel *)
          let pset = List.fold_left (fun acc base' ->
              match Map.find_inverse (PointerValue.Base base') a with
              | TOP -> raise Top.Found_TOP
              | Nt pset -> Map.KeySet.union pset acc
            ) Map.KeySet.empty basel
          in
          (* Make pointers in pset point to base *)
          let a' = Map.add_inverse (PointerValue.Base base) pset a in
          (* Remove bases in basel *)
          List.fold_left (fun acc base' ->
              Map.remove_inverse (PointerValue.Base base') acc
            ) a' basel
        with Top.Found_TOP ->
          a
      ) man flow |>
    Post.return

  (** Invalidate a base *)
  let exec_invalidate_base e range man flow =
    let valid_base, invalid_base = match ekind e with
      | E_var (v,_) -> mk_var_base v, mk_var_base v ~valid:false ~invalidation_range:(Some range)
      | E_addr a -> mk_addr_base a, mk_addr_base a ~valid:false ~invalidation_range:(Some range)
      | _ -> assert false
    in
    let flow = map_env T_cur (fun a ->
        match Map.find_inverse (PointerValue.Base valid_base) a with
        | TOP -> a
        | Nt pset ->
          if base_mode valid_base = STRONG
          then
            Map.remove_inverse (PointerValue.Base valid_base) a |>
            Map.add_inverse (PointerValue.Base invalid_base) pset
          else
            Map.add_inverse (PointerValue.Base invalid_base) pset a
      ) man flow
    in
    Post.return flow



  (** Filter equal pointers *)
  let assume_eq p q range man flow =
    man.eval p flow >>$ fun p flow ->
    man.eval q flow >>$ fun q flow ->
    let v1, o1, p1 = Static_points_to.eval p |>
                     eval_static_points_to man flow
    in
    let v2, o2, p2 = Static_points_to.eval q |>
                     eval_static_points_to man flow
    in
    let v = PointerSet.meet v1 v2 in
    if PointerSet.is_bottom v then
      Flow.set T_cur man.lattice.bottom man.lattice flow |>
      Post.return
    else
      let flow = set_value_opt p1 v man flow |>
                 set_value_opt p2 v man
      in
      remove_offset_opt p1 v1 v range man flow >>% fun flow ->
      remove_offset_opt p2 v2 v range man flow >>% fun flow ->
      match mk_offset_constraint_opt O_eq p1 v1 o1 p2 v2 o2 range with
      | None -> Post.return flow
      | Some cond ->
        man.eval cond flow ~translate:"Universal" >>$ fun cond flow ->
        man.exec ~route:numeric (mk_assume cond range) flow



  (** Filter non-equal pointers *)
  let assume_ne p q range man flow =
    man.eval p flow >>$ fun p flow ->
    man.eval q flow >>$ fun q flow ->
    let v1, o1, p1 = Static_points_to.eval p |>
                     eval_static_points_to man flow
    in
    let v2, o2, p2 = Static_points_to.eval q |>
                     eval_static_points_to man flow
    in
    (* Case 1: p and q point to the same base *)
    let same_base_case =
      let v = PointerSet.meet v1 v2 in
      match PointerSet.is_bottom v,
            mk_offset_opt p1 v o1 range,
            mk_offset_opt p2 v o2 range
      with
      | true, _, _
      | _, None, _
      | _, _, None ->
        []
      | false, Some o1, Some o2 ->
        let flow = set_value_opt p1 v man flow |>
                   set_value_opt p2 v man
        in
        let cond = mk_binop o1 O_ne o2 ~etyp:T_int range in
        [
          man.eval cond flow ~translate:"Universal" >>$ fun cond flow ->
          man.exec ~route:numeric (mk_assume cond range) flow
        ]
    in

    (* Case 2: p and q point to different bases *)
    let different_base_case =
      let vv1 = PointerSet.singleton_diff v1 v2 in
      let vv2 = PointerSet.singleton_diff v2 v1 in
      if PointerSet.is_bottom vv1 || PointerSet.is_bottom vv2 then
        []
      else
        let flow = set_value_opt p1 vv1 man flow |>
                   set_value_opt p2 vv2 man
        in
        [ Post.return flow ]
    in
    let bottom_case = Flow.set T_cur man.lattice.bottom man.lattice flow |>
                      Post.return
    in
    Post.join_list (same_base_case @ different_base_case) ~empty:(fun () -> bottom_case)



  (** Filter ordered pointers *)
  let assume_order op p q range man flow =
    man.eval p flow >>$ fun p flow ->
    man.eval q flow >>$ fun q flow ->
    let v1, o1, p1 = Static_points_to.eval p |>
                     eval_static_points_to man flow
    in
    let v2, o2, p2 = Static_points_to.eval q |>
                     eval_static_points_to man flow
    in
    let v1_valid, v1_invalid = PointerSet.filter_valid v1, PointerSet.filter_non_valid v1 in
    let v2_valid, v2_invalid = PointerSet.filter_valid v2, PointerSet.filter_non_valid v2 in

    (* Case 1: p or q point to invalid bases *)
    let invalid_base_case =
      if PointerSet.is_bottom v1_invalid && PointerSet.is_bottom v2_invalid
      then []
      else
        let flow = raise_c_invalid_pointer_compare p q range man flow in
        [ Post.return flow ]
    in

    (* Case 2: p and q point to the same valid base *)
    let same_base_case =
      let v = PointerSet.meet v1_valid v2_valid in
      if PointerSet.is_bottom v
      then []
      else
        [
          match mk_offset_constraint_opt op p1 v1 o1 p2 v2 o2 range with
          | None -> Post.return flow
          | Some cond ->
            man.eval cond flow ~translate:"Universal" >>$ fun cond flow ->
            man.exec ~route:numeric (mk_assume cond range) flow
        ]
    in

    (* Case 2: p and q point to different valid bases *)
    let different_base_case =
      let vv1 = PointerSet.singleton_diff v1_valid v2_valid in
      let vv2 = PointerSet.singleton_diff v2_valid v1_valid in
      if PointerSet.is_bottom vv1 || PointerSet.is_bottom vv2
      then []
      else
        let flow = set_value_opt p1 vv1 man flow |>
                   set_value_opt p2 vv2 man
        in
        let flow = raise_c_invalid_pointer_compare p q range man flow in
        [ Post.return flow ]
    in
    let bottom_case = Flow.set T_cur man.lattice.bottom man.lattice flow |>
                      Post.return
    in
    Post.join_list (invalid_base_case @ same_base_case @ different_base_case) ~empty:(fun () -> bottom_case)


  (** Expand pointer p and its offset to pointers ql *)
  let expand_pointer_var p ql range man flow =
    (* Expand pointed bases *)
    let a = get_env T_cur man flow in
    let value = Map.find p a in
    let a = List.fold_left (fun acc q ->
        Map.set q value acc
      ) a ql
    in
    let flow = set_env T_cur a man flow in
    (* Expand the offset if present *)
    if PointerSet.is_valid value then
      let o = mk_offset p None range in
      let ol = List.map (fun q -> mk_offset q None range) ql in
      let stmt = mk_expand o ol range in
      man.exec stmt ~route:numeric flow
    else
      Post.return flow


  (** Fold pointers ql in p *)
  let fold_pointer_var p ql range man flow =
    let a = get_env T_cur man flow in
    (* Collect the pointer values of ql before removing them *)
    let value,a' = List.fold_left (fun (accv,acca) q ->
        let accv' = Map.find q a |> PointerSet.join accv in
        let acca' = Map.remove q acca in
        accv',acca'
      ) (PointerSet.bottom,a) ql
    in
    let value' = if Map.mem p a then PointerSet.join value (Map.find p a) else value in
    let flow = set_env T_cur (Map.set p value' a') man flow in
    (* Fold the offset if present *)
    if PointerSet.is_valid value' then
      let o = mk_offset p None range in
      let ol = List.map (fun q -> mk_offset q None range) ql in
      let stmt = mk_fold o ol range in
      man.exec stmt ~route:numeric flow
    else
      Post.return flow


  (** Forget the value of pointer p *)
  let forget_pointer_var p range man flow =
    (* Forget the bases *)
    let a = get_env T_cur man flow in
    let a' = Map.set p PointerSet.top a in
    let flow = set_env T_cur a' man flow in
    (* Forget the offset. If not already present, just add it *)
    let o = mk_offset p None range in
    let stmt =
      if PointerSet.is_valid (Map.find p a) then
        mk_assign o (mk_top T_int range) range
      else
        mk_add o range
    in
    man.exec stmt ~route:numeric flow

  let assign_deref ptr e range man flow =
    let ctype = under_type ptr.etyp in
    eval_points_to ptr man flow |> OptionExt.none_to_exn >>$ fun pt flow ->
    match pt with
    | P_null | P_invalid
    | P_block ({ base_valid = false }, _, _) ->
      Cases.empty flow

    | P_block ({ base_kind = Var v}, offset, mode) when compare_typ ctype v.vtyp = 0 ->
      assume (eq offset zero range)
        ~fthen:(man.exec ~route:scalar (mk_assign (mk_var v ~mode range) e range))
        ~felse:(man.exec ~route:scalar (mk_forget (mk_var v ~mode range) range))
        man flow

    | P_block _ ->
      Post.return flow

    | P_top ->
      let flow =
        Flow.add_local_assumption
          (Soundness.A_ignore_modification_undetermined_pointer ptr)
          range flow
      in
      Post.return flow

    | P_fun _ ->
      assert false


  (** Forget the value of *ptr *)
  let forget_deref ptr range man flow =
    eval_points_to ptr man flow |> OptionExt.none_to_exn >>$ fun pt flow ->
    match pt with
    | P_block ({ base_kind = Var v}, offset, mode) ->
      man.exec ~route:scalar (mk_forget (mk_var v ~mode range) range) flow

    | _ ->
      Post.return flow

  (** Entry point of abstract transformers *)
  let exec stmt man flow =
    match skind stmt with
    | S_c_declaration(p,init,scope)
      when is_c_pointer_type p.vtyp ->
      declare_pointer_var p init scope stmt.srange man flow |>
      OptionExt.return

    | S_assign({ekind = E_var(p, mode)}, q)
      when is_c_pointer_type p.vtyp ->
      assign p q mode stmt.srange man flow |>
      OptionExt.return

    | S_assign({ekind = E_c_deref p} as x, e) when is_c_scalar_type x.etyp ->
      assign_deref p e stmt.srange man flow |>
      OptionExt.return

    | S_add { ekind = E_var (p, _) }
      when is_c_pointer_type p.vtyp ->
      add_pointer_var p stmt.srange man flow |>
      OptionExt.return

    | S_remove ({ ekind = E_var (v,_) } as p)
      when is_c_pointer_type p.etyp ->
      remove_pointer_var v stmt.srange man flow |>
      OptionExt.return


    | S_rename(({ekind = E_var (v1, _)} as p1),
               ({ekind = E_var (v2, _)} as p2))
      when is_c_pointer_type p1.etyp &&
           is_c_pointer_type p2.etyp
      ->
      rename_pointer_var v1 v2 stmt.srange man flow |>
      OptionExt.return

    | S_expand({ekind = E_var (v,_)} as p, ql)
      when is_c_pointer_type p.etyp &&
           List.for_all (function { ekind = E_var _ } as q -> is_c_pointer_type q.etyp | _ -> false) ql
      ->
      let vl = List.map (function { ekind = E_var(q, _) } -> q | _ -> assert false) ql in
      expand_pointer_var v vl stmt.srange man flow |>
      OptionExt.return

    | S_fold({ekind = E_var(v,_)} as p, ql)
      when is_c_pointer_type p.etyp &&
           List.for_all (function { ekind = E_var(q,_) } -> is_c_pointer_type q.vtyp | _ -> false) ql
      ->
      let vl = List.map (function { ekind = E_var(q, _) } -> q | _ -> assert false) ql in
      fold_pointer_var v vl stmt.srange man flow |>
      OptionExt.return

    | S_forget({ekind = E_var(v,_)} as p)
      when is_c_pointer_type p.etyp ->
      forget_pointer_var v stmt.srange man flow |>
      OptionExt.return

    | S_forget ({ekind = E_c_deref p} as x) when is_c_scalar_type x.etyp ->
      forget_deref p stmt.srange man flow |>
      OptionExt.return

    | S_invalidate e when is_c_type e.etyp  ->
      exec_invalidate_base e stmt.srange man flow |>
      OptionExt.return

    | S_rename(e,e') when is_c_block_object_type e.etyp ->
      exec_rename_base (of_c_block_object e) (of_c_block_object e') stmt.srange man flow |>
      OptionExt.return

    | S_expand(e,el) when is_c_block_object_type  e.etyp ->
      exec_expand_base (of_c_block_object e) (List.map of_c_block_object el) stmt.srange man flow |>
      OptionExt.return

    | S_fold(e,el) when is_c_block_object_type e.etyp ->
      exec_fold_bases (of_c_block_object e) (List.map of_c_block_object el) stmt.srange man flow |>
      OptionExt.return

    | S_assume(p) when is_c_pointer_type p.etyp ->
      assume_ne p (mk_c_null stmt.srange) stmt.srange man flow |>
      OptionExt.return

    | _ -> None



  (** {2 Pointer evaluation} *)
  (** ====================== *)


  (** 𝔼⟦ p - q ⟧ *)
  let eval_diff p q range man flow =
    man.eval p flow >>$ fun p flow ->
    man.eval q flow >>$ fun q flow ->
    (* p1 and p2 should point to the same type *)
    let elem_size_p = under_type p.etyp |> void_to_char |> sizeof_type in
    let elem_size_q = under_type q.etyp |> void_to_char |> sizeof_type in
    (* FIXME: do we need to check the sign also? *)
    if not @@ Z.equal elem_size_p elem_size_q
    then panic_at range
        "%a - %a: pointers do not point to the same type"
        pp_expr p pp_expr q
    ;

    (* Evaluate the pointed bases symbolically *)
    let v1, o1, p1 = Static_points_to.eval p |>
                     eval_static_points_to man flow
    in
    let v2, o2, p2 = Static_points_to.eval q |>
                     eval_static_points_to man flow
    in

    (* Size of a pointed element *)
    let elem_size = elem_size_p in

    (* Case 1 : same base => return difference of offset *)
    let case1 =
      let v = PointerSet.meet v1 v2 in
      if PointerSet.is_bottom v
      then []
      else
        let flow = set_value_opt p1 v man flow |>
                   set_value_opt p2 v man
        in
        let ee =
          mk_offset_opt p1 v o1 range |> OptionExt.bind @@ fun o1 ->
          mk_offset_opt p2 v o2 range |> OptionExt.bind @@ fun o2 ->
          let e = sub o1 o2 range in
          if Z.equal elem_size Z.one
          then Some e
          else Some (div e (mk_z elem_size range) range)
        in
        match ee with
        | None -> [man.eval (mk_top T_int range) flow] (* FIXME: why not return 0? *)
        | Some e -> [man.eval e flow ~translate:"Universal"]
    in

    (* Case 2: different base => undefined behavior *)
    let case2 =
      let v1 = PointerSet.diff v1 v2 in
      let v2 = PointerSet.diff v2 v1 in
      if PointerSet.is_bottom v1 || PointerSet.is_bottom v2
      then []
      else
        let flow = set_value_opt p1 v1 man flow |>
                   set_value_opt p2 v2 man
        in
        let flow = raise_c_invalid_pointer_sub p q range man flow in
        [Eval.empty flow]
    in

    Eval.join_list (case1 @ case2) ~empty:(fun () -> Eval.empty flow)


  (** 𝔼⟦ *ptr ⟧ *)
  let eval_deref ptr range man flow =
    let ctype = under_type ptr.etyp in
    eval_points_to ptr man flow |> OptionExt.none_to_exn >>$ fun pt flow ->
    match pt with
    | P_null | P_invalid
    | P_block ({ base_valid = false }, _, _) ->
      Eval.empty flow

    | P_block ({ base_kind = Var v; base_valid = true}, offset, mode) when compare_typ ctype v.vtyp = 0 ->
      assume (eq offset zero range) ~route:scalar
        ~fthen:(man.eval ~route:scalar (mk_var v ~mode range))
        ~felse:(man.eval ~route:scalar (mk_top ctype range))
        man flow

    | P_block _ ->
      man.eval ~route:scalar (mk_top ctype range) flow

    | P_top ->
      man.eval ~route:scalar (mk_top ctype range) flow

    | P_fun _ ->
      assert false




  (** Entry point of abstraction evaluations *)
  let eval exp man flow =
    match ekind exp with
    (* 𝔼⟦ (t)p - (t)q | t is a numeric type ⟧ *)
    | E_binop(O_minus, { ekind = E_c_cast(p, _); etyp = t1 }, { ekind = E_c_cast(q, _); etyp = t2 })
      when is_c_pointer_type p.etyp &&
           is_c_pointer_type q.etyp &&
           is_c_int_type t1 &&
           compare_typ t1 t2 = 0
      ->
      (* (t)p - (t) q is transformed into (t)(p - q) * |t0|,
         where |t0| is the size the type pointed by p
      *)
      let diff = mk_c_cast (sub p q ~typ:s32 exp.erange) t1 exp.erange in
      let exp' =
        match under_type p.etyp |> remove_typedef_qual with
        | T_c_void -> diff
        | tt -> mul (mk_z (sizeof_type tt) ~typ:t1 exp.erange) diff ~typ:t1 exp.erange
      in
      man.eval exp' flow |>
      OptionExt.return

    (* 𝔼⟦ p1 - p2 ⟧ *)
    | E_binop(O_minus, p1, p2)
      when is_c_pointer_type p1.etyp &&
           is_c_pointer_type p2.etyp
      ->
      eval_diff p1 p2 exp.erange man flow |>
      OptionExt.return

    | E_c_address_of lval ->
      begin match ekind @@ remove_casts lval with
        | E_var _ ->
          Eval.singleton exp flow |>
          OptionExt.return

        | E_c_deref p ->
          man.eval p flow |>
          OptionExt.return

        | _ -> None
      end

      (* 𝔼⟦ p == q ⟧ *)
    | E_binop(O_eq, p, q)
    | E_unop(O_log_not, {ekind = E_binop(O_ne, p, q)})
      when is_c_pointer_type p.etyp ||
           is_c_pointer_type q.etyp
      ->
      let evl1 = assume_eq p q exp.erange man flow >>$ fun () flow -> Eval.singleton (mk_true exp.erange) flow in
      let evl2 = assume_ne p q exp.erange man flow >>$ fun () flow -> Eval.singleton (mk_false exp.erange) flow in
      Eval.join evl1 evl2 |>
      OptionExt.return

    (* 𝔼⟦ ?(p != q) ⟧ *)
    | E_binop(O_ne, p, q)
    | E_unop(O_log_not, {ekind = E_binop(O_eq, p, q)})
      when is_c_pointer_type p.etyp ||
           is_c_pointer_type q.etyp
      ->
      let evl1 = assume_ne p q exp.erange man flow >>$ fun () flow -> Eval.singleton (mk_true exp.erange) flow in
      let evl2 = assume_eq p q exp.erange man flow >>$ fun () flow -> Eval.singleton (mk_false exp.erange) flow in
      Eval.join evl1 evl2 |>
      OptionExt.return


    (* 𝔼⟦ p op q | op ∈ {<, <=, >, >=} ⟧ *)
    | E_binop((O_lt | O_le | O_gt | O_ge) as op, p, q)
      when is_c_pointer_type p.etyp &&
           is_c_pointer_type q.etyp
      ->
      let evl1 = assume_order op p q exp.erange man flow >>$ fun () flow -> Eval.singleton (mk_true exp.erange) flow in
      let evl2 = assume_order (negate_comparison_op op) p q exp.erange man flow >>$ fun () flow -> Eval.singleton (mk_false exp.erange) flow in
      Eval.join evl1 evl2 |>
      OptionExt.return

    (* 𝔼⟦ !(p op q) | op ∈ {<, <=, >, >=} ⟧ *)
    | E_unop (O_log_not, { ekind = E_binop((O_lt | O_le | O_gt | O_ge) as op, p, q) })
      when is_c_pointer_type p.etyp &&
           is_c_pointer_type q.etyp
      ->
      let evl1 = assume_order (negate_comparison_op op) p q exp.erange man flow >>$ fun () flow -> Eval.singleton (mk_true exp.erange) flow in
      let evl2 = assume_order op p q exp.erange man flow >>$ fun () flow -> Eval.singleton (mk_false exp.erange) flow in
      Eval.join evl1 evl2 |>
      OptionExt.return


    (* 𝔼⟦ (bool)ptr ⟧ *)
    | E_c_cast(p, _) when exp |> etyp |> is_c_int_type &&
                          p   |> etyp |> is_c_pointer_type ->
      eval_points_to p man flow |> OptionExt.lift (fun evl ->
          evl >>$ fun pt flow ->
          match pt with
          | P_null ->
            Eval.singleton (mk_false exp.erange) flow
          | P_top | P_invalid ->
            Eval.singleton (mk_top T_bool exp.erange) flow
          | P_block _ | P_fun _ ->
            Eval.singleton (mk_true exp.erange) flow
        )

    (* 𝔼⟦ (int)ptr ⟧ *)
    | E_c_cast(p, _) when exp |> etyp |> is_c_int_type &&
                          p   |> etyp |> is_c_pointer_type ->
      eval_points_to p man flow |> OptionExt.lift (fun evl ->
          evl >>$ fun pt flow ->
          match pt with
          | P_null ->
            Eval.singleton (mk_zero exp.erange) flow
          | P_top | P_invalid ->
            let l,u = rangeof exp.etyp in
            Eval.singleton (mk_z_interval l u exp.erange) flow
          | P_block _ | P_fun _ ->
            let l,u = rangeof exp.etyp in
            if is_c_signed_int_type exp.etyp then
              Eval.join
                (Eval.singleton (mk_z_interval l Z.(of_int (-1)) exp.erange) flow)
                (Eval.singleton (mk_z_interval Z.one u exp.erange) flow)
            else
              Eval.singleton (mk_z_interval l u exp.erange) flow
        )

    (* 𝔼⟦ !ptr ⟧ *)
    | E_unop(O_log_not, p) when p |> etyp |> is_c_pointer_type ->
      eval_points_to p man flow |> OptionExt.lift (fun evl ->
          evl >>$ fun pt flow ->
          match pt with
          | P_null ->
            Eval.singleton (mk_one exp.erange) flow
          | P_top | P_invalid ->
            Eval.singleton (mk_int_interval 0 1 exp.erange) flow
          | P_block _ | P_fun _ ->
            Eval.singleton (mk_zero exp.erange) flow
        )

    (* 𝔼⟦ *p ⟧ *)
    | E_c_deref p when is_c_scalar_type exp.etyp ->
      eval_deref p exp.erange man flow |>
      OptionExt.return


    | _ -> None


  (** {2 Handler of queries} *)
  (** ====================== *)

  let ask : type a r. (a,r) query -> (a,t) man -> a flow -> r option = fun query man flow ->
    match query with
    | Q_c_points_to e -> eval_points_to e man flow
    | _ -> None


  (** {2 Pretty printer} *)
  (** ****************** *)
  
  let print_state printer a =
    pprint ~path:[Key "pointers"] printer (pbox Map.print a)

  let print_expr man flow printer exp =
    match ekind (remove_casts exp) with
    | E_var (var,_) when is_c_pointer_type var.vtyp
                      && not (is_c_array_type var.vtyp) ->
      let a = get_env T_cur man flow in
      let v = Map.find var a in
      pprint printer ~path:[ Key "pointers";
                             fkey "%a" pp_var var ]
        (pbox PointerSet.print v)
      ;
      if PointerSet.is_valid v then
        let o = mk_offset var None exp.erange in
        man.print_expr flow printer o ~route:numeric

    | _ -> ()

end

let () =
  register_standard_domain (module Domain)
