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
open Framework.Core.Sig.Stacked.Intermediate
open Universal.Ast
open Ast
open Zone
open Universal.Zone
open Common.Points_to
open Common.Base
open Alarms
open Value
open Static_points_to

module Domain =
struct



  (** {2 Domain header} *)
  (** ================= *)

  (** Map from variables to set of pointer values *)
  module Map = Framework.Lattices.Partial_map.Make(Var)(PointerSet)

  type t = Map.t

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.memory.scalars.pointer"
    end)

  let bottom = Map.bottom

  let top = Map.top

  let print fmt a =
    Format.fprintf fmt "pointers: %a@\n" Map.print a

  let interface = {
    iexec = {
      provides = [Z_c_scalar];
      uses = [Universal.Zone.Z_u_num];
    };


    ieval = {
      provides = [
        Z_c_scalar, Z_c_points_to;
        Z_c_scalar, Universal.Zone.Z_u_num
      ];
      uses = [
        Z_c_scalar, Universal.Zone.Z_u_num
      ]
    }
  }


  (** {2 Lattice operators} *)
  (** ===================== *)

  let is_bottom a = Map.is_bottom a

  let subset man ctx (a,s) (a',s') =
    Map.subset a a', s, s'

  let join man ctx (a,s) (a',s') =
    Map.join a a', s, s'

  let meet man ctx (a,s) (a',s') =
    Map.meet a a', s, s'

  let widen man ctx (a,s) (a',s') =
    Map.join a a', s, s', true

  let merge pre (a,log) (a',log') =
    let block = Log.get_domain_block log in
    let block' = Log.get_domain_block log' in

    let patch_stmt stmt a acc =
      match skind stmt with
      | S_c_declaration (var,init,scope) ->
        let v = Map.find var a in
        Some (Map.add var v acc)

      | S_rename ( {ekind = E_addr addr1}, {ekind = E_addr addr2} ) ->
        Some acc

      | _ -> None
    in

    let patch_block block a acc =
      List.fold_left (fun (acc,block') stmt ->
          match patch_stmt stmt a acc with
          | None -> acc, stmt :: block'
          | Some acc' -> acc', block'
        ) (acc,[]) block
    in

    let a', block = patch_block block a a' in
    let a, block' = patch_block block' a' a in

    Framework.Transformers.Value.Nonrel.generic_nonrel_merge pre (a,block) (a',block')
      ~top:PointerSet.top ~add:Map.add ~find:Map.find ~remove:Map.remove ~meet:Map.meet


  let add p v mode a =
    if mode = STRONG
    then Map.add p v a

    else
      let old = Map.find p a in
      Map.add p (Value.join v old) a



  (** {2 Initialization} *)
  (** ================== *)

  let init prog man flow =
    set_env T_cur Map.empty man flow



  (** {2 Offset variables} *)
  (** ==================== *)

  type var_kind +=
    | V_c_ptr_offset of var


  let pp_offset fmt p =
    Format.fprintf fmt "offset(%a)" pp_var p


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
      let () = Format.fprintf Format.str_formatter "offset(%s)" p.vname in
      Format.flush_str_formatter ()
    in
    mkv name (V_c_ptr_offset p) T_int


  (** Create the offset expression of a pointer *)
  let mk_offset (p:var) mode range : expr =
    mk_var (mk_offset_var p) ~mode range



  (** {2 Utility functions for symbolic evaluations} *)
  (** ============================================== *)

  (** Evaluate a static points-to expression *)
  let eval_static_points_to man flow (p:static_points_to) : (Value.t * expr option * (var * mode) option) =
    match p with
    | AddrOf (b, o) ->
      Value.base b, Some o, None

    | Eval(q, mode, o) ->
      let v = get_env T_cur man flow |>
              Map.find q
      in
      v, (if Value.is_valid v then Some o else None), (Some (q,mode))

    | Null ->
      Value.null, None, None

    | Invalid ->
      Value.invalid, None, None

    | Top ->
      Value.top, None, None

    | Fun _ ->
      panic ~loc:__LOC__ "symbolic_to_value: function pointers not supported"


  (** Set value of an optional pointer *)
  let set_value_opt p v man flow =
    match p with
    | None -> flow
    | Some (p,mode) -> map_env T_cur (add p v mode) man flow


  (** Create the offset expression from optional pointer *)
  let mk_offset_opt p v o range =
    if not (Value.is_valid v)
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
    mk_offset_opt p1 v1 o1 range |> Option.bind @@ fun e1 ->
    mk_offset_opt p2 v2 o2 range |> Option.bind @@ fun e2 ->
    Some (mk_binop e1 op e2 ~etyp:T_int range)


  (** Remove the offset variable when an abstract pointer value changes *)
  let remove_offset_opt p v v' range man flow =
    match p with
    | None -> Post.return flow
    | Some (p,mode) ->
      debug "remove_offset_opt %a: old=%a, new=%a" pp_var p Value.print v Value.print v';
      if Value.is_valid v && not (Value.is_valid v')
      then let () = debug "remove" in man.post ~zone:Z_u_num (mk_remove (mk_offset p mode range) range) flow
      else Post.return flow


  

  (** {2 Pointer evaluation} *)
  (** ====================== *)

  (** Evaluation a pointer expression into a points-to expression *)
  let eval_points_to exp man flow =
    Static_points_to.eval_opt exp |> Option.lift @@ fun ptr ->

    match ptr with
    | AddrOf (base, offset) ->
      Eval.singleton (mk_c_points_to_bloc base offset exp.erange) flow

    | Eval (p, mode, offset) ->
      let offset' = mk_binop (mk_offset p mode exp.erange) O_plus offset ~etyp:T_int exp.erange in
      let a = get_env T_cur man flow in
      let values = Map.find p a in
      let evals = Value.fold_points_to (fun v pt acc ->
          let flow = set_env T_cur (Map.add p v a) man flow in
          Eval.singleton (mk_c_points_to pt exp.erange) flow :: acc
        ) values offset' []
      in
      Eval.join_list evals ~empty:(Eval.empty_singleton flow)

    | Fun f ->
      Eval.singleton (mk_c_points_to_fun f exp.erange) flow

    | Null ->
      Eval.singleton (mk_c_points_to_null exp.erange) flow

    | Invalid ->
      Eval.singleton (mk_c_points_to_invalid exp.erange) flow

    | Top ->
      Eval.singleton (mk_c_points_to_top exp.erange) flow


  (** 𝔼⟦ p - q ⟧ *)
  let eval_diff p q range man flow =
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
      let v = Value.meet v1 v2 in
      if Value.is_bottom v
      then []
      else
        let flow = set_value_opt p1 v man flow |>
                   set_value_opt p2 v man
        in
        let ee =
          mk_offset_opt p1 v o1 range |> Option.bind @@ fun o1 ->
          mk_offset_opt p2 v o2 range |> Option.bind @@ fun o2 ->
          let e = sub o1 o2 range in
          if Z.equal elem_size Z.one
          then Some e
          else Some (div e (mk_z elem_size range) range)
        in
        match ee with
        | None -> [man.eval ~zone:(Z_c_scalar, Z_u_num) (mk_top T_int range) flow] (* FIXME: why not return 0? *)
        | Some e -> [man.eval ~zone:(Z_c_scalar, Z_u_num) e flow]
    in

    (* Case 2: different base => undefined behavior *)
    let case2 =
      let v1 = Value.diff v1 v2 in
      let v2 = Value.diff v2 v1 in
      if Value.is_bottom v1 || Value.is_bottom v2
      then []
      else
        let flow = set_value_opt p1 v1 man flow |>
                   set_value_opt p2 v2 man
        in
        let flow = raise_c_alarm Alarms.AIllegalPointerDiff range ~bottom:true man.lattice flow in
        [Eval.empty_singleton flow]
    in

    Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow)




  (** Evaluation of a pointer comparison into a numeric expression *)
  let eval_compare exp man flow =
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
      man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) exp' flow |>
      Option.return

    (* 𝔼⟦ p1 - p2 ⟧ *)
    | E_binop(O_minus, p1, p2)
      when is_c_pointer_type p1.etyp &&
           is_c_pointer_type p2.etyp
      ->
      eval_diff p1 p2 exp.erange man flow |>
      Option.return

    | _ -> None


  (** Entry point of abstraction evaluations *)
  let eval zone exp man flow =
    match zone with
    | Z_c_scalar, Z_c_points_to -> eval_points_to exp man flow
    | Z_c_scalar, Universal.Zone.Z_u_num -> eval_compare exp man flow
    | _ -> None



  (** {2 Computation of post-conditions} *)
  (** ================================== *)

  let remove_offset o mode range man flow =
    if mode = STRONG then
      man.post ~zone:(Universal.Zone.Z_u_num) (mk_remove o range) flow
    else
      Post.return flow


  (** Assignment abstract transformer *)
  let assign p q mode range man flow =
    let o = mk_offset p mode range in
    match Static_points_to.eval q with
    | AddrOf (b, offset) ->
      let flow' = map_env T_cur (add p (Value.base b) mode) man flow in

      man.eval offset ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow' >>$ fun offset flow' ->
      man.post ~zone:(Universal.Zone.Z_u_num) (mk_assign o offset range) flow'

    | Eval (q, mode', offset) ->
      let flow' = map_env T_cur (fun a ->
          add p (Map.find q a) mode a
        ) man flow
      in
      (* Assign offset only if q points to a valid block *)
      let a = get_env T_cur man flow in
      if Map.find q a |> Value.is_valid
      then
        let qo = mk_offset q mode' range in
        let offset' = mk_binop qo O_plus offset ~etyp:T_int range in

        man.eval offset' ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow' >>$ fun offset' flow ->
        man.post ~zone:(Universal.Zone.Z_u_num) (mk_assign o offset' range) flow'
      else
        remove_offset o mode range man flow'

    | Fun f ->
      map_env T_cur (add p (Value.cfun f) mode) man flow |>
      remove_offset o mode range man

    | Invalid ->
      map_env T_cur (add p Value.invalid mode) man flow  |>
      remove_offset o mode range man

    | Null ->
      map_env T_cur (add p Value.null mode) man flow |>
      remove_offset o mode range man

    | Top ->
      map_env T_cur (add p Value.top mode) man flow |>
      man.post ~zone:(Universal.Zone.Z_u_num) (mk_assign o (mk_top T_int range) range)



  (* Declaration of a scalar pointer variable *)
  let declare_var v init scope range man flow =
    match scope, init with
    (** Uninitialized global variable *)
    | Variable_global, None | Variable_file_static _, None ->
      (* The variable is initialized with NULL (C99 6.7.8.10) *)
      map_env T_cur (Map.add v Value.null) man flow |>
      Post.return

    (** Uninitialized local variable *)
    | Variable_local _, None | Variable_func_static _, None ->
      (* The value of the variable is undetermined (C99 6.7.8.10) *)
      map_env T_cur (Map.add v Value.invalid) man flow |>
      Post.return

    | _, Some (C_init_expr e) ->
      assign v e STRONG range man flow

    | _ -> assert false



  (** Add a pointer variable to the support of the non-rel map *)
  let add_var p range man flow =
    let o = mk_offset p STRONG range in
    map_env T_cur (Map.add p Value.top) man flow |>
    man.post ~zone:(Universal.Zone.Z_u_num) (mk_add o range)



  (** Remove a pointer variable from the support of the non-rel map *)
  let remove_var p range man flow =
    let flow = map_env T_cur (Map.remove p) man flow in
    let o = mk_offset p STRONG range in
    man.post ~zone:(Universal.Zone.Z_u_num) (mk_remove o range) flow



  (** Make pointers pointing to addr as invalid *)
  let invalidate_addr addr range man flow =
    let base = A addr in
    let flow' = map_env T_cur (fun a ->
        let a' = Map.map (fun v ->
            if not (Value.mem_base base v)
            then
              v
            else
              let v' = Value.invalidate v in
              if addr.addr_mode = STRONG then
                Value.remove_base base v'
              else
                v'
          ) a
        in
        a'
      ) man flow
    in
    Post.return flow'



  (** Rename a pointer variable *)
  let rename_var p1 p2 range man flow =
    let flow' = map_env T_cur (Map.rename p1 p2) man flow in

    (* Rename the offset if present *)
    let a1 = get_env T_cur man flow |> Map.find p1 in
    if Value.is_valid a1 then
      let o1 = mk_offset p1 STRONG range in
      let o2 = mk_offset p2 STRONG range in
      man.post ~zone:(Universal.Zone.Z_u_num) (mk_rename o1 o2 range) flow'
    else
      Post.return flow'



  (** Rename a pointed address *)
  let rename_addr addr1 addr2 range man flow =
    let base1 = A addr1 in
    let base2 = A addr2 in
    map_env T_cur (Map.map (Value.rename_base base1 base2)) man flow |>
    Post.return


  (** Filter equal pointers *)
  let assume_eq p q range man flow =
    let v1, o1, p1 = Static_points_to.eval p |>
                     eval_static_points_to man flow
    in
    let v2, o2, p2 = Static_points_to.eval q |>
                     eval_static_points_to man flow
    in
    let v = Value.meet v1 v2 in
    if Value.is_bottom v then
      Flow.set T_cur man.lattice.bottom man.lattice flow |>
      Post.return
    else
      let flow = set_value_opt p1 v man flow |>
                 set_value_opt p2 v man
      in
      remove_offset_opt p1 v1 v range man flow >>$ fun () flow ->
      remove_offset_opt p2 v2 v range man flow >>$ fun () flow ->
      match mk_offset_constraint_opt O_eq p1 v1 o1 p2 v2 o2 range with
      | None -> Post.return flow
      | Some cond ->
        man.eval ~zone:(Z_c_scalar,Z_u_num) cond flow >>$ fun cond flow ->
        man.post ~zone:Z_u_num (mk_assume cond range) flow



  (** Filter non-equal pointers *)
  let assume_ne p q range man flow =
    debug "assume_ne %a %a" pp_expr p pp_expr q;
    let v1, o1, p1 = Static_points_to.eval p |>
                     eval_static_points_to man flow
    in
    let v2, o2, p2 = Static_points_to.eval q |>
                     eval_static_points_to man flow
    in
    debug "o1 = %a" (Option.print pp_expr) o1;
    debug "o2 = %a" (Option.print pp_expr) o2;
    (* Case 1: p and q point to the same base *)
    let same_base_case =
      let v = Value.meet v1 v2 in
      match Value.is_bottom v,
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
          man.eval ~zone:(Z_c_scalar,Z_u_num) cond flow >>$ fun cond flow ->
          man.post ~zone:Z_u_num (mk_assume cond range) flow
        ]
    in

    (* Case 2: p and q point to different bases *)
    let different_base_case =
      let vv1 = Value.singleton_diff v1 v2 in
      let vv2 = Value.singleton_diff v2 v1 in
      if Value.is_bottom vv1 || Value.is_bottom vv2 then
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
    Post.join_list (same_base_case @ different_base_case) ~empty:bottom_case



  (** Filter ordered pointers *)
  let assume_order op p q range man flow =
    let v1, o1, p1 = Static_points_to.eval p |>
                     eval_static_points_to man flow
    in
    let v2, o2, p2 = Static_points_to.eval q |>
                     eval_static_points_to man flow
    in
    debug "o1 = %a" (Option.print pp_expr) o1;
    debug "o2 = %a" (Option.print pp_expr) o2;
    (* Case 1: p and q point to the same base *)
    let same_base_case =
      let v = Value.meet v1 v2 in
      if Value.is_bottom v
      then []
      else
        let () = debug "case 1" in
        [
          remove_offset_opt p1 v1 v range man flow >>$ fun () flow ->
          remove_offset_opt p2 v2 v range man flow >>$ fun () flow ->
          match mk_offset_constraint_opt op p1 v1 o1 p2 v2 o2 range with
          | None -> Post.return flow
          | Some cond ->
            man.eval ~zone:(Z_c_scalar,Z_u_num) cond flow >>$ fun cond flow ->
            man.post ~zone:Z_u_num (mk_assume cond range) flow
        ]
    in

    (* Case 2: p and q point to different bases *)
    let different_base_case =
      let vv1 = Value.singleton_diff v1 v2 in
      let vv2 = Value.singleton_diff v2 v1 in
      if Value.is_bottom vv1 || Value.is_bottom vv2
      then []
      else
        let () = debug "case 2" in
        let flow = set_value_opt p1 vv1 man flow |>
                   set_value_opt p2 vv2 man
        in
        let flow = raise_c_alarm Alarms.AIllegalPointerOrder range ~bottom:true man.lattice flow in
        debug "%a" (Flow.print man.lattice.print) flow;
        [ Post.return flow ]
    in
    let bottom_case = Flow.set T_cur man.lattice.bottom man.lattice flow |>
                      Post.return
    in
    Post.join_list (same_base_case @ different_base_case) ~empty:bottom_case




  (** Entry point of abstract transformers *)
  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration(p,init,scope) when is_c_pointer_type p.vtyp ->
      declare_var p init scope stmt.srange man flow |>
      Option.return

    | S_assign({ekind = E_var(p, mode)}, q) when is_c_pointer_type p.vtyp ->
      assign p q mode stmt.srange man flow |>
      Option.return

    | S_add { ekind = E_var (p, _) } when is_c_pointer_type p.vtyp ->
      add_var p stmt.srange man flow |>
      Option.return

    | S_remove { ekind = E_var (p, _) } when is_c_pointer_type p.vtyp ->
      remove_var p stmt.srange man flow |>
      Option.return

    | S_remove { ekind = E_addr addr } ->
      invalidate_addr addr stmt.srange man flow |>
      Option.return

    | S_rename({ekind = E_var (p1, _)}, {ekind = E_var (p2, _)})
      when is_c_pointer_type p1.vtyp &&
           is_c_pointer_type p2.vtyp
      ->
      rename_var p1 p2 stmt.srange man flow |>
      Option.return

    | S_rename ({ekind = E_addr addr1}, {ekind = E_addr addr2}) ->
      rename_addr addr1 addr2 stmt.srange man flow |>
      Option.return

    (* S⟦ ?(p == q) ⟧ *)
    | S_assume({ ekind = E_binop(O_eq, p, q) })
    | S_assume({ ekind = E_unop(O_log_not, {ekind = E_binop(O_ne, p, q)}) })
      when is_c_pointer_type p.etyp ||
           is_c_pointer_type q.etyp
      ->
      assume_eq p q stmt.srange man flow |>
      Option.return

    (* S⟦ ?(p != q) ⟧ *)
    | S_assume ({ ekind = E_binop(O_ne, p, q) })
    | S_assume ({ ekind = E_unop(O_log_not, {ekind = E_binop(O_eq, p, q)}) })
      when is_c_pointer_type p.etyp ||
           is_c_pointer_type q.etyp
      ->
      assume_ne p q stmt.srange man flow |>
      Option.return


    (* S⟦ ?(p op q) | op ∈ {<, <=, >, >=} ⟧ *)
    | S_assume ({ ekind = E_binop((O_lt | O_le | O_gt | O_ge) as op, p, q) })
      when is_c_pointer_type p.etyp &&
           is_c_pointer_type q.etyp
      ->
      assume_order op p q stmt.srange man flow |>
      Option.return

    (* S⟦ ?!(p op q) | op ∈ {<, <=, >, >=} ⟧ *)
    | S_assume ({ ekind = E_unop (O_log_not, { ekind = E_binop((O_lt | O_le | O_gt | O_ge) as op, p, q) })})
      when is_c_pointer_type p.etyp &&
           is_c_pointer_type q.etyp
      ->
      assume_order (negate_comparison_op op) p q stmt.srange man flow |>
      Option.return

    (* S⟦ ?NULL ⟧ *)
    | S_assume ({ ekind = E_c_cast({ ekind = E_constant (C_int n) } as exp, _) })
      when is_c_pointer_type exp.etyp &&
           Z.equal n Z.zero ->
      Flow.set T_cur man.lattice.bottom man.lattice flow |>
      Post.return |>
      Option.return

    (* S⟦ ?NULL ⟧ *)
    | S_assume ({ ekind = E_unop (O_log_not, { ekind = E_c_cast({ ekind = E_constant (C_int n) } as exp, _) }) })
      when is_c_pointer_type exp.etyp &&
           Z.equal n Z.zero ->
      Post.return flow |>
      Option.return

    (* S⟦ ?INVALID ⟧ *)
    (* S⟦ ?!INVALID ⟧ *)
    | S_assume ({ ekind = E_constant (C_c_invalid) })
    | S_assume ({ ekind = E_unop (O_log_not, { ekind = E_constant (C_c_invalid) }) }) ->
      Post.return flow |>
      Option.return

    (* S⟦ ?⊤ ⟧ *)
    (* S⟦ ?!⊤ ⟧ *)
    | S_assume ({ ekind = E_constant (C_top t) })
    | S_assume ({ ekind = E_unop (O_log_not, { ekind = E_constant (C_top t) }) })
      when is_c_pointer_type t ->
      Post.return flow |>
      Option.return

    (* S⟦ ?p ⟧ *)
    | S_assume ({ ekind = E_var _ } as exp)
    | S_assume ({ ekind = E_c_cast({ ekind = E_var _ },_) } as exp)
      when is_c_pointer_type exp.etyp ->
      assume_ne exp (mk_zero stmt.srange ~typ:(T_c_pointer T_c_void)) stmt.srange man flow |>
      Option.return

    (* S⟦ ?!p ⟧ *)
    | S_assume ({ ekind = E_unop (O_log_not, ({ekind = E_var _} as exp)) })
    | S_assume ({ ekind = E_unop (O_log_not, ({ ekind = E_c_cast({ ekind = E_var _ },_) } as exp)) })
      when is_c_pointer_type exp.etyp ->
      assume_eq exp (mk_zero stmt.srange ~typ:(T_c_pointer T_c_void)) stmt.srange man flow |>
      Option.return


    (* S⟦ ?"..." ⟧ *)
    | S_assume ({ ekind = E_constant (C_c_string _) }) ->
      Post.return flow |>
      Option.return

    (* S⟦ ?!"..." ⟧ *)
    | S_assume ({ ekind = E_unop(O_log_not, ({ekind = E_constant (C_c_string _)})) }) ->
      Flow.set T_cur man.lattice.bottom man.lattice flow |>
      Post.return |>
      Option.return

    (* S⟦ (t)p ⟧ *)
    | S_assume ({ ekind = E_c_cast(p, _) })
      when is_c_pointer_type p.etyp ->
      man.post ~zone (mk_assume p stmt.srange) flow |>
      Option.return


    (* S⟦ !(t)p ⟧ *)
    | S_assume ({ ekind = E_unop (O_log_not, ({ ekind = E_c_cast(p, _) })) })
      when is_c_pointer_type p.etyp ->
      man.post ~zone (mk_assume (mk_not p stmt.srange) stmt.srange) flow |>
      Option.return



    | _ -> None



  (** {2 Handler of queries} *)
  (** ====================== *)

  let ask _ _ _ = None

  let refine channel man flow = Channel.return flow

end

let () =
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Domain)
