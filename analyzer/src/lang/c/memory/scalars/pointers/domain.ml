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


module Domain =
struct


  (** {2 Non-relational map} *)
  (** ====================== *)

  module Map =
    Framework.Transformers.Value.Nonrel.Make(
      Framework.Core.Sig.Value.Simplified.MakeLowlevel(Value)
    )


  (** {2 Domain header} *)
  (** ================= *)

  type t = Map.t

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.memory.scalars.pointer"
    end)

  let bottom = Map.bottom

  let top = Map.top

  let print fmt a =
    Map.print fmt a

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
    Map.widen ctx a a', s, s', true

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

    Map.merge pre (a,block) (a',block')


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
  let mk_offset_expr (p:var) mode range : expr =
    mk_var (mk_offset_var p) ~mode range


  (** {2 Utility functions for symbolic evaluations} *)
  (** ============================================== *)

  (* Get the base and eventual pointer offset from a symbolic pointer evaluation *)
  let symbolic_to_value (p:Symbolic.t) man flow : (Value.t * expr option * (var * mode) option) =
    match p with
    | Symbolic.AddrOf (b, o) ->
      Value.base b, Some o, None

    | Eq(q, mode, o) ->
      let v = get_env T_cur man flow |>
              Map.find q
      in
      v, (if Value.is_valid_base v then Some o else None), (Some (q,mode))

    | Null ->
      Value.null, None, None

    | Invalid ->
      Value.invalid, None, None

    | Top ->
      Value.top, None, None

    | Fun _ ->
      panic ~loc:__LOC__ "symbolic_to_value: function pointers not supported"


  (* Set value of an optional pointer returned by [symbolic_to_value] *)
  let set_value_opt p v man flow =
    match p with
    | None -> flow
    | Some (p,mode) -> map_env T_cur (add p v mode) man flow


  (* Create the offset expression from optional pointer info *)
  let mk_offset_expr_opt p v o range =
    if not (Value.is_valid v)
    then None
    else
    match p, o with
    | Some (pp,mode), Some oo ->
      Some (mk_binop (mk_offset_expr pp mode range) O_plus oo range ~etyp:T_int)

    | None, Some oo ->
      Some oo

    | _ -> None


  (* Offset conditions for comparing two pointers *)
  let mk_offset_constraint_opt op p1 v1 o1 p2 v2 o2 range =
    mk_offset_expr_opt p1 v1 o1 range |> Option.bind @@ fun e1 ->
    mk_offset_expr_opt p2 v2 o2 range |> Option.bind @@ fun e2 ->
    Some (mk_binop e1 op e2 range)


  let eval_offset_constraint_same_base_opt op p1 v1 o1 p2 v2 o2 range man flow =
    match mk_offset_constraint_opt op p1 v1 o1 p2 v2 o2 range, op with
    | None, O_eq -> Eval.singleton (mk_one range) flow
    | None, O_ne -> Eval.singleton (mk_zero range) flow
    | Some cond,_ -> man.eval ~zone:(Z_c_scalar,Z_u_num) cond flow
    | _ -> assert false


  let remove_offset_opt p v v' range man flow =
    match p with
    | None -> flow
    | Some (p,mode) ->
      if Value.is_valid_base v && not (Value.is_valid_base v')
      then man.exec ~zone:Z_u_num (mk_remove (mk_offset_expr p mode range) range) flow
      else flow


  (** {2 Pointer evaluation} *)
  (** ====================== *)


  (** Evaluation of points-to information *)
  let eval_points_to exp man flow =
    Symbolic.eval_opt exp |> Option.lift @@ fun ptr ->

    match ptr with
    | Symbolic.AddrOf (base, offset) ->
      Eval.singleton (mk_c_points_to_bloc base offset exp.erange) flow

    | Eq (p, mode, offset) ->
      let offset' = mk_binop (mk_offset_expr p mode exp.erange) O_plus offset ~etyp:T_int exp.erange in
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
      let el = [
        Eval.singleton (mk_c_points_to_valid exp.erange) flow;
        Eval.singleton (mk_c_points_to_null exp.erange) flow;
        Eval.singleton (mk_c_points_to_invalid exp.erange) flow
      ]
      in
      Eval.join_list ~empty:(Eval.empty_singleton flow) el



  (** 𝔼⟦ p == q ⟧ *)
  let eval_eq p q range man flow =
    (* Evaluate the pointed bases symbolically *)
    let sp = Symbolic.eval p in
    let sq = Symbolic.eval q in

    let v1, o1, p1 = symbolic_to_value sp man flow in
    let v2, o2, p2 = symbolic_to_value sq man flow in

    (* Compute common pointed addresses *)
    let v = Value.meet v1 v2 in

    let flow = set_value_opt p1 v man flow |>
               set_value_opt p2 v man
    in

    (* Refine offsets in case v is a valid address *)
    let flow = remove_offset_opt p1 v1 v range man flow |>
               remove_offset_opt p2 v2 v range man
    in

    if Value.is_bottom v then
      Eval.singleton (mk_zero range) flow
    else
      eval_offset_constraint_same_base_opt O_eq p1 v1 o1 p2 v2 o2 range man flow



  (** 𝔼⟦ p != q ⟧ *)
  let eval_ne p q range man flow =
    (* Evaluate the pointed bases symbolically *)
    let sp = Symbolic.eval p in
    let sq = Symbolic.eval q in

    let v1, o1, p1 = symbolic_to_value sp man flow in
    let v2, o2, p2 = symbolic_to_value sq man flow in

    (* Case 1: same valid bases *)
    let case1 =
      let v = Value.meet v1 v2 in
      if Value.is_bottom v
      then []
      else
        let flow = set_value_opt p1 v man flow |>
                   set_value_opt p2 v man |>
                   remove_offset_opt p1 v1 v range man |>
                   remove_offset_opt p2 v2 v range man
        in
        [eval_offset_constraint_same_base_opt O_ne p1 v o1 p2 v o2 range man flow]
    in

    (* Case 2: different bases *)
    let case2 =
      let v1' = Value.diff v1 v2 in
      let v2' = Value.diff v2 v1 in
      if Value.is_bottom v1' || Value.is_bottom v2'
      then []
      else
        let flow = set_value_opt p1 v1' man flow |>
                   set_value_opt p2 v2' man |>
                   remove_offset_opt p1 v1 v1' range man |>
                   remove_offset_opt p2 v2 v2' range man
        in
        [Eval.singleton (mk_one range) flow]

    in
    Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow)


  (** 𝔼⟦ p op q | op ∈ {<, <=, >, >=} ⟧ *)
  let eval_order op p q range man flow =
    (* Evaluate the pointed bases symbolically *)
    let sp = Symbolic.eval p in
    let sq = Symbolic.eval q in

    let v1, o1, p1 = symbolic_to_value sp man flow in
    let v2, o2, p2 = symbolic_to_value sq man flow in

    (* Case 1: same valid bases *)
    let case1 =
      let v = Value.meet v1 v2 |> Value.meet Value.valid_top in
      if Value.is_bottom v
      then []
      else
        let flow = set_value_opt p1 v man flow |>
                   set_value_opt p2 v man
        in
        [eval_offset_constraint_same_base_opt op p1 v o1 p2 v o2 range man flow]
    in

    (* Case 2: different bases => undefined behavior *)
    let case2 =
      let v1 = Value.diff v1 v2 in
      let v2 = Value.diff v2 v1 in
      if Value.is_bottom v1 || Value.is_bottom v2
      then []
      else
        let flow = set_value_opt p1 v1 man flow |>
                   set_value_opt p2 v2 man
        in
        let flow = raise_alarm Alarms.AIllegalPointerOrder range ~bottom:true man.lattice flow in
        [Eval.empty_singleton flow]
    in

    Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow)


  (** 𝔼⟦ p - q ⟧ *)
  let eval_diff p q range man flow =
    (* p1 and p2 should point to the same type *)
    if compare_typ (under_type p.etyp) (under_type q.etyp) <> 0
    then panic_at range
        "%a - %a: pointers do not point to the same type"
        pp_expr p pp_expr q
    ;

    (* Evaluate the pointed bases symbolically *)
    let sp = Symbolic.eval p in
    let sq = Symbolic.eval q in

    let v1, o1, p1 = symbolic_to_value sp man flow in
    let v2, o2, p2 = symbolic_to_value sq man flow in

    (* Size of a pointed element *)
    let elem_size = under_type p.etyp |> void_to_char |> sizeof_type in

    (* Case 1 : same base => return difference of offset *)
    let case1 =
      let v = Value.meet v1 v2 |> Value.meet Value.valid_top in
      if Value.is_bottom v
      then []
      else
        let flow = set_value_opt p1 v man flow |>
                   set_value_opt p2 v man
        in
        let ee =
          mk_offset_expr_opt p1 v o1 range |> Option.bind @@ fun o1 ->
          mk_offset_expr_opt p2 v o2 range |> Option.bind @@ fun o2 ->
          let e = sub o1 o2 range in
          if Z.equal elem_size Z.one
          then Some e
          else Some (div e (mk_z elem_size range) range)
        in
        match ee with
        | None -> [man.eval ~zone:(Z_c_scalar, Z_u_num) (mk_top T_int range) flow]
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
        let flow = raise_alarm Alarms.AIllegalPointerDiff range ~bottom:true man.lattice flow in
        [Eval.empty_singleton flow]
    in

    Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow)



  (** 𝔼⟦ valid_ptr(p) ⟧ *)
  let eval_is_valid p range man flow =
    (* A valid pointer is not NULL nor INVALID, and its offset is
       within [0, sizeof(base) - sizeof(under_type t) [ *)

    (* Evaluate the pointed address *)
    eval_points_to p man flow |>
    Option.lift @@ Eval.bind @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to(P_block(b, o)) ->
      (* Evaluate the size of the base *)
      Common.Base.eval_base_size b range man flow |>
      Eval.bind @@ fun size flow ->

      man.eval size ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow |>
      Eval.bind @@ fun size flow ->

      let elm = under_type p.etyp |> void_to_char |> (fun t -> mk_z (sizeof_type t) range)
      in

      (* Check validity of the offset *)
      let cond = mk_in o (mk_zero range) (sub size elm range) range in
      Eval.singleton cond flow

    | E_c_points_to(P_fun _) -> Eval.singleton (mk_one range) flow

    | E_c_points_to(P_null | P_invalid) -> Eval.singleton (mk_zero range) flow

    | E_c_points_to(P_valid) -> Eval.singleton (mk_top T_bool range) flow

    | _ -> panic_at range "is_valid(%a | %a %a) not supported"
             pp_expr p pp_expr p pp_expr pt



  (** Evaluation of pointer comparisons *)
  let rec eval_compare exp man flow =
    match ekind exp with
    (* 𝔼⟦ p == q ⟧ *)
    | E_binop(O_eq, p, q)
    | E_unop(O_log_not, {ekind = E_binop(O_ne, p, q)})
      when is_c_pointer_type p.etyp ||
           is_c_pointer_type q.etyp
      ->
      eval_eq p q exp.erange man flow |>
      Option.return

    (* 𝔼⟦ p != q ⟧ *)
    | E_binop(O_ne, p, q)
    | E_unop(O_log_not, {ekind = E_binop(O_eq, p, q)})
      when is_c_pointer_type p.etyp ||
           is_c_pointer_type q.etyp
      ->
      eval_ne p q exp.erange man flow |>
      Option.return

    (* 𝔼⟦ p ⟧ *)
    | E_constant (C_top _)
    | E_var _ when is_c_pointer_type exp.etyp ->
      eval_ne exp (mk_zero exp.erange ~typ:(T_c_pointer T_c_void)) exp.erange man flow |>
      Option.return

    (* 𝔼⟦ !p ⟧ *)
    | E_unop (O_log_not, ({ekind = E_constant (C_top _)} as p))
    | E_unop (O_log_not, ({ekind = E_var _} as p)) when is_c_pointer_type exp.etyp ->
      eval_eq p (mk_zero exp.erange ~typ:(T_c_pointer T_c_void)) exp.erange man flow |>
      Option.return

    (* 𝔼⟦ (t)p ⟧ *)
    | E_c_cast(p, _) when is_c_pointer_type p.etyp ->
      eval_compare p man flow

    (* 𝔼⟦ valid_ptr(p) ⟧ *)
    | Stubs.Ast.E_stub_builtin_call( VALID_PTR, p) ->
      eval_is_valid p exp.erange man flow

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

    (* 𝔼⟦ p op q | op ∈ {<, <=, >, >=} ⟧ *)
    | E_binop((O_lt | O_le | O_gt | O_ge) as op, p, q)
      when is_c_pointer_type p.etyp &&
           is_c_pointer_type q.etyp
      ->
      eval_order op p q exp.erange man flow |>
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
    let o = mk_offset_expr p mode range in
    match Symbolic.eval q with
    | Symbolic.AddrOf (b, offset) ->
      let flow' = map_env T_cur (add p (Value.base b) mode) man flow in

      man.eval offset ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow' >>$ fun offset flow' ->
      man.post ~zone:(Universal.Zone.Z_u_num) (mk_assign o offset range) flow'

    | Eq (q, mode', offset) ->
      let flow' = map_env T_cur (fun a ->
          add p (Map.find q a) mode a
        ) man flow
      in
      (* Assign offset only if q points to a valid block *)
      let a = get_env T_cur man flow in
      if Map.find q a |> Value.is_valid_base
      then
        let qo = mk_offset_expr q mode' range in
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
    let o = mk_offset_expr p STRONG range in
    map_env T_cur (Map.add p Value.top) man flow |>
    man.post ~zone:(Universal.Zone.Z_u_num) (mk_add o range)



  (** Remove a pointer variable from the support of the non-rel map *)
  let remove_var p range man flow =
    let flow = map_env T_cur (Map.remove p) man flow in
    let o = mk_offset_expr p STRONG range in
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
    let flow = map_env T_cur (Map.rename p1 p2) man flow in
    let o1 = mk_offset_expr p1 STRONG range in
    let o2 = mk_offset_expr p2 STRONG range in
    man.post ~zone:(Universal.Zone.Z_u_num) (mk_rename o1 o2 range) flow



  (** Rename a pointed address *)
  let rename_addr addr1 addr2 range man flow =
    let base1 = A addr1 in
    let base2 = A addr2 in
    map_env T_cur (Map.map (Value.rename_base base1 base2)) man flow |>
    Post.return



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

    | _ -> None



  (** {2 Handler of queries} *)
  (** ====================== *)

  let ask _ _ _ = None

  let refine channel man flow = Channel.return flow

end

let () =
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Domain)
