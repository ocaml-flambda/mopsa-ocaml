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


(** {2 Value lattice of bases} *)
(** ========================== *)

module BaseSet =
struct

  (** Pointer base *)
  type pb =
    | PB_block of base         (** Memory blocks *)
    | PB_fun   of Ast.c_fundec (** Function point *)
    | PB_null                  (** Null pointer *)
    | PB_invalid               (** Invalid pointer *)


  include Framework.Lattices.Powerset.Make(
      struct

        type t = pb

        let print fmt = function
          | PB_block base -> pp_base fmt base
          | PB_fun f -> Format.pp_print_string fmt f.Ast.c_func_org_name
          | PB_null-> Format.pp_print_string fmt "NULL"
          | PB_invalid -> Format.pp_print_string fmt "Invalid"

        let compare p1 p2 =
          match p1, p2 with
          | PB_block b1, PB_block b2 -> compare_base b1 b2
          | PB_fun f1, PB_fun f2 -> compare f1.Ast.c_func_unique_name f2.Ast.c_func_unique_name
          | _, _ -> Pervasives.compare p1 p2
      end
      )


  include Framework.Core.Id.GenValueId(struct
      type typ = t
      let name = "c.memory.pointers"
      let display = "pointers"
    end)

  let zones = [Zone.Z_c_scalar]

  let null = singleton PB_null

  let invalid = singleton PB_invalid

  let block b = singleton (PB_block b)

  let bfun f = singleton (PB_fun f)

  let mem_block v =
    if is_top v then true
    else exists (function PB_block _ -> true | _ -> false) v

  let types = []

  let of_constant c =
    match c with
    | C_int n when Z.equal n Z.zero -> null
    | _ -> top

  let unop op v = top

  let binop op v1 v2 = top

  let bwd_unop = Framework.Core.Sig.Value.Simplified.default_bwd_unop

  let bwd_binop = Framework.Core.Sig.Value.Simplified.default_bwd_binop

  let filter v b =
    if b then diff v null
    else meet v null

  let is_singleton v =
    not (is_top v) &&
    cardinal v == 1

  let compare op v1 v2 r =
    let op = if r then op else negate_comparison op in
    match op with
    | O_eq ->
      let v = meet v1 v2 in
      v, v

    | O_ne ->
      if is_singleton v1 then v1, diff v2 v1
      else if is_singleton v2 then diff v1 v2, v2
      else v1, v2

    | _ -> v1, v2


  let ask _ _ = None

  let refine channel v = Channel.return v


end


(** {2 Abstract domain of pointers} *)
(** =============================== *)

module Domain =
struct


  (** {2 Domain lattice} *)
  (** ================== *)

  (** An abstract element is a partial map from pointer variables to a set
      of bases *)
  module Map =
    Framework.Transformers.Value.Nonrel.Make(
      Framework.Core.Sig.Value.Simplified.MakeLowlevel(BaseSet)
    )

  type t = Map.t

  include GenDomainId(struct
      type typ = t
      let name = "c.memory.scalars.pointer"
    end)

  let bottom = Map.bottom

  let top = Map.top

  let is_bottom a = Map.is_bottom a

  let subset man ctx (a,s) (a',s') =
    Map.subset a a', s, s'

  let join man ctx (a,s) (a',s') =
    Map.join a a', s, s'

  let meet man ctx (a,s) (a',s') =
    Map.meet a a', s, s'

  let widen man ctx (a,s) (a',s') =
    Map.widen ctx a a', s, s', true

  let merge ctx pre (a,log) (a',log') =
    let block = Log.get_domain_block log in
    let block' = Log.get_domain_block log' in

    let patch_stmt stmt a acc =
      match skind stmt with
      | S_c_declaration (var,init) ->
        let v = Map.find var a in
        Some (Map.add var v acc)
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

    Map.merge ctx pre (a,block) (a',block')


  let print fmt a =
    Map.print fmt a

  (** {2 Zoning interface} *)
  (** ==================== *)

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

  (** {2 Initialization} *)
  (** ================== *)

  let init prog man flow =
    set_domain_env T_cur Map.empty man flow


  (** {2 Utility functions for evaluations *)
  (** ==================================== *)

  (** Create the offset variable of a pointer *)
  let mk_offset_var (p:var) : var =
    let vname = "offset(" ^ p.org_vname ^ ")" in
    let uniq = vname ^ ":" ^ (string_of_int p.vuid) in
    mkv vname uniq p.vuid T_int

  let mk_offset_var_expr (p:var) range : expr =
    mk_var (mk_offset_var p) ~mode:STRONG range

  let pointed_size t =
    let tt = under_type t |> remove_typedef_qual in
    match tt with
    | T_c_void -> Z.one
    | _ -> sizeof_type tt

  (** Pointer evaluations *)
  type ptr =
    | ADDROF of Common.Base.base * expr
    | EQ of var * expr
    | FUN of c_fundec
    | NULL
    | INVALID
    | TOP

  (** Advance the offset of a pointer evaluation *)
  let advance_offset (op:operator) (ptr:ptr) (o:expr) t range : ptr =
    let size = pointed_size t in
    let advance oo =
      if Z.equal size Z.one then
        mk_binop oo op o range ~etyp:T_int
      else
        mk_binop oo op (mk_binop o O_mult (mk_z size range) range ~etyp:T_int) range ~etyp:T_int
    in
    match ptr with
    | ADDROF (b, oo) -> ADDROF (b, advance oo)

    | EQ (p, oo) -> EQ (p, advance oo)

    | NULL -> NULL

    | INVALID -> INVALID

    | FUN _ ->
      panic_at range
        "pointers.add_offset: pointer arithmetics on functions not supported"

    | TOP -> TOP


  (* Get the base and eventual pointer offset from a pointer evaluation *)
  let get_pointer_info (p:ptr) man flow : (BaseSet.t * expr option * var option) =
    match p with
    | ADDROF (b, o) -> BaseSet.block b, Some o, None

    | EQ(q, o) ->
      let b = get_domain_env T_cur man flow |>
              Map.find q
      in
      b, (if BaseSet.mem_block b then Some o else None), Some q

    | NULL ->
      BaseSet.null, None, None

    | INVALID ->
      BaseSet.invalid, None, None

    | TOP ->
      BaseSet.top, None, None

    | FUN _ -> panic ~loc:__LOC__ "function pointers not supported"


  (* Set base of an optional pointer info *)
  let set_base v b man flow =
    match v with
    | None -> flow
    | Some vv -> map_domain_env T_cur (Map.add vv b) man flow

  (* Create the offset expression from optional pointer info *)
  let offset_expr v o range =
    match v, o with
    | Some vv, Some oo -> mk_binop (mk_offset_var_expr vv range) O_plus oo range ~etyp:T_int
    | None, Some oo -> oo
    | _ -> assert false

  (* Offset conditions for comparing two pointers *)
  let compare_cond op p1 o1 p2 o2 range =
    match o1, o2 with
    | Some _, Some _ ->
      let e1 = offset_expr p1 o1 range in
      let e2 = offset_expr p2 o2 range in
      mk_binop e1 op e2 range
    | _ ->
      match op with
      | O_eq -> mk_one range
      | O_ne -> mk_zero range
      | _ -> assert false

  (** {2 Pointer evaluation} *)
  (** ====================== *)

  (** Static evaluation of a pointer base and offset *)
  let rec eval_pointer_opt exp : ptr option =
    match ekind exp with
    | E_constant(C_int n) when Z.equal n Z.zero ->
      NULL |> Option.return

    | E_constant(C_c_invalid) ->
      INVALID |> Option.return

    | E_constant(C_top t) when is_c_pointer_type t ->
      TOP |> Option.return

    | E_addr (addr) ->
      ADDROF(A addr, mk_zero exp.erange) |> Option.return

    | E_c_deref { ekind = E_c_address_of e } ->
      eval_pointer_opt e

    | E_c_address_of { ekind = E_c_deref p } ->
      eval_pointer_opt p

    | E_c_cast (e, _) ->
      eval_pointer_opt e

    | E_c_function f ->
      FUN f |> Option.return

    | E_constant (C_c_string (s, _)) ->
      ADDROF(S s, mk_zero exp.erange) |> Option.return

    | E_var (a, _) when is_c_array_type a.vtyp ->
      ADDROF(V a, mk_zero exp.erange) |> Option.return

    | E_c_deref a when is_c_array_type (under_type a.etyp) ->
      eval_pointer_opt a

    | E_c_address_of { ekind = E_var (v, _) } ->
      ADDROF (V v, mk_zero exp.erange) |> Option.return

    | E_c_address_of { ekind = E_c_function f } ->
      FUN f |> Option.return

    | E_binop(O_plus | O_minus as op, e1, e2) ->
      let p, i =
        if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp
        then e1, e2
        else e2, e1
      in
      eval_pointer_opt p |>
      Option.lift @@ fun ptr ->
      advance_offset op ptr i p.etyp exp.erange

    | E_var (v, STRONG) when is_c_pointer_type v.vtyp ->
      EQ (v, mk_zero exp.erange) |> Option.return

    | x when is_c_int_type exp.etyp ->
      ADDROF(Common.Base.Z, exp) |> Option.return

    | _ ->
      warn_at exp.erange "eval_pointer_opt: %a not supported" pp_expr exp;
      None

  let eval_pointer exp : ptr =
    match eval_pointer_opt exp with
    | Some ptr -> ptr
    | None -> panic_at exp.erange
                "eval_pointer: %a not supported"
                pp_expr exp


  (** Evaluation of points-to information *)
  let eval_points_to exp man flow =
    eval_pointer_opt exp |>
    Option.lift @@ fun ptr ->

    match ptr with
    | ADDROF (base, offset) ->
      Eval.singleton (mk_c_points_to_bloc base offset exp.erange) flow

    | EQ (p, offset) ->
      let offset' = mk_binop (mk_offset_var_expr p exp.erange) O_plus offset ~etyp:T_int exp.erange in
      let bases = Map.find p (get_domain_env T_cur man flow) in
      if BaseSet.is_top bases then
        Eval.singleton (mk_c_points_to_top exp.erange) flow
      else
        let el = BaseSet.fold (fun b acc ->
            match b with
            | PB_block b ->
              Eval.singleton (mk_c_points_to_bloc b offset' exp.erange) flow :: acc

            | PB_fun f ->
              Eval.singleton (mk_c_points_to_fun f exp.erange) flow :: acc

            | PB_null ->
              Eval.singleton (mk_c_points_to_null exp.erange) flow :: acc

            | PB_invalid ->
              Eval.singleton (mk_c_points_to_invalid exp.erange) flow :: acc
          ) bases []
        in
        Eval.join_list el ~empty:(Eval.empty_singleton flow)

    | FUN f ->
      Eval.singleton (mk_c_points_to_fun f exp.erange) flow

    | NULL ->
      Eval.singleton (mk_c_points_to_null exp.erange) flow

    | INVALID ->
      Eval.singleton (mk_c_points_to_invalid exp.erange) flow

    | TOP ->
      Eval.singleton (mk_c_points_to_top exp.erange) flow

  (** Evaluation of pointer comparisons *)
  let rec eval_pointer_compare exp man flow =
    match ekind exp with
    (* 𝔼⟦ p == q ⟧ *)
    (* 𝔼⟦ !(p != q) ⟧ *)
    | E_binop(O_eq, e1, e2)
    | E_unop(O_log_not, {ekind = E_binop(O_ne, e1, e2)})
      when is_c_pointer_type e1.etyp ||
           is_c_pointer_type e2.etyp
      ->
      (* Evaluate the pointed bases *)
      let p1 = eval_pointer e1 in
      let p2 = eval_pointer e2 in

      let b1, o1, v1 = get_pointer_info p1 man flow in
      let b2, o2, v2 = get_pointer_info p2 man flow in

      (* Compute new bases *)
      let b1', b2' = BaseSet.compare O_eq b1 b2 true in

      let flow' = set_base v1 b1' man flow |>
                  set_base v2 b2' man
      in

      (* Refine offsets in case p or q may point to a block *)
      if BaseSet.mem_block b1' && BaseSet.mem_block b2' then
        man.eval ~zone:(Z_c_scalar, Z_u_num) (compare_cond O_eq v1 o1 v2 o2 exp.erange) flow' |>
        Option.return
      else
        (* Remove offsets in other case *)
        let remove_offset v b range man flow =
          match v with
          | None -> flow
          | Some vv ->
            if BaseSet.mem_block b then
              man.exec (mk_remove (mk_offset_var_expr vv exp.erange) exp.erange) flow
            else
              flow
        in
        let flow' = remove_offset v1 b1 exp.erange man flow' |>
                    remove_offset v2 b2 exp.erange man
        in
        Eval.singleton (mk_one exp.erange) flow' |>
        Option.return

    (* 𝔼⟦ p != q ⟧ *)
    (* 𝔼⟦ !(p == q) ⟧ *)
    | E_binop(O_ne, e1, e2)
    | E_unop(O_log_not, {ekind = E_binop(O_eq, e1, e2)})
      when is_c_pointer_type e1.etyp ||
           is_c_pointer_type e2.etyp
      ->
      (* Evaluate the pointed bases *)
      let p1 = eval_pointer e1 in
      let p2 = eval_pointer e2 in

      let b1, o1, v1 = get_pointer_info p1 man flow in
      let b2, o2, v2 = get_pointer_info p2 man flow in

      (* Compute new bases *)
      let b1eq, b2eq = BaseSet.compare O_eq b1 b2 true in
      let b1ne, b2ne = BaseSet.compare O_ne b1 b2 true in

      (* Case 1: different bases *)
      let case1 =
        if not (BaseSet.is_bottom b1ne) && not (BaseSet.is_bottom b2ne) then
          let flow' = set_base v1 b1ne man flow |>
                      set_base v2 b2ne man
          in
          [Eval.singleton (mk_one exp.erange) flow']
        else
          []
      in

      (* Case 2: same base => different offsets *)
      let case2 =
        if not (BaseSet.is_bottom b1eq) && not (BaseSet.is_bottom b2eq) then
          let flow' = set_base v1 b1eq man flow |>
                      set_base v2 b2eq man
          in
          [man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) (compare_cond O_ne v1 o1 v2 o2 exp.erange) flow']
        else
          []
      in
      Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow) |>
      Option.return


    (* 𝔼⟦ p ⟧ *)
    | E_var(p, _) when is_c_pointer_type p.vtyp ->
      let exp' = mk_binop exp O_ne (mk_zero exp.erange) exp.erange in

      eval_pointer_compare exp' man flow

    (* 𝔼⟦ !p ⟧ *)
    | E_unop (O_log_not, ({ekind = E_var (p, _)} as e)) when is_c_pointer_type p.vtyp ->
      let exp' = mk_binop e O_eq (mk_zero exp.erange) exp.erange in

      eval_pointer_compare exp' man flow

    (* 𝔼⟦ (t)p ⟧ *)
    | E_c_cast(p, _) when is_c_pointer_type p.etyp ->
      eval_pointer_compare p man flow

    (* 𝔼⟦ ptr_valid(p) ⟧ *)
    | Stubs.Ast.E_stub_builtin_call( PTR_VALID, p) ->
      (* A valid pointer is not NULL nor INVALID and its offset is
         within [0, sizeof(base) - sizeof(under_type t) [ *)

      (* Evaluate the pointed memory block *)
      eval_points_to p man flow |>
      Option.lift @@ Eval.bind @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to(P_block(b, o)) ->
          (* Evaluate the size of the base *)
          Common.Base.eval_base_size b exp.erange man flow |>
          Eval.bind @@ fun size flow ->

          man.eval size ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow |>
          Eval.bind @@ fun size flow ->

          (* Check validity of the offset *)
          let cond = mk_in o (mk_zero exp.erange) size exp.erange in
          Eval.singleton cond flow

        | E_c_points_to(P_fun _) -> Eval.singleton (mk_one exp.erange) flow

        | E_c_points_to(P_null | P_invalid) -> Eval.singleton (mk_zero exp.erange) flow

        | E_c_points_to(P_top) -> Eval.singleton (mk_top T_bool exp.erange) flow

        | _ -> panic_at exp.erange "valid(%a) not supported" pp_expr pt
      end

    (* 𝔼⟦ p1 - p2 ⟧ *)
    | E_binop(O_minus, p1, p2)
      when is_c_pointer_type p1.etyp &&
           is_c_pointer_type p2.etyp
      ->
      (* p1 and p2 should point to the same type *)
      if compare_typ (under_type p1.etyp) (under_type p2.etyp) <> 0
      then panic_at exp.erange
          "%a: pointers do not point to the same type"
          pp_expr exp
      ;

      (* Evaluate pointers *)
      let b1, o1, v1 = get_pointer_info (eval_pointer p1) man flow in
      let b2, o2, v2 = get_pointer_info (eval_pointer p2) man flow in

      (* Check if the bases are the same *)
      (* Compute new bases *)
      let b1eq, b2eq = BaseSet.compare O_eq b1 b2 true in
      let b1ne, b2ne = BaseSet.compare O_ne b1 b2 true in

      (* Size of a pointed element *)
      let elem_size = pointed_size p1.etyp in

      (* Case 1 : same base => return difference of offset *)
      let case1 =
        if not (BaseSet.is_bottom b1eq) &&
           not (BaseSet.is_bottom b2eq) &&
           BaseSet.mem_block b1eq &&
           BaseSet.mem_block b2eq
        then
          let flow' = set_base v1 b1eq man flow |>
                      set_base v2 b2eq man
          in
          let o1 = offset_expr v1 o1 exp.erange in
          let o2 = offset_expr v2 o2 exp.erange in
          let e = div (sub o1 o2 exp.erange) (mk_z elem_size exp.erange) exp.erange in
          [man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) e flow']
        else
          []
      in

      (* Case 2: different base => raise an alarm *)
      let case2 =
        if not (BaseSet.is_bottom b1ne) &&
           not (BaseSet.is_bottom b2ne)
        then
          let flow' = set_base v1 b1ne man flow |>
                      set_base v2 b2ne man
          in
          let flow'' = raise_alarm Alarms.AIllegalPointerDiff ~bottom:true exp.erange man.lattice flow' in
          [Eval.empty_singleton flow'']
        else
          []
      in

      Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow) |>
      Option.return

    (* 𝔼⟦ (t)p - (t)q | t is a numeric type ⟧ *)
    | E_binop(O_minus, { ekind = E_c_cast(p, _); etyp = t1 }, { ekind = E_c_cast(q, _); etyp = t2 })
      when is_c_pointer_type p.etyp &&
           is_c_pointer_type q.etyp &&
           is_c_int_type t1 &&
           compare_typ t1 t2 = 0
      ->
      let diff = mk_c_cast (sub p q ~typ:s32 exp.erange) t1 exp.erange in
      let exp' = mul (mk_z (pointed_size p.etyp) ~typ:t1 exp.erange) diff ~typ:t1 exp.erange in
      man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) exp' flow |>
      Option.return

    (* 𝔼⟦ p op q | op ∈ {<, <=, >, >=} ⟧ *)
    | E_binop((O_lt | O_le | O_gt | O_ge) as op, p, q)
      when is_c_pointer_type p.etyp &&
           is_c_pointer_type q.etyp
      ->
      (* Evaluate the pointed bases *)
      let p1 = eval_pointer p in
      let p2 = eval_pointer q in

      let b1, o1, v1 = get_pointer_info p1 man flow in
      let b2, o2, v2 = get_pointer_info p2 man flow in

      (* Compute new bases *)
      let b1eq, b2eq = BaseSet.compare O_eq b1 b2 true in
      let b1ne, b2ne = BaseSet.compare O_ne b1 b2 true in

      (* Case 1: different bases *)
      let case1 =
        if not (BaseSet.is_bottom b1ne) && not (BaseSet.is_bottom b2ne) then
          let flow' = set_base v1 b1ne man flow |>
                      set_base v2 b2ne man
          in
          [Eval.singleton (mk_top T_bool exp.erange) flow']
        else
          []
      in

      (* Case 2: same base => compare offsets *)
      let case2 =
        if not (BaseSet.is_bottom b1eq) && not (BaseSet.is_bottom b2eq) then
          let flow' = set_base v1 b1eq man flow |>
                      set_base v2 b2eq man
          in
          [man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) (compare_cond op v1 o1 v2 o2 exp.erange) flow']
        else
          []
      in
      Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow) |>
      Option.return



    | _ -> None


  let eval zone exp man flow =
    match zone with
    | Z_c_scalar, Z_c_points_to -> eval_points_to exp man flow
    | Z_c_scalar, Universal.Zone.Z_u_num -> eval_pointer_compare exp man flow
    | _ -> None



  (** {2 Computation of post-conditions} *)
  (** ================================== *)

  (** Assignment abstract transformer *)
  let assign p q range man flow =
    let o = mk_offset_var_expr p range in
    match eval_pointer q with
    | ADDROF (b, offset) ->
      let flow' = map_domain_env T_cur (Map.add p (BaseSet.block b)) man flow in

      man.eval offset ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow' |>
      post_eval man @@ fun offset flow' ->

      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_assign o offset range) flow'

    | EQ (q, offset) ->
      let flow' = map_domain_env T_cur (fun a -> Map.add p (Map.find q a) a) man flow in
      (* Assign offset only if q points to a block *)
      if mem_domain_env T_cur (fun a -> Map.find q a |> BaseSet.mem_block) man flow then
        let qo = mk_offset_var_expr q range in
        let offset' = mk_binop qo O_plus offset ~etyp:T_int range in

        man.eval offset' ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow' |>
        post_eval man @@ fun offset' flow ->

        man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_assign o offset' range) flow'
      else
        man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_remove o range) flow'

    | FUN f ->
      map_domain_env T_cur (Map.add p (BaseSet.bfun f)) man flow |>
      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_remove o range)

    | INVALID ->
      map_domain_env T_cur (Map.add p BaseSet.invalid) man flow  |>
      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_remove o range)

    | NULL ->
      map_domain_env T_cur (Map.add p BaseSet.null) man flow |>
      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_remove o range)

    | TOP ->
      map_domain_env T_cur (Map.add p BaseSet.top) man flow |>
      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_assign o (mk_top T_int range) range)

  (* Declaration of a scalar pointer variable *)
  let declare_var v init range man flow =
    match var_scope v, init with
    (** Uninitialized global variable *)
    | Variable_global, None | Variable_file_static _, None ->
      (* The variable is initialized with NULL (C99 6.7.8.10) *)
      map_domain_env T_cur (Map.add v BaseSet.null) man flow |>
      Post.return

    (** Uninitialized local variable *)
    | Variable_local _, None | Variable_func_static _, None ->
      (* The value of the variable is undetermined (C99 6.7.8.10) *)
      map_domain_env T_cur (Map.add v BaseSet.invalid) man flow |>
      Post.return

    | _, Some (C_init_expr e) ->
      assign v e range man flow

    | _ -> assert false


  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_declaration(p,init) when is_c_pointer_type p.vtyp ->
      declare_var p init stmt.srange man flow |>
      Option.return

    | S_assign({ekind = E_var(p, _)}, q) when is_c_pointer_type p.vtyp ->
      assign p q stmt.srange man flow |>
      Option.return

    | S_add { ekind = E_var (p, _) } when is_c_pointer_type p.vtyp ->
      let o = mk_offset_var_expr p range in
      map_domain_env T_cur (Map.add p BaseSet.top) man flow |>
      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_add o range) |>
      Option.return

    | S_remove { ekind = E_var (p, _) } when is_c_pointer_type p.vtyp ->
      let flow1 = map_domain_env T_cur (Map.remove p) man flow in
      let o = mk_offset_var_expr p range in
      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_remove o range) flow1 |>
      Option.return

    | S_remove { ekind = E_addr addr } ->
      let block = BaseSet.PB_block (A addr) in
      let flow' = map_domain_env T_cur (fun a ->
          let a' = Map.map (fun base ->
              if not (BaseSet.mem block base) then
                base
              else
                let base' = BaseSet.add PB_invalid base in
                if addr.addr_mode = STRONG then
                  BaseSet.remove block base'
                else
                  base'
            ) a
          in
          a'
        ) man flow
      in
      Post.return flow' |>
      Option.return

    | S_expand({ekind = E_var (p, _)}, pl) when is_c_pointer_type p.vtyp ->
      let pl = List.map (function { ekind = E_var (q,_) } -> q | _ -> assert false) pl in
      let a = get_domain_env T_cur man flow in
      let pt = Map.find p a in
      let o = mk_offset_var_expr p range in
      let ool, flow =
        pl |> List.fold_left (fun (ool, flow) p' ->
            let oo = mk_offset_var_expr p' range in
            oo :: ool, map_domain_env T_cur (Map.add p' pt) man flow
          ) ([],flow)
      in
      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_expand o ool range) flow |>
      Option.return

    | S_rename({ekind = E_var (p1, _)}, {ekind = E_var (p2, _)})
      when is_c_pointer_type p1.vtyp &&
           is_c_pointer_type p2.vtyp
      ->
      let flow1 = map_domain_env T_cur (fun a ->
          let b1 = Map.find p1 a in
          let a' = Map.add p2 b1 a |>
                   Map.remove p1
          in
          a'
        ) man flow
      in
      let o1 = mk_offset_var_expr p1 range in
      let o2 = mk_offset_var_expr p2 range in
      man.exec_sub ~zone:(Universal.Zone.Z_u_num) (mk_rename o1 o2 range) flow1 |>
      Option.return

    | S_rename ({ekind = E_addr addr1}, {ekind = E_addr addr2}) ->
      let block1 = BaseSet.PB_block (A addr1) in
      let block2 = BaseSet.PB_block (A addr2) in
      let flow' = map_domain_env T_cur (fun a ->
          let a' = Map.map (fun bases ->
              if not (BaseSet.mem block1 bases) then bases
              else
                BaseSet.remove block1 bases |>
                BaseSet.add block2
            ) a
          in
          a'
        ) man flow
      in
      Post.return flow' |>
      Option.return

    | _ -> None


  (** {2 Handler of queries} *)
  (** ====================== *)

  let ask _ _ _ = None

  let refine channel man flow = Channel.return flow

end

let () =
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Domain)
