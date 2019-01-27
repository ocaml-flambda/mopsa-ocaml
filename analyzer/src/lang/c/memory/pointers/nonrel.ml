(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational abstract for C pointers *)

open Mopsa
open Universal.Ast
open Ast
open Zone
open Common.Points_to

module Domain =
struct


  (** {2 Domain lattice} *)
  (** ================== *)

  (** An abstract element is a partial map from pointer variables
      to a set of bases *)
  include Framework.Domains.Nonrel.Make(Framework.Value.LiftSimpleValue(Bases))


  (** {2 Domain identification} *)
  (** ========================= *)

  type _ domain += D_c_cells_pointer : t domain
  let id = D_c_cells_pointer
  let name = "c.memory.cells.pointer"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cells_pointer -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** {2 Zoning interface} *)
  (** ==================== *)

  let exec_interface = {
    export = [Z_c_scalar];
    import = [Universal.Zone.Z_u_num];
  }


  let eval_interface = {
    export = [
      Z_c_scalar, Z_c_points_to;
      Z_c_scalar, Universal.Zone.Z_u_num
    ];
    import = [
      Z_c_scalar, Universal.Zone.Z_u_num
    ]
  }


  (** {2 Initialization} *)
  (** ================== *)

  let init prog man flow =
    Flow.set_domain_env T_cur empty man flow |>
    Flow.without_callbacks |>
    OptionExt.return


  (** {2 Utility functions for evaluations *)
  (** ==================================== *)

  (** Create the offset variable of a pointer *)
  let mk_offset_var (p:var) : var =
    let vname = p.org_vname ^ "_offset" in
    let uniq = vname ^ ":" ^ (string_of_int p.vuid) in
    mkv vname uniq p.vuid T_int

  let mk_offset_var_expr (p:var) range : expr =
    mk_var (mk_offset_var p) ~mode:STRONG range

  (** Pointer evaluations *)
  type ptr =
    | ADDROF of Common.Base.base * expr
    | EQ of var * expr
    | FUN of c_fundec
    | NULL
    | INVALID

  (** Advance the offset of a pointer evaluation *)
  let advance_offset (op:operator) (ptr:ptr) (o:expr) t range : ptr =
    let size = sizeof_type t in
    let advance oo =
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


  (* Get the base and eventual pointer offset from a pointer evaluation *)
  let get_pointer_info (p:ptr) man flow : (Bases.t * expr option * var option) =
    match p with
    | ADDROF (b, o) -> debug "addof"; Bases.block b, Some o, None

    | EQ(q, o) -> debug "eq";
      let b = Flow.get_domain_env T_cur man flow |>
              find q
      in
      b, (if Bases.mem_block b then Some o else None), Some q

    | NULL -> debug "null";
      Bases.null, None, None

    | INVALID -> debug "invalid";
      Bases.invalid, None, None

    | FUN _ -> panic "eval_pointer_compare: function pointers not supported"


  (* Set base of an optional pointer info *)
  let set_base v b man flow =
    match v with
    | None -> flow
    | Some vv -> Flow.map_domain_env T_cur (add vv b) man flow

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
  let rec eval_pointer exp : ptr =
    let open Bases in
    match ekind exp with
    | E_constant(C_int n) when Z.equal n Z.zero ->
      NULL

    | E_constant(C_c_invalid) ->
      INVALID

    | E_addr (addr) ->
      ADDROF(A addr, mk_zero exp.erange)

    | E_c_deref { ekind = E_c_address_of e } ->
      eval_pointer e

    | E_c_address_of { ekind = E_c_deref p } ->
      eval_pointer p

    | E_c_cast (e, _) ->
      eval_pointer e

    | E_c_function f ->
      FUN f

    | E_constant (C_c_string (s, _)) ->
      ADDROF(S s, mk_zero exp.erange)

    | E_var (a, _) when is_c_array_type a.vtyp ->
      ADDROF(V a, mk_zero exp.erange)

    | E_c_deref a when is_c_array_type (under_type a.etyp) ->
      eval_pointer a

    | E_c_address_of { ekind = E_var (v, _) } ->
      ADDROF (V v, mk_zero exp.erange)

    | E_c_address_of { ekind = E_c_function f } ->
      FUN f

    | E_binop(O_plus | O_minus as op, e1, e2) ->
      let p, i =
        if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp
        then e1, e2
        else e2, e1
      in
      let ptr  = eval_pointer p in
      advance_offset op ptr i (under_type p.etyp) exp.erange

    | E_var (v, STRONG) when is_c_pointer_type v.vtyp ->
      EQ (v, mk_zero exp.erange)

    | x when is_c_int_type exp.etyp ->
      ADDROF(Common.Base.Z, exp)

    | _ -> panic_at exp.erange "eval_base_offset: %a not supported" pp_expr exp


  (** Evaluation of points-to information *)
  let eval_points_to exp man flow =
    Some (
      let ptr = eval_pointer exp in

      match ptr with
      | ADDROF (base, offset) ->
        Eval.singleton (mk_c_points_to_bloc base offset exp.erange) flow

      | EQ (p, offset) ->
        let offset' = mk_binop (mk_offset_var_expr p exp.erange) O_plus offset ~etyp:T_int exp.erange in
        let bases = find p (Flow.get_domain_env T_cur man flow) in
        if Bases.is_top bases then
          Eval.singleton (mk_c_points_to_top exp.erange) flow
        else
          let el = Bases.fold (fun b acc ->
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
    )

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
      let b1', b2' = Bases.compare () O_eq b1 b2 true in

      let flow' = set_base v1 b1' man flow |>
                  set_base v2 b2' man
      in

      (* Refine offsets in case p or q may point to a block *)
      if Bases.mem_block b1' && Bases.mem_block b2' then
        man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) (compare_cond O_eq v1 o1 v2 o2 exp.erange) flow' |>
        Eval.return
      else
        (* Remove offsets in other case *)
        let remove_offset v b range man flow =
          match v with
          | None -> flow
          | Some vv ->
            if Bases.mem_block b then
              man.exec (mk_remove (mk_offset_var_expr vv exp.erange) exp.erange) flow
            else
              flow
        in
        let flow' = remove_offset v1 b1 exp.erange man flow' |>
                    remove_offset v2 b2 exp.erange man
        in
        Eval.singleton (mk_one exp.erange) flow' |>
        Eval.return

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
      let b1eq, b2eq = Bases.compare () O_eq b1 b2 true in
      let b1ne, b2ne = Bases.compare () O_ne b1 b2 true in

      (* Case 1: different bases *)
      let case1 =
        if not (Bases.is_bottom b1ne) && not (Bases.is_bottom b2ne) then
          let flow' = set_base v1 b1ne man flow |>
                      set_base v2 b2ne man
          in
          [Eval.singleton (mk_one exp.erange) flow']
        else
          []
      in

      (* Case 2: same base => different offsets *)
      let case2 =
        if not (Bases.is_bottom b1eq) && not (Bases.is_bottom b2eq) then
          let flow' = set_base v1 b1eq man flow |>
                      set_base v2 b2eq man
          in
          [man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) (compare_cond O_ne v1 o1 v2 o2 exp.erange) flow']
        else
          []
      in
      Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow) |>
      Eval.return


    (* 𝔼⟦ p ⟧ *)
    | E_var(p, _) when is_c_pointer_type p.vtyp ->
      let exp' = mk_binop exp O_ne (mk_zero exp.erange) exp.erange in

      eval_pointer_compare exp' man flow

    (* 𝔼⟦ !p ⟧ *)
    | E_unop (O_log_not, ({ekind = E_var (p, _)} as e)) when is_c_pointer_type p.vtyp ->
      let exp' = mk_binop e O_eq (mk_zero exp.erange) exp.erange in

      eval_pointer_compare exp' man flow

    (* 𝔼⟦ ptr_valid(p) ⟧ *)
    | Stubs.Ast.E_stub_builtin_call( PTR_VALID, p) ->
      (* A valid pointer is not NULL nor INVALID and its offset is
         within [0, sizeof(base) - sizeof(under_type t) [ *)

      (* Evaluate the pointed memory block *)
      eval_points_to p man flow |>
      OptionExt.lift @@ Eval.bind @@ fun pt flow ->

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
      if compare_typ (under_type p1.etyp) (under_type p2.etyp) != 0
      then panic_at exp.erange
          "%a: pointers do not point to the same type"
          pp_expr exp
      ;

      (* Evaluate pointers *)
      let b1, o1, v1 = get_pointer_info (eval_pointer p1) man flow in
      let b2, o2, v2 = get_pointer_info (eval_pointer p2) man flow in

      (* Check if the bases are the same *)
      (* Compute new bases *)
      let b1eq, b2eq = Bases.compare () O_eq b1 b2 true in
      let b1ne, b2ne = Bases.compare () O_ne b1 b2 true in

      (* Size of a pointed element *)
      let elem_size = under_type p1.etyp |> sizeof_type in

      (* Case 1 : same base => return difference of offset *)
      let case1 =
        if not (Bases.is_bottom b1eq) &&
           not (Bases.is_bottom b2eq) &&
           Bases.mem_block b1eq &&
           Bases.mem_block b2eq
        then
          let flow' = set_base v1 b1eq man flow |>
                      set_base v2 b2eq man
          in
          let o1 = offset_expr v1 o1 exp.erange in
          let o2 = offset_expr v2 o2 exp.erange in
          let e = div (sub o1 o2 exp.erange) (mk_z elem_size exp.erange) exp.erange in
          [Eval.singleton e flow']
        else
          []
      in

      (* Case 2: different base => raise an alarm *)
      let case2 =
        if not (Bases.is_bottom b1ne) &&
           not (Bases.is_bottom b2ne)
        then
          let flow' = set_base v1 b1ne man flow |>
                      set_base v2 b2ne man
          in
          let flow'' = raise_alarm Alarms.AIllegalPointerDiff ~bottom:true exp.erange man flow' in
          [Eval.empty_singleton flow'']
        else
          []
      in

      Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow) |>
      Eval.return

    | _ -> None


  let eval zone exp man flow =
    match zone with
    | Z_c_scalar, Z_c_points_to -> eval_points_to exp man flow
    | Z_c_scalar, Universal.Zone.Z_u_num -> eval_pointer_compare exp man flow
    | _ -> None



  (** {2 Computation of post-conditions} *)
  (** ================================== *)

  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_declaration(p) when is_c_pointer_type p.vtyp ->
      let flow' = Flow.map_domain_env T_cur (
          add p Bases.null
        ) man flow
      in
      Post.return flow'

    | S_assign({ekind = E_var(p, _)}, q) when is_c_pointer_type p.vtyp ->
      let o = mk_offset_var_expr p range in
      let ptr = eval_pointer q in
      let flow' =
        match ptr with
        | ADDROF (b, offset) ->
          let flow' = Flow.map_domain_env T_cur (add p (Bases.block b)) man flow in
          man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign o offset range) flow'

        | EQ (q, offset) ->
          let flow' = Flow.map_domain_env T_cur (fun a -> add p (find q a) a) man flow in
          (* Assign offset only if q points to a block *)
          if Flow.test_domain_env T_cur (fun a -> find q a |> Bases.mem_block) man flow then
            let qo = mk_offset_var_expr q range in
            man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign o (mk_binop qo O_plus offset ~etyp:T_int range) range) flow'
          else
            man.exec ~zone:(Universal.Zone.Z_u_num) (mk_remove o range) flow'

        | FUN f ->
          Flow.map_domain_env T_cur (add p (Bases.bfun f)) man flow |>
          man.exec ~zone:(Universal.Zone.Z_u_num) (mk_remove o range)

        | INVALID ->
          Flow.map_domain_env T_cur (add p Bases.invalid) man flow  |>
          man.exec ~zone:(Universal.Zone.Z_u_num) (mk_remove o range)

        | NULL ->
          Flow.map_domain_env T_cur (add p Bases.null) man flow |>
          man.exec ~zone:(Universal.Zone.Z_u_num) (mk_remove o range)
      in
      Post.return flow'

    | S_add { ekind = E_var (p, _) } when is_c_pointer_type p.vtyp ->
      let o = mk_offset_var_expr p range in
      Flow.map_domain_env T_cur (add p Bases.top) man flow |>
      man.exec ~zone:(Universal.Zone.Z_u_num) (mk_add o range) |>
      Post.return

    | S_remove { ekind = E_var (p, _) } when is_c_pointer_type p.vtyp ->
      let flow1 = Flow.map_domain_env T_cur (remove p) man flow in
      let o = mk_offset_var_expr p range in
      let flow2 = man.exec ~zone:(Universal.Zone.Z_u_num) (mk_remove o range) flow1 in
      Post.return flow2

    | S_remove { ekind = E_addr addr } ->
      let block = Bases.PB_block (A addr) in
      let flow' = Flow.map_domain_env T_cur (fun a ->
          let a' = map (fun base ->
              if not (Bases.mem block base) then
                base
              else
                let base' = Bases.add PB_invalid base in
                if addr.addr_mode = STRONG then
                  Bases.remove block base'
                else
                  base'
            ) a
          in
          a'
        ) man flow
      in
      Post.return flow'

    | S_expand({ekind = E_var (p, _)}, pl) when is_c_pointer_type p.vtyp ->
      let pl = List.map (function { ekind = E_var (q,_) } -> q | _ -> assert false) pl in
      let a = Flow.get_domain_env T_cur man flow in
      let pt = find p a in
      let o = mk_offset_var_expr p range in
      let ool, flow =
        pl |> List.fold_left (fun (ool, flow) p' ->
            let oo = mk_offset_var_expr p' range in
            oo :: ool, Flow.map_domain_env T_cur (add p' pt) man flow
          ) ([],flow)
      in
      man.exec ~zone:(Universal.Zone.Z_u_num) (mk_expand o ool range) flow |>
      Post.return

    | S_rename({ekind = E_var (p1, _)}, {ekind = E_var (p2, _)})
      when is_c_pointer_type p1.vtyp &&
           is_c_pointer_type p2.vtyp
      ->
      let flow1 = Flow.map_domain_env T_cur (fun a ->
          let b1 = find p1 a in
          let a' = add p2 b1 a |>
                   remove p1
          in
          a'
        ) man flow
      in
      let o1 = mk_offset_var_expr p1 range in
      let o2 = mk_offset_var_expr p2 range in
      let flow2 = man.exec ~zone:(Universal.Zone.Z_u_num) (mk_rename o1 o2 range) flow1 in
      Post.return flow2

    | S_rename ({ekind = E_addr addr1}, {ekind = E_addr addr2}) ->
      let block1 = Bases.PB_block (A addr1) in
      let block2 = Bases.PB_block (A addr2) in
      let flow' = Flow.map_domain_env T_cur (fun a ->
          let a' = map (fun bases ->
              if not (Bases.mem block1 bases) then bases
              else
                Bases.remove block1 bases |>
                Bases.add block2
            ) a
          in
          a'
        ) man flow
      in
      Post.return flow'

    | _ -> None


  (** {2 Handler of queries} *)
  (** ====================== *)

  let ask _ _ _ = None

end

let () =
  Framework.Domain.register_domain (module Domain);
  ()
