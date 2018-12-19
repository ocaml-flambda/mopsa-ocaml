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


  (** Domain lattice *)
  (** ============== *)

  module NR = Framework.Domains.Nonrel.Make(Framework.Value.LiftSimpleValue(Bases))

  type t = NR.t

  let bottom = NR.bottom

  let top = NR.top

  let is_bottom = NR.is_bottom

  let subset = NR.subset

  let join = NR.join

  let meet = NR.meet

  let widen = NR.widen

  let print = NR.print


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_cells_pointer : t domain
  let id = D_c_cells_pointer
  let name = "c.memory.cells.pointer"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cells_pointer -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning interface *)
  (** ================ *)

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


  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Flow.set_domain_env T_cur NR.top man flow |>
    OptionExt.return


  (** Evaluation of expressions *)
  (** ========================= *)

  (** Create the offset variable of a pointer *)
  let mk_offset_var (p:var primed) : var primed =
    primed_lift (fun pp ->
        {
          vname = pp.vname ^ "_offset";
          vuid = pp.vuid;
          vtyp = T_int
        }
      ) p

  let mk_offset_var_expr (p:var primed) range : expr =
    PrimedVar.to_expr (mk_offset_var p) STRONG range

  (** Pointer evaluations *)
  type ptr =
    | ADDROF of Common.Base.base * expr
    | EQ of var primed * expr
    | FUN of c_fundec
    | NULL
    | INVALID

  (** Add a pointer evaluation with an offset expression *)
  let add_offset (ptr:ptr) (o:expr) t range : ptr =
    let size = sizeof_type t in
    let add oo =
      mk_binop oo O_plus (mk_binop o O_mult (mk_z size range) range ~etyp:T_int) range ~etyp:T_int
    in
    match ptr with
    | ADDROF (b, oo) -> ADDROF (b, add oo)

    | EQ (p, oo) -> EQ (p, add oo)

    | NULL -> NULL

    | INVALID -> INVALID

    | FUN _ ->
      panic_at range
        "pointers.add_offset: pointer arithmetics on functions not supported"

  (** Static evaluation of a pointer base and offset *)
  let rec eval_pointer exp : ptr =
    let open Bases in
    match ekind exp with
    | E_constant(C_int n) when Z.equal n Z.zero ->
      NULL

    | E_constant(C_c_invalid) ->
      INVALID

    | E_addr addr ->
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

    | E_binop(O_plus, e1, e2) ->
      let p, i =
        if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp
        then e1, e2
        else e2, e1
      in
      let ptr  = eval_pointer p in
      add_offset ptr i (under_type p.etyp) exp.erange

    | x when PrimedVar.match_expr exp &&
             is_c_pointer_type exp.etyp ->
      EQ (PrimedVar.from_expr exp, mk_zero exp.erange)

    | _ -> panic_at exp.erange "eval_base_offset: %a not supported" pp_expr exp


  let eval_points_to exp man flow =
    Some (
      let ptr = eval_pointer exp in

      match ptr with
      | ADDROF (base, offset) ->
        Eval.singleton (mk_c_points_to_bloc base offset exp.erange) flow

      | EQ (p, offset) ->
        let offset' = mk_binop (mk_offset_var_expr p exp.erange) O_plus offset exp.erange in
        let bases = NR.find p (Flow.get_domain_env T_cur man flow) in
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

  let rec eval_pointer_compare exp man flow =
    (* Some utility functions *)

    (* Get the base and eventual pointer offset from a pointer evaluation *)
    let get_pointer_info (p:ptr) : (Bases.t * expr option * var primed option) =
      match p with
      | ADDROF (b, o) -> Bases.block b, Some o, None

      | EQ(q, o) ->
        let b = Flow.get_domain_env T_cur man flow |>
                NR.find q
        in
        b, Some o, Some q

      | NULL -> Bases.null, None, None

      | INVALID -> Bases.invalid, None, None

      | FUN _ -> panic_at exp.erange "eval_pointer_compare: function pointers not supported"
    in

    (* Set base of an optional pointer *)
    let set_base v b man flow =
      match v with
      | None -> flow
      | Some vv -> Flow.map_domain_env T_cur (NR.add vv b) man flow
    in

    (* Offset conditions of two equal pointers *)
    let offsets_cond op p1 o1 p2 o2 =
      match p1, o1, p2, o2 with
      | Some v1, Some o1, Some v2, Some o2 ->
        let e1 = mk_binop (mk_offset_var_expr v1 exp.erange) O_plus o1 exp.erange ~etyp:T_int in
        let e2 = mk_binop (mk_offset_var_expr v2 exp.erange) O_plus o2 exp.erange ~etyp:T_int in
        mk_binop e1 op e2 exp.erange

      | Some v, Some o, None, Some oo
      | None, Some oo, Some v, Some o ->
        let e1 = mk_binop (mk_offset_var_expr v exp.erange) O_plus o exp.erange ~etyp:T_int in
        mk_binop e1 op oo exp.erange

      | None, Some o1, None, Some o2 ->
        mk_binop o1 op o2 exp.erange

      | _ ->
        match op with
        | O_eq -> mk_one exp.erange
        | O_ne -> mk_zero exp.erange
        | _ -> assert false
    in

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

      let b1, o1, v1 = get_pointer_info p1 in
      let b2, o2, v2 = get_pointer_info p2 in

      (* Compute new bases *)
      let b1', b2' = Bases.compare () O_eq b1 b2 true in

      let flow' = set_base v1 b1' man flow |>
                  set_base v2 b2' man
      in

      man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) (offsets_cond O_eq v1 o1 v2 o2) flow' |>
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

      let b1, o1, v1 = get_pointer_info p1 in
      let b2, o2, v2 = get_pointer_info p2 in

      (* Compute new bases *)
      let b1eq, b2eq = Bases.compare () O_eq b1 b2 true in
      let b1ne, b2ne = Bases.compare () O_ne b1 b2 true in

      (* Case 1: different bases *)
      let case1 =
        if not (Bases.is_bottom b1ne) && not (Bases.is_bottom b2ne) then
          let flow' = set_base v1 b1ne man flow |>
                      set_base v2 b2ne man
          in
          debug "case 1";
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
          debug "case 2";
          [man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) (offsets_cond O_ne v1 o1 v2 o2) flow']
        else
          []
      in
      Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow) |>
      Eval.return


    (* 𝔼⟦ p ⟧ *)
    | p when is_c_pointer_type exp.etyp &&
             PrimedVar.match_expr exp
      ->
      let exp' = mk_binop exp O_ne (mk_zero exp.erange) exp.erange in

      eval_pointer_compare exp' man flow

    (* 𝔼⟦ !p ⟧ *)
    | E_unop (O_log_not, p) when is_c_pointer_type p.etyp &&
                                 PrimedVar.match_expr p
      ->
      let exp' = mk_binop exp O_eq (mk_zero exp.erange) exp.erange in

      eval_pointer_compare exp' man flow


    | E_binop(O_minus, e1, e2)
      when is_c_pointer_type e1.etyp &&
           is_c_pointer_type e2.etyp
      ->
      panic_at exp.erange "eval_pointer_compare: pointer difference not supported"

    | _ -> None


  let eval zone exp man flow =
    match zone with
    | Z_c_scalar, Z_c_points_to -> eval_points_to exp man flow
    | Z_c_scalar, Universal.Zone.Z_u_num -> eval_pointer_compare exp man flow
    | _ -> None



  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
      let flow' = Flow.map_domain_env T_cur (
          NR.add (unprimed p) Bases.null
        ) man flow
      in
      Post.return flow'

    | S_assign(p, q) when PrimedVar.match_expr p &&
                          is_c_pointer_type p.etyp ->
      let pp = PrimedVar.from_expr p in
      let o = mk_offset_var_expr pp range in
      let ptr = eval_pointer q in
      let flow' =
        match ptr with
        | ADDROF (b, offset) ->
          let flow' = Flow.map_domain_env T_cur (NR.add pp (Bases.block b)) man flow in
          man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign o offset range) flow'

        | EQ (q, offset) ->
          let flow' = Flow.map_domain_env T_cur (fun a -> NR.add pp (NR.find q a) a) man flow in
          let qo = mk_offset_var_expr q range in
          man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign o (mk_binop qo O_plus offset range) range) flow'

        | FUN f ->
          Flow.map_domain_env T_cur (NR.add pp (Bases.bfun f)) man flow

        | INVALID ->
          Flow.map_domain_env T_cur (NR.add pp Bases.invalid) man flow

        | NULL ->
          Flow.map_domain_env T_cur (NR.add pp Bases.null) man flow
      in
      Post.return flow'

    | S_add p when is_c_pointer_type p.etyp &&
                   PrimedVar.match_expr p ->
      let pp = PrimedVar.from_expr p in
      let flow1 = Flow.map_domain_env T_cur (NR.add pp Bases.top) man flow in
      let o = mk_offset_var_expr pp range in
      let flow2 = man.exec ~zone:(Universal.Zone.Z_u_num) (mk_add o range) flow1 in
      Post.return flow2

    | S_remove p when is_c_pointer_type p.etyp &&
                      PrimedVar.match_expr p ->
      let pp = PrimedVar.from_expr p in
      let flow1 = Flow.map_domain_env T_cur (NR.remove pp) man flow in
      let o = mk_offset_var_expr pp range in
      let flow2 = man.exec ~zone:(Universal.Zone.Z_u_num) (mk_remove o range) flow1 in
      Post.return flow2

    | S_expand(p, pl) when is_c_pointer_type p.etyp &&
                           PrimedVar.match_expr p &&
                           List.for_all PrimedVar.match_expr pl ->
      let pp = PrimedVar.from_expr p in
      let ppl = List.map PrimedVar.from_expr pl in
      let a = Flow.get_domain_env T_cur man flow in
      let pt = NR.find pp a in
      let o = mk_offset_var_expr pp range in
      let ool, flow =
        ppl |> List.fold_left (fun (ool, flow) pp' ->
            let oo = mk_offset_var_expr pp' range in
            oo :: ool, Flow.map_domain_env T_cur (NR.add pp' pt) man flow
          ) ([],flow)
      in
      man.exec ~zone:(Universal.Zone.Z_u_num) (mk_expand o ool range) flow |>
      Post.return

    | S_rename(p1, p2) when is_c_pointer_type p1.etyp &&
                            is_c_pointer_type p2.etyp &&
                            PrimedVar.match_expr p1 &&
                            PrimedVar.match_expr p2 ->
      let pp1 = PrimedVar.from_expr p1 in
      let pp2 = PrimedVar.from_expr p2 in
      let flow1 = Flow.map_domain_env T_cur (fun a ->
          let b1 = NR.find pp1 a in
          let a' = NR.add pp2 b1 a |>
                   NR.remove pp1
          in
          a'
        ) man flow
      in
      let o1 = mk_offset_var_expr pp1 range in
      let o2 = mk_offset_var_expr pp2 range in
      let flow2 = man.exec ~zone:(Universal.Zone.Z_u_num) (mk_rename o1 o2 range) flow1 in
      Post.return flow2


    | _ -> None

  (** Handler of queries *)
  (** ================== *)

  let ask _ _ _ = None

end

let () =
  Framework.Domain.register_domain (module Domain);
  ()
