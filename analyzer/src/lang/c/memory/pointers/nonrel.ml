(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of pointer arithmetic *)

open Framework.Essentials
open Universal.Ast
open Ast
open Zone
open Common.Points_to

module Domain =
struct


  (** Domain lattice *)
  (** ============== *)

  module NR = Framework.Domains.Nonrel.Make(Framework.Value.LiftSimpleValue(Bases.Value))

  type t = NR.t

  let bottom = NR.bottom

  let top = NR.top

  let is_bottom = NR.is_bottom

  let subset = NR.subset

  let join = NR.join

  let meet = NR.meet

  let widen = NR.widen

  let print fmt a =
    Format.fprintf fmt "pointers: @[%a@]@\n"
      NR.print a


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
    import = []
  }


  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Flow.set_domain_env T_cur NR.top man flow |>
    OptionExt.return


  (** Evaluation of expressions *)
  (** ========================= *)

  (** Create the offset variable of a pointer *)
  let mk_offset_var (p:var) : var =
    {
      vname = p.vname ^ "_offset";
      vuid = p.vuid;
      vtyp = T_int
    }

  let mk_offset_var_expr (p:var) range : expr =
    mk_var (mk_offset_var p) range

  (** Pointer evaluations *)
  type ptr =
    | ADDROF of Common.Base.base * expr
    | EQ of var * expr
    | FUN of c_fundec
    | NULL
    | INVALID

  (**Compute the offset of a pointer given its pointer evaluation *)
  let add_offset ptr o t range =
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

  (** Get the pointer variable and the offset of a pointer expression *)
  let rec eval_pointer exp : ptr =
    let open Bases.Value in
    match ekind exp with
    | E_constant(C_int n) when Z.equal n Z.zero ->
      NULL

    | E_constant(C_c_invalid) ->
      INVALID

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

    | E_var (p, _) when is_c_pointer_type p.vtyp ->
      EQ (p, mk_zero exp.erange)

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

    | _ -> panic_at exp.erange "eval_base_offset: %a not supported" pp_expr exp


  let eval_points_to exp man flow =
    Some (
      let ptr = eval_pointer exp in

      match ptr with
      | ADDROF (base, offset) ->
        Eval.singleton (mk_c_points_to_bloc base offset exp.erange) flow

      | EQ (p, offset) ->
        let offset' = mk_binop (mk_var (mk_offset_var p) exp.erange) O_plus offset exp.erange in
        let bases = NR.find p (Flow.get_domain_env T_cur man flow) in
        let el = Bases.Value.fold (fun b acc ->
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
        Eval.join_list el

      | FUN f ->
        Eval.singleton (mk_c_points_to_fun f exp.erange) flow

      | NULL ->
        Eval.singleton (mk_c_points_to_null exp.erange) flow

      | INVALID ->
        Eval.singleton (mk_c_points_to_invalid exp.erange) flow
    )

  let rec eval_pointer_compare exp man flow =
    match ekind exp with
    | E_var (p, _) when is_c_pointer_type p.vtyp ->
      man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num)
        (mk_not
           (mk_binop exp O_eq (mk_zero exp.erange) exp.erange)
           exp.erange
        )
        flow |>
      Eval.return

    | E_binop(O_eq, e1, e2) when is_c_pointer_type e1.etyp ||
                                 is_c_pointer_type e2.etyp
      ->
      let p1 = eval_pointer e1 in
      let p2 = eval_pointer e2 in

      let get_pointer_info (p:ptr) : (Bases.Value.t * expr option * var option) =
        match p with
        | ADDROF (b, o) ->
          Bases.Value.block b, Some o, None

        | EQ(q, o) ->
          let b = Flow.get_domain_env T_cur man flow |>
                  NR.find q
          in
          b, Some o, Some q

        | NULL ->
          Bases.Value.null, None, None

        | INVALID ->
          Bases.Value.invalid, None, None

        | FUN _ ->
          panic_at exp.erange "eval_pointer_compare: function pointers not supported"
      in

      let b1, o1, v1 = get_pointer_info p1 in
      let b2, o2, v2 = get_pointer_info p2 in

      let filter_offsets b =
        match v1, o1, v2, o2 with
        | Some v1, Some o1, Some v2, Some o2 ->
          let e1 = mk_binop (mk_offset_var_expr v1 exp.erange) O_plus o1 exp.erange ~etyp:T_int in
          let e2 = mk_binop (mk_offset_var_expr v2 exp.erange) O_plus o2 exp.erange ~etyp:T_int in
          let cond = mk_binop e1 O_eq e2 exp.erange in
          if b then man.exec ~zone:Universal.Zone.Z_u_num (mk_assume cond exp.erange) flow
          else man.exec ~zone:Universal.Zone.Z_u_num (mk_assume (mk_not cond exp.erange) exp.erange) flow

        | Some v, Some o, None, Some oo
        | None, Some oo, Some v, Some o ->
          let e1 = mk_binop (mk_offset_var_expr v exp.erange) O_plus o exp.erange ~etyp:T_int in
          let cond = mk_binop e1 O_eq oo exp.erange in
          if b then man.exec ~zone:Universal.Zone.Z_u_num (mk_assume cond exp.erange) flow
          else man.exec ~zone:Universal.Zone.Z_u_num (mk_assume (mk_not cond exp.erange) exp.erange) flow

        | None, Some o1, None, Some o2 ->
          let cond = mk_binop o1 O_eq o2 exp.erange in
          if b then man.exec ~zone:Universal.Zone.Z_u_num (mk_assume cond exp.erange) flow
          else man.exec ~zone:Universal.Zone.Z_u_num (mk_assume (mk_not cond exp.erange) exp.erange) flow

        | _ -> flow

      in

      let assign_base v b man flow =
        match v with
        | None -> flow
        | Some vv -> Flow.map_domain_env T_cur (NR.add vv b) man flow
      in

      let btrue1, btrue2 = Bases.Value.compare () O_eq b1 b2 true in
      let bfalse1, bfalse2 = Bases.Value.compare () O_ne b1 b2 true in

      let btrue = Bases.Value.meet () btrue1 btrue2 in
      let bfalse = Bases.Value.meet () bfalse1 bfalse2 in

      let evl = match Bases.Value.is_bottom btrue,
                      Bases.Value.is_bottom bfalse
        with
        | true, true ->
          [Eval.empty_singleton flow]

        | false, true ->
          let ftrue = filter_offsets true |>
                      assign_base v1 btrue1 man |>
                      assign_base v2 btrue2 man
          in
          [Eval.singleton (mk_one exp.erange) ftrue]

        | true, false ->
          let ffalse = filter_offsets false |>
                       assign_base v1 bfalse1 man |>
                       assign_base v2 bfalse2 man
          in
          [Eval.singleton (mk_zero exp.erange) ffalse]

        | false, false ->
          let ftrue = filter_offsets true |>
                      assign_base v1 btrue1 man |>
                      assign_base v2 btrue2 man
          in
          let evl1 = Eval.singleton (mk_one exp.erange) ftrue in

          let ffalse = filter_offsets false |>
                       assign_base v1 bfalse1 man |>
                       assign_base v2 bfalse2 man
          in
          let evl2 = Eval.singleton (mk_zero exp.erange) ffalse in

          [evl1; evl2]
      in
      Eval.join_list evl |>
      Eval.return

    | E_binop(O_minus, e1, e2) when is_c_pointer_type e1.etyp &&
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
          NR.add p Bases.Value.null
        ) man flow
      in
      Post.return flow'

    | S_assign({ekind = E_var(p, mode)}, q) when is_c_pointer_type p.vtyp ->
      let o = mk_offset_var p in
      let ptr = eval_pointer q in
      let flow' =
        match ptr with
        | ADDROF (b, offset) ->
          let flow' = Flow.map_domain_env T_cur (NR.add p (Bases.Value.block b)) man flow in
          man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign (mk_var o range) offset range) flow'

        | EQ (q, offset) ->
          let flow' = Flow.map_domain_env T_cur (fun a -> NR.add p (NR.find q a) a) man flow in
          let qo = mk_offset_var q in
          man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign (mk_var o range) (mk_binop (mk_var qo range) O_plus offset range) range) flow'

        | FUN f ->
          Flow.map_domain_env T_cur (NR.add p (Bases.Value.bfun f)) man flow

        | INVALID ->
          Flow.map_domain_env T_cur (NR.add p Bases.Value.invalid) man flow

        | NULL ->
          Flow.map_domain_env T_cur (NR.add p Bases.Value.null) man flow
      in
      Post.return flow'

    | S_add_var(p) when is_c_pointer_type p.vtyp ->
      let flow1 = Flow.map_domain_env T_cur (NR.add p Bases.Value.top) man flow in
      let o = mk_offset_var p in
      let flow2 = man.exec ~zone:(Universal.Zone.Z_u_num) (mk_add_var o range) flow1 in
      Post.return flow2

    | S_remove_var(p) when is_c_pointer_type p.vtyp ->
      let flow1 = Flow.map_domain_env T_cur (NR.remove p) man flow in
      let o = mk_offset_var p in
      let flow2 = man.exec ~zone:(Universal.Zone.Z_u_num) (mk_remove_var o range) flow1 in
      Post.return flow2

    | S_expand(p, pl) when is_c_pointer_type p.vtyp ->
      let a = Flow.get_domain_env T_cur man flow in
      let pt = NR.find p a in
      let o = mk_offset_var p in
      let flow =
        pl |> List.fold_left (fun flow pp ->
            let oo = mk_offset_var pp in
            Flow.map_domain_env T_cur (NR.add pp pt) man flow |>
            man.exec ~zone:(Universal.Zone.Z_u_num) (mk_expand o [oo] range)
          ) flow
      in
      Post.return flow

    | _ -> None

  (** Handler of queries *)
  (** ================== *)

  let ask _ _ _ = None

end

let () =
  Framework.Domain.register_domain (module Domain);
  ()
