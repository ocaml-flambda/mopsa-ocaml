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
    export = [Z_c_scalar, Z_c_points_to];
    import = []
  }


  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Flow.set_domain_env T_cur NR.top man flow |>
    OptionExt.return


  (** Evaluation of expressions *)
  (** ========================= *)

  let mk_offset_var p =
    {
      vname = p.vname ^ "_offset";
      vuid = p.vuid;
      vtyp = T_int
    }

  type ptr =
    | B of Common.Base.base * expr
    | P of var * expr
    | F of c_fundec

  let add_offset ptr o t range =
    let size = sizeof_type t in
    let add oo =
      mk_binop oo O_plus (mk_binop o O_mult (mk_z size range) range ~etyp:T_int) range ~etyp:T_int
    in
    match ptr with
    | B (b, oo) -> B (b, add oo)
    | P (p, oo) -> P (p, add oo)
    | _ -> assert false

  (** Get the pointer variable and the offset of a pointer expression *)
  let rec eval_pointer exp man flow : ('a, ptr) evl =
    let open Bases.Value in
    match ekind exp with
    | E_c_deref { ekind = E_c_address_of e } ->
      eval_pointer e man flow

    | E_c_address_of { ekind = E_c_deref p } ->
      eval_pointer p man flow

    | E_c_cast (e, _) ->
      eval_pointer e man flow

    | E_c_function f ->
      Eval.singleton (F f) flow

    | E_constant (C_c_string (s, _)) ->
      Eval.singleton (B(S s, mk_zero exp.erange)) flow

    | E_var (a, _) when is_c_array_type a.vtyp ->
      Eval.singleton (B(V a, mk_zero exp.erange)) flow

    | E_var (p, _) when is_c_pointer_type p.vtyp ->
      Eval.singleton (P (p, mk_zero exp.erange)) flow

    | E_c_address_of e ->
      begin match ekind e with
        | E_var (v, _) -> Eval.singleton (B (V v, mk_zero e.erange)) flow
        | E_c_function f -> Eval.singleton (F f) flow
        | _ -> panic_at exp.erange "eval_base_offset.eval_pointer: %a not supported" pp_expr exp
      end

    | E_binop(O_plus, e1, e2) ->
      let p, i =
        if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp
        then e1, e2
        else e2, e1
      in
      eval_pointer p man flow |>
      Eval.bind @@ fun ptr flow ->
      let ptr' = add_offset ptr i (under_type p.etyp) exp.erange in
      Eval.singleton ptr' flow

    | _ -> panic_at exp.erange "eval_base_offset: %a not supported" pp_expr exp


  let eval zone exp man flow =
    Some (
      eval_pointer exp man flow |>
      Eval.bind @@ fun ptr flow ->

      match ptr with
      | B (base, offset) ->
        Eval.singleton (mk_c_points_to_bloc base offset exp.erange) flow
      | P (p, offset) ->
        let offset' = mk_binop (mk_var (mk_offset_var p) exp.erange) O_plus offset exp.erange in
        let bases = NR.find p (Flow.get_domain_env T_cur man flow) in
        let el = Bases.Value.fold (fun b acc ->
            match b with
            | PB_block b -> Eval.singleton (mk_c_points_to_bloc b offset' exp.erange) flow :: acc
            | PB_fun f -> Eval.singleton (mk_c_points_to_fun f exp.erange) flow :: acc
            | PB_null -> Eval.singleton (mk_c_points_to_null exp.erange) flow :: acc
            | PB_invalid -> Eval.singleton (mk_c_points_to_invalid exp.erange) flow :: acc
          ) bases []
        in
        Eval.join_list el

      | F f ->
        Eval.singleton (mk_c_points_to_fun f exp.erange) flow
    )



  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
      let a = Flow.get_domain_env T_cur man flow in
      let a' = NR.add p Bases.Value.null a in
      let flow' = Flow.set_domain_env T_cur a' man flow in
      Post.return flow'

    | S_assign({ekind = E_var(p, mode)}, q) when is_c_pointer_type p.vtyp ->
      let o = mk_offset_var p in

      Some (
        eval_pointer q man flow |>
        Post.bind man @@ fun ptr flow ->

        let flow' =
          match ptr with
          | B (b, offset) ->
            let flow' = Flow.map_domain_env T_cur (NR.add p (Bases.Value.block b)) man flow in
            man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign (mk_var o range) offset range) flow'

          | P (q, offset) ->
            let flow' = Flow.map_domain_env T_cur (fun a -> NR.add p (NR.find q a) a) man flow in
            let qo = mk_offset_var q in
            man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign (mk_var o range) (mk_binop (mk_var qo range) O_plus offset range) range) flow'

          | F f -> assert false
        in
        Post.of_flow flow'
      )

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
