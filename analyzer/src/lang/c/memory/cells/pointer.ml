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
open Base
open Cell
open Zone


module Domain =
struct

  (** Lattice definition *)
  (** ================== *)

  module PointerBaseSet = Framework.Lattices.Powerset.Make(PointerBase)

  (* An abstract element is a map from pointer cells to a set of pointed bases *)
  include Framework.Lattices.Total_map.Make(Cell)(PointerBaseSet)

  let is_bottom _ = false

  let widening = join

  let print fmt a =
    Format.fprintf fmt "ptr: @[%a@]@\n"
      print a


  (** Utility functions *)
  (** ================= *)

  let add_with_mode annot p pb mode a =
    let a' = add p pb a in
    match mode with
    | STRONG (* | EXPAND *) -> a'
    | WEAK -> join annot a a'

  let points_to_fun annot p f ?(mode=STRONG) a =
    add_with_mode annot p (PointerBaseSet.singleton (PB_fun f)) mode a

  let points_to_base annot p b ?(mode=STRONG) a =
    add_with_mode annot p (PointerBaseSet.singleton (PB_var b)) mode a

  let points_to_var annot p v ?(mode=STRONG) a =
    (* (match v.vkind with V_orig -> () | _ -> assert false); *)
    add_with_mode annot p (PointerBaseSet.singleton (PB_var (V v))) mode a

  let points_to_null annot p ?(mode=STRONG) a =
    add_with_mode annot p (PointerBaseSet.singleton PB_null) mode a

  let points_to_invalid annot p ?(mode=STRONG) a =
    add_with_mode annot p (PointerBaseSet.singleton PB_invalid) mode a

  let points_to_base =
    function
    | P_fun f -> PB_fun f
    | P_var (b, _, _) -> PB_var b
    | P_null -> PB_null
    | P_invalid -> PB_invalid


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
    export = [Z_c_cell];
    import = [Universal.Zone.Z_u_num];
  }


  let eval_interface = {
    export = [
      Z_c, Z_c_points_to_cell;
      Z_c, Z_c_points_to_fun;
      Z_c, Z_c_scalar_num
    ];
    import = [Z_c, Z_c_cell]
  }


  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Flow.set_domain_env T_cur top man flow |>
    Option.return


  (** Evaluation of expressions *)
  (** ========================= *)

  let mk_offset_var p =
    let v = cell_to_var p in
    {
      vname = v.vname ^ "_offset";
      vuid = v.vuid;
      vtyp = T_int
    }

  let rec eval zone exp man flow =
    let range = exp.erange in
    match ekind exp with
    | _
      when sat_zone2 zone (Z_c, Z_c_points_to_cell) ->
      begin
        eval_points_to exp man flow |> Eval.bind @@ fun p flow ->
        Eval.singleton (mk_expr (E_c_points_to p) range) flow
      end
      |>
      Option.return

    | _
      when sat_zone2 zone (Z_c, Z_c_points_to_fun) ->
      begin
        eval_points_to exp man flow |> Eval.bind @@ fun p flow ->
        match p with
        | P_fun f ->
          let exp' = mk_expr (E_c_function f) ~etyp:(T_c_function None) range in
          Eval.singleton exp' flow
        | _ -> assert false
      end
      |>
      Option.return

    | E_binop(O_eq, p, q)
      when is_c_pointer_type p.etyp
        && is_c_pointer_type q.etyp
        && sat_zone2 zone (any_zone, Z_c_scalar_num)
      ->
      begin
        eval_points_to p man flow |> Eval.bind @@ fun p flow ->
        eval_points_to q man flow |> Eval.bind @@ fun q flow ->
        match p, q with
        | P_var (base1, offset1, t1), P_var (base2, offset2, t2) ->
          if compare_base base1 base2 <> 0 || compare (remove_typedef t1) (remove_typedef t2) <> 0 then
            Eval.singleton (mk_zero range) flow
          else
            Eval.assume
              (mk_binop offset1 O_eq offset2 range ~etyp:T_int)
              ~fthen:(fun true_flow -> Eval.singleton (mk_one range) true_flow)
              ~felse:(fun false_flow -> Eval.singleton (mk_zero range) false_flow)
              ~fboth:(fun _ _ -> Eval.singleton (mk_int_interval 0 1 range) flow)
              man flow

        | P_null, P_null -> Eval.singleton (mk_one range) flow

        | P_invalid, _ | _, P_invalid ->
          Eval.singleton (mk_int_interval 0 1 range) flow (* FIXME: maybe detect an error here? *)

        | _ ->
          Eval.singleton (mk_zero range) flow
      end
      |> Option.return

    | E_binop(O_ne, p, q)
      when is_c_pointer_type p.etyp
        && is_c_pointer_type q.etyp
        && sat_zone2 zone (any_zone, Z_c_scalar_num)
      ->
      begin
        eval_points_to p man flow |> Eval.bind @@ fun p flow ->
        eval_points_to q man flow |> Eval.bind @@ fun q flow ->
        match p, q with
        | P_var (base1, offset1, t1), P_var (base2, offset2, t2) ->
          if compare_base base1 base2 <> 0 || compare (remove_typedef t1) (remove_typedef t2) <> 0 then
            Eval.singleton (mk_one range) flow
          else
            Eval.assume
              (mk_binop offset1 O_ne offset2 range ~etyp:T_int)
              ~fthen:(fun true_flow -> Eval.singleton (mk_one range) true_flow)
              ~felse:(fun false_flow -> Eval.singleton (mk_zero range) false_flow)
              ~fboth:(fun _ _ -> Eval.singleton (mk_int_interval 0 1 range) flow)
              man flow

        | P_null, P_null -> Eval.singleton (mk_zero range) flow

        | P_invalid, _ | _, P_invalid ->
          Eval.singleton (mk_int_interval 0 1 range) flow (* FIXME: maybe detect an error here? *)

        | _ -> Eval.singleton (mk_one range) flow
      end
      |>
      Option.return

    | E_binop(O_minus, p, q)
      when is_c_pointer_type p.etyp
        && is_c_pointer_type q.etyp
        && sat_zone2 zone (any_zone, Z_c_scalar_num)
      ->
      panic_at range "pointer.eval: %a not yet supported" pp_expr exp

    | _ -> None


  and eval_points_to exp man flow : ('a, points_to) evl =
    let range = erange exp in
    match ekind exp with
    | E_constant (C_int n) when Z.equal n Z.zero ->
      Eval.singleton P_null flow

    | E_constant C_c_invalid ->
      Eval.singleton P_invalid flow

    | E_constant (C_c_string (s, _)) ->
      Eval.singleton (P_var (S s, mk_zero range, s8)) flow

    | E_addr addr ->
      Eval.singleton (P_var (A addr, mk_int 0 range, T_c_void)) flow

    | E_c_address_of e ->
      man.eval ~zone:(Z_c, Z_c_cell) e flow |>
      Eval.bind @@ fun e flow ->
      let rec aux e =
        match ekind e with
        | E_c_cell(c, _) ->
          let b, o, t = extract_cell_info c in
          Eval.singleton (P_var (b, o range, t)) flow

        | E_c_function f ->
          Eval.singleton (P_fun f) flow

        | E_c_cast(e', _) ->
          begin
            aux e' |> Eval.bind @@ fun e' flow ->
            match e' with
            | P_var(b, o, _) -> Eval.singleton (P_var(b, o, e.etyp)) flow
            | _ -> Eval.singleton e' flow
          end

        | _ -> assert false
      in
      aux e

    | E_c_function f ->
      Eval.singleton (P_fun f) flow

    | E_binop(O_plus, e1, e2) ->
      begin
        let p, n =
          if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp then e1, e2
          else e2, e1
        in
        eval_points_to p man flow |>
        Eval.bind @@ fun pt flow ->
        match pt with
        | P_var (b, o, t) ->
          let size = sizeof_type t in
          let pt = P_var (b, (mk_binop o O_plus (mk_binop n O_mult (mk_z size range) range ~etyp:T_int) range ~etyp:T_int), t) in
          Eval.singleton pt flow

        | P_null ->
          Eval.singleton P_null flow

        | P_invalid ->
          Eval.singleton P_invalid flow

        | P_fun f ->
          assert false
      end

    | E_c_cast(e, _) ->
      begin
        eval_points_to e man flow |>
        Eval.bind @@ fun pt flow ->
        match pt with
        | P_var (b, o, _) -> Eval.singleton (P_var(b, o, under_pointer_type exp.etyp)) flow
        | _ -> Eval.singleton pt flow
      end

    | _ ->
      man.eval ~zone:(Z_c, Z_c_cell) exp flow |>
      Eval.bind @@ fun e flow ->
      match ekind e with
      | E_c_cell(c, _) when cell_type c |> is_c_pointer_type ->
        let a = Flow.get_domain_env T_cur man flow in
        let bases = find c a in
        let evls = PointerBaseSet.fold (fun pb acc ->
            let pt =
              match pb with
              | PB_var b -> P_var (b, mk_var (mk_offset_var c) exp.erange, under_pointer_type (cell_type c))
              | PB_fun f -> P_fun f
              | PB_invalid -> P_invalid
              | PB_null -> P_null
            in
            let flow' = Flow.map_domain_env T_cur (add c (PointerBaseSet.singleton pb)) man flow in
            (Eval.singleton pt flow') :: acc
          ) bases []
        in
        if List.length evls = 0
        then Eval.empty_singleton flow
        else Eval.join_list evls

      | E_c_cell(c, _) when cell_type c |> is_c_array_type ->
        let b, o, t = extract_cell_info c in
        Eval.singleton (P_var (b, o exp.erange, under_array_type t)) flow

      | E_c_function f ->
        Eval.singleton (P_fun f) flow

      | _ -> assert false


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
      begin
        man.eval ~zone:(Z_c, Z_c_cell) (mk_var p stmt.srange) flow |>
        Post.bind man @@ fun p flow ->
        match ekind p with
        | E_c_cell(c, mode) ->
          Flow.map_domain_env T_cur (points_to_null (Flow.get_all_annot flow) c) man flow |>
          Post.of_flow

        | _ -> assert false
      end
      |>
      Option.return

    | S_assign({ekind = E_c_cell(p, mode)}, q) when cell_type p |> is_c_pointer_type ->
      begin
        eval_points_to q man flow |>
        Post.bind man @@ fun pt flow ->
        let b = points_to_base pt |>
                PointerBaseSet.singleton
        in
        let flow1 = Flow.map_domain_env T_cur (add_with_mode (Flow.get_all_annot flow) p b mode) man flow in
        let flow2 =
          match pt with
          | P_var (_, offset, _) ->
            let o = mk_offset_var p in
            man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign (mk_var o range) offset range) flow1

          (* FIXME: This case is ugly: assigning a zero offset to a
             null pointer avoids loosing information about offsets after joins. *)
          | P_null ->
            let o = mk_offset_var p in
            man.exec ~zone:(Universal.Zone.Z_u_num) (mk_assign (mk_var o range) (mk_zero range) range) flow1
          | _ -> flow1
        in
        Post.of_flow flow2
      end
      |>
      Option.return

    | S_c_remove_cell(p) when cell_type p |> is_c_pointer_type ->
      let flow1 = Flow.map_domain_env T_cur (remove p) man flow in
      let o = mk_offset_var p in
      let flow2 = man.exec ~zone:(Universal.Zone.Z_u_num) (mk_remove_var o range) flow1 in
      Post.of_flow flow2 |>
      Option.return

    | _ -> None

  (** Handler of queries *)
  (** ================== *)

  let ask _ _ _ = None

end

let () =
  Framework.Domain.register_domain (module Domain);
  ()
