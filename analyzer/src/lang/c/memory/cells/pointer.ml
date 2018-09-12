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


module Domain =
struct

  (** Lattice definition *)
  (** ================== *)

  module PointerBaseSet = Framework.Lattices.Powerset.Make(PointerBase)

  (* An abstract element is a map from pointer cells to a set of pointed bases *)
  include Framework.Lattices.Total_map.Make(Cell)(PointerBaseSet)


  (** Utility functions *)
  (** ================= *)

  let add annot p pb mode a =
    let a' = add p pb a in
    match mode with
    | STRONG | EXPAND -> a'
    | WEAK -> join annot a a'

  let print fmt a =
    Format.fprintf fmt "ptr: @[%a@]@\n"
      print a

  let points_to_fun annot p f ?(mode=STRONG) a =
    add annot p (PointerBaseSet.singleton (PB_fun f)) mode a

  let points_to_base annot p b ?(mode=STRONG) a =
    add annot p (PointerBaseSet.singleton (PB_var b)) mode a

  let points_to_var annot p v ?(mode=STRONG) a =
    (* (match v.vkind with V_orig -> () | _ -> assert false); *)
    add annot p (PointerBaseSet.singleton (PB_var (V v))) mode a

  let points_to_null annot p ?(mode=STRONG) a =
    add annot p (PointerBaseSet.singleton PB_null) mode a

  let points_to_invalid annot p ?(mode=STRONG) a =
    add annot p (PointerBaseSet.singleton PB_invalid) mode a


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


  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Flow.set_domain_env T_cur empty man flow |>
    Option.return


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval_interface = {
    export = [
      Zone.Z_c, Z_c_points_to;
      Zone.Z_c, Zone.Z_c_num
    ];
    import = [Zone.Z_c, Z_c_cell]
  }

  let rec points_to exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_constant (C_int n) when Z.equal n Z.zero ->
      Eval.singleton P_null flow

    | E_constant C_c_invalid ->
      Eval.singleton P_invalid flow

    | E_addr addr ->
      let pt' = P_var (A addr, mk_int 0 range, T_c_void) in
      Eval.singleton pt' flow

    | _ -> panic_at range "pointer.points_to: %a not yet supported" pp_expr exp

  let eval zone exp man flow =
    let range = exp.erange in
    match snd zone, ekind exp with
    | Z_c_points_to, _ ->
      panic_at range "pointer.eval: %a not yet supported" pp_expr exp

    | Zone.Z_c_num, E_binop(O_eq, p, q)
      when is_c_pointer_type p.etyp
        && is_c_pointer_type q.etyp ->
      panic_at range "pointer.eval: %a not yet supported" pp_expr exp

    | Zone.Z_c_num, E_binop(O_ne, p, q)
      when is_c_pointer_type p.etyp
        && is_c_pointer_type q.etyp ->
      panic_at range "pointer.eval: %a not yet supported" pp_expr exp

    | Zone.Z_c_num, E_binop(O_minus, p, q)
      when is_c_pointer_type p.etyp
        && is_c_pointer_type q.etyp ->
      panic_at range "pointer.eval: %a not yet supported" pp_expr exp

    | _ -> None



  (** Computation of post-conditions *)
  (** ============================== *)

  let exec_interface = {
    export = [Z_c_cell];
    import = [Universal.Zone.Z_universal_num];
  }

  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_local_declaration(p, None) when is_c_pointer_type p.vtyp ->
      panic_at range "pointer.exec: %a not yet supported" pp_stmt stmt

    | S_assign({ekind = E_c_cell p}, q, mode) when cell_type p |> is_c_pointer_type ->
      panic_at range "pointer.exec: %a not yet supported" pp_stmt stmt

    | S_remove_var(p) when is_c_pointer_type p.vtyp ->
      panic_at range "pointer.exec: %a not yet supported" pp_stmt stmt

    | _ -> None

  (** Handler of queries *)
  (** ================== *)

  let ask _ _ _ = None

end

let () =
  Framework.Domain.register_domain (module Domain);
  ()
