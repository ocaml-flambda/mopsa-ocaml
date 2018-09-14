(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Smashing-based abstraction of C memory cells. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Base
open Cell
open Zone

module Domain =
struct


  (** Definition of smashed cells *)
  (** =========================== *)

  type cell +=
    | C_smash of base * typ

  let () =
    register_cell {
      extract = (fun next c ->
          match c with
          | C_smash (base, typ) ->
            let o1, o2 = Ast.rangeof typ in
            let offset = mk_z_interval o1 o2 in
            base, offset, typ
          | _ -> next c
        );

      to_var = (fun next c ->
          match c with
          | C_smash(b,t) ->
            let vname =
              let () = Format.fprintf Format.str_formatter "%a" pp_cell c in
              Format.flush_str_formatter ()
            in
            {
              vname;
              vuid = base_uid b;
              vtyp = t;
            }

          | _ -> next c
        );

      print = (fun next fmt c ->
          match c with
          | C_smash(b,t) -> Format.fprintf fmt "⟪%a,%a⟫" pp_base b pp_typ t
          | _ -> next fmt c
        );

      compare = (fun next c c' ->
          match c, c' with
          | C_smash(b,t), C_smash(b', t') ->
            Compare.compose [
              (fun () -> compare_base b b');
              (fun () -> compare_typ t t');
            ]
          | _ -> next c c'
        );
    }


  (** Lattice definition *)
  (** ================== *)

  (* An abstract element is the set of smashed cells previously realized *)
  include Framework.Lattices.Powerset.Make(Cell)

  let is_bottom _ = false

  let widening = join

  let print fmt a =
    Format.fprintf fmt "smashed cells: @[%a@]@\n"
      print a


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_cells_smashing : t domain

  let id = D_c_cells_smashing

  let name = "c.memory.cells.smashing"

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cells_smashing -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning interface *)
  (** ================ *)

  let exec_interface = {
    export = [Z_c];
    import = [Z_c_cell];
  }

  let eval_interface = {
    export = [Z_c_scalar, Z_c_cell];
    import = [
      Z_c, Z_c_cell;
      Z_c, Z_c_points_to;
    ];
  }




  (** Initialization *)
  (** ============== *)

  let rec init_visitor man =
    let open Init_visitor in
    {
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          assert false
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          assert false
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          assert false
        );
    }

  let init prog man flow =
    match prog.prog_kind with
    | C_program(globals, _) ->
      let flow = Flow.set_domain_env T_cur empty man flow in
      let flow' = Init_visitor.init_globals (init_visitor man) globals flow in
      Some flow'

    | _ -> None


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_c_local_declaration(v, init) ->
      panic_at stmt.srange "smashing.exec: statement %a not supported" pp_stmt stmt

    | S_rename_var(v, v') ->
      panic_at stmt.srange "smashing.exec: statement %a not supported" pp_stmt stmt

    | S_remove_var v ->
      panic_at stmt.srange "smashing.exec: statement %a not supported" pp_stmt stmt

    | S_assign(lval, rval, mode) when is_c_scalar_type lval.etyp ->
      panic_at stmt.srange "smashing.exec: statement %a not supported" pp_stmt stmt

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    | E_var _ ->
      panic_at exp.erange "smashing.eval: expression %a not supported" pp_expr exp

    | E_c_deref _ ->
      panic_at exp.erange "smashing.eval: expression %a not supported" pp_expr exp

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let ask query man flow = None


end

let () =
  Framework.Domain.register_domain (module Domain)
