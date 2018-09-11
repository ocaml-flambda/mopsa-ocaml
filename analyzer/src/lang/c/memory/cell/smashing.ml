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
open Pointer


module Domain =
struct


  (** Cell definition *)
  (** =============== *)

  module Cell =
  struct

    type t = {
      b : base;
      t : typ;
    }

    let print fmt c =
      Format.fprintf fmt "⟪%a,%a⟫"
        pp_base c.b
        pp_typ c.t

    let compare c c' =
      Compare.compose [
        (fun () -> compare_base c.b c'.b);
        (fun () -> compare_typ c.t c'.t);
      ]

    let to_var c =
      {
        vname = (
            let () = Format.fprintf Format.str_formatter "%a" print c in
            Format.flush_str_formatter ()
          );
        vuid = base_uid c.b;
        vtyp = c.t;
      }
  end

  let add_var v man flow = assert false


  (** Lattice definition *)
  (** ================== *)

  include Framework.Lattices.Powerset.Make(Cell)

  let is_bottom _ = false

  let widening = join

  let print fmt a =
    Format.fprintf fmt "smash cells: @[%a@]@\n"
      print a


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_cell_smashing : t domain

  let id = D_c_cell_smashing

  let name = "c.memory.cell.smashing"

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cell_smashing -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Initialization *)
  (** ============== *)

  let rec init_visitor man =
    let open Init_visitor in
    {
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          match ekind v with
          | E_var v ->
            let v', flow1 = add_var v man flow in
            let stmt = mk_assign (mk_var v' range) e range in
            man.exec ~zone:Zone.Z_c_scalar stmt flow1

          | _ -> assert false
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


  (** Interface of transfer functions *)
  (** =============================== *)

  let exec_interface = {
    export = [
      Zone.Z_c_scalar
    ];
    import = [
      Universal.Zone.Z_universal_num;
      Zone.Z_c_ptr
    ];
  }

  let eval_interface = {
    export = [
      (Zone.Z_c_scalar, Universal.Zone.Z_universal_num);
      (Zone.Z_c_scalar, Zone.Z_c_ptr);
    ];
    import = [
      (Zone.Z_c_scalar_deref_free, Universal.Zone.Z_universal_num);
      (Zone.Z_c_scalar_deref_free, Zone.Z_c_ptr);
    ];
  }


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_c_local_declaration(v, init) -> assert false

    | S_rename_var(v, v') -> assert false

    | S_remove_var v -> assert false

    | S_assign(lval, rval, mode) when is_c_scalar_type lval.etyp -> assert false

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    | E_var _ | E_c_deref _ | E_c_array_subscript _
    | E_c_arrow_access _ | E_c_member_access _ ->
      assert false

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let ask query man flow = None


end

let () =
  Framework.Domain.register_domain (module Domain)
