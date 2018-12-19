(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Manager of the `Memory` resource *)

open Mopsa
open Universal.Ast
open Stubs.Ast
open Ast
open Zone
open Mm.Common.Points_to


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_resources_memory : unit domain

  let id = D_c_resources_memory
  let name = "c.resources.memory"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_resources_memory -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [];
    import = [Z_c]
  }

  let eval_interface = {
    export = [Z_c, Z_c_low_level];
    import = [
      Z_c, Z_c_points_to;
      Universal.Zone.Z_u_heap, Z_any
    ]
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  = None


  (** Evaluation of expressions *)
  (** ========================= *)

  let mk_bytes_var addr =
    let vname =
      Format.fprintf Format.str_formatter "%a_bytes" pp_addr addr;
      Format.flush_str_formatter ()
    in
    mkv vname addr.addr_uid (T_c_integer C_unsigned_long)

  let eval zone exp man flow =
    match ekind exp with
    | E_alloc_addr (A_stub_resource "Memory") ->
      (* Allocate in the heap *)
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) exp flow |>
      Eval.bind_return @@ fun exp flow ->

      begin match ekind exp with
      | E_addr addr ->
        (* Add byte attribute *)
        let bytes = mk_bytes_var addr in
        let flow' = man.exec ~zone:Z_c (mk_add_var bytes exp.erange) flow in
        Eval.singleton exp flow'

      | _ -> assert false
        end

    | E_stub_attribute(
        { ekind = E_addr ({ addr_kind = Stubs.Ast.A_stub_resource "Memory" } as addr) },
        "bytes"
      ) ->
      let bytes = mk_bytes_var addr in
      Eval.singleton (mk_var bytes exp.erange) flow |>
      Eval.return

    | _ -> None


  let ask _ _ _ = None

end

let () =
    Framework.Domains.Stateless.register_domain (module Domain)
