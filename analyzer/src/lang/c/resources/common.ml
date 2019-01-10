(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Common transfer functions for resource management *)

open Mopsa
open Universal.Ast
open Stubs.Ast
open Memory.Common.Points_to
open Ast
open Zone


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_resources_common : unit domain

  let id = D_c_resources_common
  let name = "c.resources.common"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_resources_common -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = [Z_c]; import = []}

  let eval_interface = {
    export = [Z_c, Z_c_low_level];
    import = [Z_c, Z_c_points_to]
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None

  (** Byte attribute *)
  (** ============== *)

  let mk_bytes_var addr range =
    let vname =
      Format.fprintf Format.str_formatter "%a_bytes" pp_addr addr;
      Format.flush_str_formatter ()
    in
    let uniq =  vname ^ ":" ^ (string_of_int addr.addr_uid) in
    let v = mkv vname uniq (addr.addr_uid) (T_c_integer C_unsigned_long) in
    mk_var v ~mode:addr.addr_mode range

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  =
    match skind stmt with
    | S_stub_free { ekind = E_addr (addr) } ->
      Post.return flow

    | S_stub_free p ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      Post.bind_return man @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (A ({ addr_kind = A_stub_resource _ } as addr), _)) ->
          (* Remove the bytes attribute before removing the address *)
          let stmt' = mk_remove (mk_bytes_var addr stmt.srange) stmt.srange in
          let flow' = man.exec stmt' flow in

          let stmt' = mk_free_addr addr stmt.srange in
          let flow' = man.exec stmt' flow' in

          let stmt'' = mk_stub_free (mk_addr addr stmt.srange) stmt.srange in
          man.exec stmt'' flow' |>
          Post.of_flow

        | E_c_points_to P_top ->
          panic_at stmt.srange "resources.common: free(⊺) not supported"

        | _ -> assert false
      end


    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ new Resource ⟧ *)
    | E_alloc_addr (A_stub_resource _) ->
      (* Allocate in the heap *)
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) exp flow |>
      Eval.bind_return @@ fun exp flow ->

      begin match ekind exp with
      | E_addr addr ->
        (* Add byte attribute *)
        let bytes = mk_bytes_var addr exp.erange in
        let flow' = man.exec (mk_add bytes exp.erange) flow in
        Eval.singleton exp flow'

      | _ -> assert false
        end

    (* 𝔼⟦ size(@resource) ⟧ *)
    | E_stub_builtin_call(SIZE, { ekind = E_addr ({ addr_kind = Stubs.Ast.A_stub_resource _ } as addr)}) ->
      let bytes = mk_bytes_var addr exp.erange in
      Eval.singleton bytes flow |>
      Eval.return

    | E_stub_attribute({ ekind = E_addr _ }, _) ->
      None

    | E_stub_attribute(p, attr) ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      Eval.bind_return @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (A ({ addr_kind = A_stub_resource _ } as addr), _)) ->
          let exp' = { exp with ekind = E_stub_attribute(mk_addr addr exp.erange, attr) }  in
          man.eval exp' flow

        | E_c_points_to P_top ->
          (* When the resource is not assigned yet, can we just return an interval ? *)
          let l, u = rangeof exp.etyp in
          let exp' = mk_z_interval l u ~typ:exp.etyp exp.erange in
          Eval.singleton exp' flow

        | _ -> assert false
      end

    | E_stub_resource_mem(p, res) ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      Eval.bind_return @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (A { addr_kind = A_stub_resource res' }, _)) ->
          if res = res' then
            Eval.singleton (mk_one exp.erange ~typ:u8) flow
          else
            Eval.singleton (mk_zero exp.erange ~typ:u8) flow

        | E_c_points_to P_top ->
          Eval.singleton (mk_top T_bool exp.erange) flow

        | _ ->
          Eval.singleton (mk_zero exp.erange ~typ:u8) flow
      end

    | _ -> None

  let ask _ _ _ = None

end

let () =
    Framework.Domains.Stateless.register_domain (module Domain)
