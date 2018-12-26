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
open Mm.Common.Points_to
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


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  =
    match skind stmt with
    | S_stub_free { ekind = E_addr (addr, _) } ->
      let stmt' = mk_free_addr addr stmt.srange in
      man.exec stmt' flow |>
      Post.return

    | S_stub_free p ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      Post.bind_return man @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (A ({ addr_kind = A_stub_resource _ } as addr, mode), _)) ->
          let stmt' = mk_stub_free (mk_addr addr ~mode stmt.srange) stmt.srange in
          man.exec stmt' flow |>
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
    | E_stub_attribute({ ekind = E_addr _ }, _) ->
      None

    | E_stub_attribute(p, attr) ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      Eval.bind_return @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (A ({ addr_kind = A_stub_resource _ } as addr, mode), _)) ->
          let exp' = { exp with ekind = E_stub_attribute(mk_addr addr ~mode exp.erange, attr) }  in
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
        | E_c_points_to (P_block (A ({ addr_kind = A_stub_resource res' }, _), _)) ->
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
