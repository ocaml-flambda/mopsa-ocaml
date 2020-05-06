(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Common transfer functions for handling C stubs *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Universal.Ast
open Stubs.Ast
open Common.Points_to
open Ast
open Zone
open Universal.Zone
open Common.Alarms
open Aux_vars


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.cstubs.resources"
    end)

  let interface= {
    iexec = {
      provides = [Z_c];
      uses = [Z_c; Z_c_scalar; Z_c_low_level; Z_u_heap]
    };

    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        Z_c, Z_c_points_to;
        Z_c_low_level, Z_c_points_to;
        Z_u_heap, Z_any
      ]
    }
  }

  let alarms = [Common.Alarms.A_c_double_free]

  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow





  (** Computation of post-conditions *)
  (** ============================== *)

  let exec_stub_free p range man flow =
    man.eval ~zone:(Z_c, Z_c_points_to) p flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block ({ base_kind = Addr ({ addr_kind = A_stub_resource _ } as addr); base_valid = true }, _, mode)) ->
      (* Remove the bytes attribute before removing the address *)
      let flow =
        if addr.addr_mode = STRONG then
          let stmt' = mk_remove_var (mk_bytes_var addr) range in
          man.exec ~zone:Z_c_scalar stmt' flow
        else
          flow
      in
      (* Tell the heap abstraction to free the address *)
      let stmt' = mk_free addr range in
      man.post stmt' ~zone:Z_u_heap flow

    | E_c_points_to (P_block ({ base_kind = Addr { addr_kind = A_stub_resource _ }; base_valid = false; base_invalidation_range = Some drange }, _, _)) ->
      raise_c_double_free_alarm p drange (Sig.Stacked.Manager.of_domain_man man) ~range flow |>
      Post.return

    | E_c_points_to P_null ->
      Post.return flow

    | E_c_points_to P_top ->
      Soundness.warn_at range
        "ignoring free statement because of undetermined resource pointer"
      ;
      Post.return flow


    | _ ->
      panic_at range "resources.common: free(p | p %a) not supported" pp_expr pt


  let exec_stub_rename_resource addr1 addr2 stmt man flow =
    let bytes1 = mk_bytes_var addr1 in
    let bytes2 = mk_bytes_var addr2 in
    man.exec ~zone:Z_c_scalar (mk_rename_var bytes1 bytes2 stmt.srange) flow |>
    man.exec ~zone:Z_c_low_level stmt |>
    man.exec ~zone:Z_c_points_to stmt |>
    Post.return

  let exec_stub_remove_resource addr stmt man flow =
    let bytes = mk_bytes_var addr in
    man.exec ~zone:Z_c_scalar (mk_remove_var bytes stmt.srange) flow |>
    man.exec ~zone:Z_c_low_level stmt |>
    man.exec ~zone:Z_c_points_to (mk_invalidate_addr addr stmt.srange) |>
    Post.return

  let exec_stub_expand_resource addr addrl stmt man flow =
    let bytes = mk_bytes_var addr in
    let bytesl = List.map mk_bytes_var addrl in
    man.exec ~zone:Z_c_scalar (mk_expand_var bytes bytesl stmt.srange) flow |>
    man.exec ~zone:Z_c_low_level stmt |>
    man.exec ~zone:Z_c_points_to stmt |>
    Post.return

  let exec_stub_fold_resource addr addrl stmt man flow =
    let bytes = mk_bytes_var addr in
    let bytesl = List.map mk_bytes_var addrl in
    man.exec ~zone:Z_c_scalar (mk_fold_var bytes bytesl stmt.srange) flow |>
    man.exec ~zone:Z_c_low_level stmt |>
    man.exec ~zone:Z_c_points_to stmt |>
    Post.return

  let is_resouce_addr addr =
    match addr.addr_kind with
    | A_stub_resource _ -> true
    | _ -> false

  let is_resource_addr_expr e =
    match ekind e with
    | E_addr a -> is_resouce_addr a
    | _ -> false

  let extract_resource_addr e =
    match ekind e with
    | E_addr ({ addr_kind = A_stub_resource _ } as a) -> a
    | _ -> assert false

  let exec zone stmt man flow  =
    match skind stmt with
    | S_stub_free p ->
      exec_stub_free p stmt.srange man flow |>
      OptionExt.return

    | S_rename (e1, e2) when is_resource_addr_expr e1 &&
                             is_resource_addr_expr e2 ->
      exec_stub_rename_resource (extract_resource_addr e1) (extract_resource_addr e2) stmt man flow |>
      OptionExt.return

    | S_remove (e) when is_resource_addr_expr e ->
      exec_stub_remove_resource (extract_resource_addr e) stmt man flow |>
      OptionExt.return

    | S_expand (e,el) when is_resource_addr_expr e &&
                           List.for_all is_resource_addr_expr el ->
      exec_stub_expand_resource (extract_resource_addr e) (List.map extract_resource_addr el) stmt man flow |>
      OptionExt.return

    | S_fold (e,el) when is_resource_addr_expr e &&
                         List.for_all is_resource_addr_expr el ->
      exec_stub_fold_resource (extract_resource_addr e) (List.map extract_resource_addr el) stmt man flow |>
      OptionExt.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)


  (* Allocate in the heap *)
  let eval_stub_alloc res range man flow =
    let alloc = mk_alloc_addr (A_stub_resource res) range in
    man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) alloc flow >>$ fun exp flow ->
    match ekind exp with
    | E_addr addr ->
      (* Add bytes attribute *)
      let bytes = mk_bytes_var addr in
      man.post ~zone:Z_c_scalar (mk_add_var bytes exp.erange) flow >>$ fun () flow ->
      (* Add the address dimension to memory abstraction *)
      man.post (mk_add exp range) flow >>$ fun () flow ->
      Eval.singleton exp flow

    | _ -> assert false



  let eval_stub_resource_mem p res range man flow =
    man.eval ~zone:(Z_c, Z_c_points_to) p flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block ({ base_kind = Addr { addr_kind = A_stub_resource res' } }, _, _)) ->
      if res = res' then
        Eval.singleton (mk_one range ~typ:u8) flow
      else
        Eval.singleton (mk_zero range ~typ:u8) flow

    | E_c_points_to P_top ->
      Eval.singleton (mk_top T_bool range) flow

    | _ ->
      Eval.singleton (mk_zero range ~typ:u8) flow


  let eval zone exp man flow =
    match ekind exp with
    | E_stub_alloc res ->
      eval_stub_alloc res exp.erange man flow |>
      OptionExt.return

    | E_stub_resource_mem(p, res) ->
      eval_stub_resource_mem p res exp.erange man flow |>
      OptionExt.return

    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
