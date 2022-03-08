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
open Sig.Abstraction.Stateless
open Universal.Ast
open Stubs.Ast
open Common.Points_to
open Ast
open Common.Alarms
open Aux_vars


let is_resource_addr_chain : (addr_kind -> bool) ref =
  ref
    (function
     | A_stub_resource _ -> true
     | _ -> false
    )

let register_is_resource_addr_chain (info: (addr_kind -> bool) -> addr_kind -> bool) =
  is_resource_addr_chain := info !is_resource_addr_chain;
  ()

let is_resouce_addr addr = !is_resource_addr_chain (akind addr)



module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.cstubs.resources"
    end)

  let dependencies= []

  let checks = [CHK_C_DOUBLE_FREE]

  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec_stub_free p range man flow =
    resolve_pointer p man flow >>$ fun pt flow ->
    match pt with
    | P_block ({ base_kind = Addr ({ addr_kind = A_stub_resource _ } as addr); base_valid = true }, _, mode) ->
      (* Tell the heap abstraction to free the address *)
      let stmt' = mk_free addr range in
      man.exec stmt' flow

    | P_block ({ base_kind = Addr { addr_kind = A_stub_resource _ }; base_valid = false; base_invalidation_range = Some drange }, _, _) ->
      raise_c_double_free_alarm p drange man ~range flow |>
      Post.return

    | P_null ->
      Post.return flow

    | P_top ->
      let flow =
        Flow.add_local_assumption
          (Soundness.A_ignore_modification_undetermined_pointer p)
          range flow
      in
      Post.return flow


    | _ ->
      panic_at range "resources.common: free(p | p %a) not supported" pp_points_to pt


  let exec_stub_rename_resource addr1 addr2 stmt man flow =
    let bytes1 = mk_bytes_var addr1 in
    let bytes2 = mk_bytes_var addr2 in
    man.exec (mk_rename_var bytes1 bytes2 stmt.srange) flow >>%
    man.exec ~route:(Below name) stmt

  let exec_stub_remove_resource addr stmt man flow =
    let bytes = mk_bytes_var addr in
    man.exec (mk_remove_var bytes stmt.srange) flow >>%
    man.exec ~route:(Below name) stmt

  let exec_stub_expand_resource addr addrl stmt man flow =
    let bytes = mk_bytes_var addr in
    let bytesl = List.map mk_bytes_var addrl in
    man.exec (mk_expand_var bytes bytesl stmt.srange) flow >>%
    man.exec ~route:(Below name) stmt

  let exec_stub_fold_resource addr addrl stmt man flow =
    let bytes = mk_bytes_var addr in
    let bytesl = List.map mk_bytes_var addrl in
    man.exec (mk_fold_var bytes bytesl stmt.srange) flow>>%
    man.exec ~route:(Below name) stmt

  let is_resource_addr_expr e =
    match ekind e with
    | E_addr (a, _) -> is_resouce_addr a
    | _ -> false

  let extract_resource_addr e =
    assert(is_resource_addr_expr e);
    match ekind e with
    | E_addr (a, _) -> a
    | _ -> assert false

  let exec stmt man flow  =
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
  let eval_stub_alloc res mode range man flow =
    let alloc = mk_alloc_addr (A_stub_resource res) ~mode range in
    man.eval alloc flow >>$ fun eaddr flow ->
    match ekind eaddr with
    | E_addr (addr, _) ->
      (* Add bytes attribute *)
      let bytes = mk_bytes_var addr in
      man.exec (mk_add_var bytes eaddr.erange) flow >>%
      (* Add the address dimension to memory abstraction *)
      man.exec ~route:(Below name) (mk_add eaddr range) >>%
      Eval.singleton eaddr

    | _ -> assert false



  let eval_stub_resource_mem p res range man flow =
    resolve_pointer p man flow >>$ fun pt flow ->
    match pt with
    | P_block ({ base_kind = Addr { addr_kind = A_stub_resource res' } }, _, _) ->
      if res = res' then
        Eval.singleton (mk_one range) flow
      else
        Eval.singleton (mk_zero range) flow

    | P_top ->
      Eval.singleton (mk_top T_bool range) flow

    | _ ->
      Eval.singleton (mk_zero range) flow


  let eval exp man flow =
    match ekind exp with
    | E_stub_alloc (res,mode) ->
      eval_stub_alloc res mode exp.erange man flow |>
      OptionExt.return

    | E_stub_resource_mem(p, res) ->
      eval_stub_resource_mem p res exp.erange man flow |>
      OptionExt.return

    | _ -> None

  let ask _ _ _ = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
