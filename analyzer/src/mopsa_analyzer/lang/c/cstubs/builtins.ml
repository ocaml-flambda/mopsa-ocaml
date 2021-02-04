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

(** Evaluation of stub builtins: bytes, size, base, offset and valid_ptr *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Stubs.Ast
open Common.Points_to
open Ast
open Common.Alarms
open Aux_vars


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.cstubs.builtins"
    end)

  let checks = []


  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow




  (** Computation of post-conditions *)
  (** ============================== *)


  let exec stmt man flow = None



  (** Evaluation of expressions *)
  (** ========================= *)


  let eval_base_bytes base mode range man flow =
    let open Common.Base in
    match base.base_kind with
    | Addr addr ->
      man.eval (mk_bytes addr mode range) flow

    | _ ->
      eval_base_size ~route:(Below name) base range man flow


  let byte_to_element t bytes range =
    let elm =
      match remove_typedef_qual t with
      | T_c_void -> Z.one
      | tt -> sizeof_type tt in
    if Z.equal elm Z.one
    then bytes
    else mk_binop bytes O_div (mk_z elm range) ~etyp:bytes.etyp range

  let eval exp man flow =
    match ekind exp with

    | E_stub_builtin_call(BYTES, [{ ekind = E_addr (addr, mode) }]) ->
      man.eval (mk_var (mk_bytes_var addr) ~mode:mode exp.erange) flow |>
      OptionExt.return


    | E_stub_builtin_call(BYTES, [e]) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block (base,_,mode) ->
          eval_base_bytes base mode exp.erange man flow

        | _ ->
          man.eval (mk_top ul exp.erange) flow
      )

    | E_stub_builtin_call(LENGTH, [e]) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block (base,_,mode) ->
          eval_base_bytes base mode exp.erange man flow >>$ fun bytes flow ->
          man.eval (byte_to_element (under_type e.etyp) bytes exp.erange) flow

        | _ ->
          man.eval (mk_top ul exp.erange) flow
      )

    | E_stub_builtin_call(BASE, [e]) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block ({ base_kind = Var v; base_valid = true; },_,mode) ->
          Eval.singleton (mk_c_cast (mk_c_address_of (mk_var v ~mode exp.erange) exp.erange) (T_c_pointer T_c_void) exp.erange) flow

        | P_block ({ base_kind = String (str,k,_) },_,_) ->
          Eval.singleton (mk_c_string ~kind:k str exp.erange) flow

        | P_block ({ base_kind = Addr addr; base_valid = true; },_,_) ->
          Eval.singleton (mk_c_cast (mk_addr addr exp.erange) (T_c_pointer T_c_void) exp.erange) flow

        | P_top ->
          Eval.singleton (mk_top (T_c_pointer T_c_void) exp.erange) flow

        | P_null ->
          Eval.singleton (mk_c_null exp.erange) flow

        | P_invalid ->
          Eval.singleton (mk_c_invalid_pointer exp.erange) flow

        | P_block ({ base_valid = false; },_,_) ->
          Eval.singleton (mk_c_invalid_pointer exp.erange) flow

        | _ -> panic_at exp.erange "base(%a) where %a %a not supported" pp_expr e pp_expr e pp_points_to pt
      )

    | E_stub_builtin_call(OFFSET, [e]) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block(_,o,_) -> Eval.singleton o flow
        | _ -> man.eval (mk_top ul exp.erange) flow
      )

    | E_stub_builtin_call(INDEX, [e]) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block(_,o,_) -> Eval.singleton (byte_to_element (under_type e.etyp) o exp.erange) flow
        | _ -> man.eval (mk_top ul exp.erange) flow
      )

    | E_stub_builtin_call((VALID_FLOAT | FLOAT_INF | FLOAT_NAN) as op, [flt]) ->
      let cls = match op with
        | VALID_FLOAT -> float_valid
        | FLOAT_INF -> float_inf
        | FLOAT_NAN -> float_nan
        | _ -> assert false
      in
      Some (
        man.eval flt flow ~translate:"Universal" >>$ fun flt flow ->
        Eval.singleton (mk_float_class cls flt exp.erange) flow
      )

    | E_stub_builtin_call(ALIVE, [p]) ->
      Some (
        resolve_pointer p man flow >>$ fun pt flow ->
        let range = exp.erange in
        match pt with
        | P_block ({ base_valid }, _, _) ->
          if base_valid then
            Eval.singleton (mk_one range) flow
          else
            Eval.singleton (mk_zero range) flow

        | P_null | P_invalid | P_fun _ -> Eval.singleton (mk_zero range) flow

        | P_top -> Eval.singleton (mk_top T_bool range) flow

      )

    | E_stub_builtin_call(RESOURCE, [p]) ->
      Some (
        resolve_pointer p man flow >>$ fun pt flow ->
        let range = exp.erange in
        match pt with
        | P_block ({ base_kind = Addr {addr_kind = A_stub_resource _} }, _, _) ->
          Eval.singleton (mk_one range) flow

        | P_block ({ base_kind = Addr _}, _, _)
        | P_block ({ base_kind = Var _ }, _, _)
        | P_block ({ base_kind = String _ }, _, _)
        | P_null | P_invalid | P_fun _ ->
          Eval.singleton (mk_zero range) flow

        | P_top ->
          Eval.singleton (mk_top T_bool range) flow
      )

    | _ -> None

  let ask _ _ _ = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
