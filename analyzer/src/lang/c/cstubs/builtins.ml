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
open Sig.Domain.Stateless
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

  let below = mk_semantic "below" ~domain:name

  let dependencies = [ below ]

  let alarms = []


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
      Eval.singleton (mk_bytes addr mode range) flow

    | _ ->
      eval_base_size ~semantic:below base range man flow


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

    | E_stub_builtin_call(BYTES, { ekind = E_addr addr }) ->
      Rewrite.reval_singleton (mk_var (mk_bytes_var addr) exp.erange) flow |>
      OptionExt.return


    | E_stub_builtin_call(BYTES, e) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block (base,_,mode) ->
          eval_base_bytes base mode exp.erange man flow |>
          Rewrite.reval_eval

        | _ ->
          Rewrite.reval_singleton (mk_top ul exp.erange) flow
      )

    | E_stub_builtin_call(LENGTH, e) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block (base,_,mode) ->
          eval_base_bytes base mode exp.erange man flow >>$ fun bytes flow ->
          Rewrite.reval_singleton (byte_to_element (under_type e.etyp) bytes exp.erange) flow

        | _ ->
          Rewrite.reval_singleton (mk_top ul exp.erange) flow
      )

    | E_stub_builtin_call(BASE, e) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block ({ base_kind = Var v; base_valid = true; },_,mode) ->
          Rewrite.reval_singleton (mk_c_cast (mk_c_address_of (mk_var v ~mode exp.erange) exp.erange) (T_c_pointer T_c_void) exp.erange) flow

        | P_block ({ base_kind = String (str,k,_) },_,_) ->
          Rewrite.reval_singleton (mk_c_string ~kind:k str exp.erange) flow

        | P_block ({ base_kind = Addr addr; base_valid = true; },_,_) ->
          Rewrite.reval_singleton (mk_c_cast (mk_addr addr exp.erange) (T_c_pointer T_c_void) exp.erange) flow

        | P_top ->
          Rewrite.reval_singleton (mk_top (T_c_pointer T_c_void) exp.erange) flow

        | P_null ->
          Rewrite.reval_singleton (mk_c_null exp.erange) flow

        | P_invalid ->
          Rewrite.reval_singleton (mk_c_invalid_pointer exp.erange) flow

        | P_block ({ base_valid = false; },_,_) ->
          Rewrite.reval_singleton (mk_c_invalid_pointer exp.erange) flow

        | _ -> panic_at exp.erange "base(%a) where %a %a not supported" pp_expr e pp_expr e pp_points_to pt
      )

    | E_stub_builtin_call(VALID_PTR, p) ->
      Some (
        resolve_pointer p man flow >>$ fun pt flow ->
        let range = exp.erange in
        match pt with
        | P_block(b, o, m) ->
          eval_base_bytes b m range man flow >>$ fun size flow ->
          man.eval size flow >>$ fun size flow ->
          let elm = under_type p.etyp |> void_to_char |> (fun t -> mk_z (sizeof_type t) range) in
          (* Check validity of the offset *)
          let cond = mk_in o (mk_zero range) (sub size elm range) range in
          Rewrite.reval_singleton cond flow

        | P_fun _ -> Rewrite.reval_singleton (mk_one range) flow

        | P_null | P_invalid -> Rewrite.reval_singleton (mk_zero range) flow

        | P_top -> Rewrite.reval_singleton (mk_top T_bool range) flow
      )


    | E_stub_builtin_call(OFFSET, e) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block(_,o,_) -> Rewrite.reval_singleton o flow
        | _ -> Rewrite.reval_singleton (mk_top ul exp.erange) flow
      )

    | E_stub_builtin_call(INDEX, e) ->
      Some (
        resolve_pointer e man flow >>$ fun pt flow ->
        match pt with
        | P_block(_,o,_) -> Rewrite.reval_singleton (byte_to_element (under_type e.etyp) o exp.erange) flow
        | _ -> Rewrite.reval_singleton (mk_top ul exp.erange) flow
      )

    | E_stub_builtin_call((VALID_FLOAT | FLOAT_INF | FLOAT_NAN) as op, flt) ->
      let cls = match op with
        | VALID_FLOAT -> float_valid
        | FLOAT_INF -> float_inf
        | FLOAT_NAN -> float_nan
        | _ -> assert false
      in
      Some (
        man.eval flt flow >>$ fun flt flow ->
        Rewrite.reval_singleton (mk_float_class cls flt exp.erange) flow
      )

    | E_stub_builtin_call(ALIVE, p) ->
      Some (
        resolve_pointer p man flow >>$ fun pt flow ->
        let range = exp.erange in
        match pt with
        | P_block ({ base_valid }, _, _) ->
          if base_valid then
            Rewrite.reval_singleton (mk_one range) flow
          else
            Rewrite.reval_singleton (mk_zero range) flow

        | P_null | P_invalid | P_fun _ -> Rewrite.reval_singleton (mk_zero range) flow

        | P_top -> Rewrite.reval_singleton (mk_top T_bool range) flow

      )

    | _ -> None

  let ask _ _ _ = None

end

let () =
  register_stateless_domain (module Domain)
