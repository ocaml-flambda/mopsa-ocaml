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
      let name = "c.cstubs.builtins"
    end)

  let interface= {
    iexec = {
      provides = [];
      uses     = []
    };

    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses     = [Z_c_low_level, Z_c_points_to]
    }
  }


  let alarms = []


  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow




  (** Computation of post-conditions *)
  (** ============================== *)


  let exec zone stmt man flow = None



  (** Evaluation of expressions *)
  (** ========================= *)


  let eval_base_bytes base mode range man flow =
    let open Common.Base in
    match base.base_kind with
    | Addr addr ->
      Eval.singleton (mk_bytes addr mode range) flow

    | _ ->
      eval_base_size base range (Sig.Stacked.Manager.of_domain_man man) flow


  let eval zone exp man flow =
    match ekind exp with

    | E_stub_builtin_call(BYTES, { ekind = E_addr addr }) ->
      Eval.singleton (mk_var (mk_bytes_var addr) exp.erange) flow |>
      OptionExt.return


    | E_stub_builtin_call(BYTES, e) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_points_to) e flow |>
        Eval.bind @@ fun pt flow ->

        match ekind pt with
        | E_c_points_to (P_block (base,_,mode)) ->
          eval_base_bytes base mode exp.erange man flow

        | E_c_points_to P_top ->
          Soundness.warn_at exp.erange "ignoring size computation of ⊤ pointer";
          Eval.singleton (mk_top ul exp.erange) flow

        | _ -> panic_at exp.erange "bytes(%a | %a %a) not supported" pp_expr e pp_expr e pp_expr pt
      )

    | E_stub_builtin_call(SIZE, e) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_points_to) e flow |>
        Eval.bind @@ fun pt flow ->

        let elm =
          match under_type e.etyp |> remove_typedef_qual with
          | T_c_void -> Z.one
          | t -> sizeof_type t
        in

        match ekind pt with
        | E_c_points_to (P_block (base,_,mode)) ->
          eval_base_bytes base mode exp.erange man flow >>$ fun bytes flow ->
          if Z.equal elm Z.one
          then Eval.singleton bytes flow
          else Eval.singleton (mk_binop bytes O_div (mk_z elm exp.erange) ~etyp:bytes.etyp exp.erange) flow

        | E_c_points_to P_top ->
          Soundness.warn_at exp.erange "ignoring size computation of ⊤ pointer";
          let _,max = rangeof ul in
          Eval.singleton (mk_z_interval Z.one max exp.erange) flow

        | E_c_points_to P_null
        | E_c_points_to P_invalid ->
          Soundness.warn_at exp.erange "size(%a) where %a %a not supported" pp_expr e pp_expr e pp_expr pt;
          Eval.singleton (mk_top ul exp.erange) flow


        | _ -> panic_at exp.erange "size(%a | %a %a) not supported" pp_expr e pp_expr e pp_expr pt
      )


    | E_stub_builtin_call(BASE, e) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_points_to) e flow |>
        Eval.bind @@ fun pt flow ->

        match ekind pt with
        | E_c_points_to (P_block ({ base_kind = Var v; base_valid = true; },_,mode)) ->
          Eval.singleton (mk_c_cast (mk_c_address_of (mk_var v ~mode exp.erange) exp.erange) (T_c_pointer T_c_void) exp.erange) flow

        | E_c_points_to (P_block ({ base_kind = String str },_,_)) ->
          Eval.singleton (mk_c_string str exp.erange) flow

        | E_c_points_to (P_block ({ base_kind = Addr addr; base_valid = true; },_,_)) ->
          Eval.singleton (mk_c_cast (mk_addr addr exp.erange) (T_c_pointer T_c_void) exp.erange) flow

        | E_c_points_to P_top ->
          Soundness.warn_at exp.erange "ignoring base computation of ⊤ pointer";
          Eval.singleton (mk_top (T_c_pointer T_c_void) exp.erange) flow

        | E_c_points_to P_null ->
          Eval.singleton (mk_c_null exp.erange) flow

        | E_c_points_to P_invalid ->
          Eval.singleton (mk_c_invalid_pointer exp.erange) flow

        | E_c_points_to (P_block ({ base_valid = false; },_,_)) ->
          Eval.singleton (mk_c_invalid_pointer exp.erange) flow

        | _ -> panic_at exp.erange "base(%a) where %a %a not supported" pp_expr e pp_expr e pp_expr pt
      )

    | E_stub_builtin_call(VALID_PTR, p) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_points_to) p flow >>$ fun pt flow ->
        let range = exp.erange in
        match ekind pt with
        | E_c_points_to(P_block(b, o, m)) ->
          eval_base_bytes b m range man flow >>$ fun size flow ->
          man.eval size ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow >>$ fun size flow ->
          let elm = under_type p.etyp |> void_to_char |> (fun t -> mk_z (sizeof_type t) range) in
          (* Check validity of the offset *)
          let cond = mk_in o (mk_zero range) (sub size elm range) range in
          Eval.singleton cond flow

        | E_c_points_to(P_fun _) -> Eval.singleton (mk_one range) flow

        | E_c_points_to(P_null | P_invalid) -> Eval.singleton (mk_zero range) flow

        | E_c_points_to(P_top) -> Eval.singleton (mk_top T_bool range) flow

        | _ -> panic_at range "is_valid(%a | %a %a) not supported"
             pp_expr p pp_expr p pp_expr pt
      )


    | E_stub_builtin_call(OFFSET, e) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_points_to) e flow |>
        Eval.bind @@ fun pt flow ->
        match ekind pt with
        | E_c_points_to(P_block(_,o,_)) -> Eval.singleton o flow
        | _ -> Eval.singleton (mk_top ul exp.erange) flow
      )

    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
