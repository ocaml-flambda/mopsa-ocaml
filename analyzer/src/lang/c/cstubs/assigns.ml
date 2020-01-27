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

(** Execution of stub assignments *)


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
      let name = "c.cstubs.assigns"
    end)

  let interface= {
    iexec = {
      provides = [Z_c];
      uses     = [Z_c_low_level]
    };

    ieval = {
      provides = [];
      uses     = [];
    }
  }

  let alarms = []


  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec_stub_assigns target ofssets range man flow =
    assert false


  let exec_stub_rename_primed target offsets range man flow =
    assert false


  let exec zone stmt man flow  =
    match skind stmt with
    | S_stub_assigns(target,offsets) ->
      exec_stub_assigns target offsets stmt.srange man flow |>
      OptionExt.return

    | S_stub_rename_primed(target,offsets) ->
      exec_stub_rename_primed target offsets stmt.srange man flow |>
      OptionExt.return
      
    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)


  let eval_base_bytes base range man flow =
    let open Common.Base in
    match base with
    | ValidAddr addr ->
      Eval.singleton (mk_bytes addr range) flow

    | _ ->
      eval_base_size base range (Sig.Stacked.Manager.of_domain_man man) flow


  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ new Resource ⟧ *)
    | E_stub_alloc res ->
      (* Allocate in the heap *)
      let alloc = mk_alloc_addr (A_stub_resource res) exp.erange in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) alloc flow |>
      OptionExt.return |> OptionExt.lift @@ Eval.bind @@ fun exp flow ->

      begin match ekind exp with
      | E_addr addr ->
        (* Add bytes attribute *)
        let bytes = mk_bytes_var addr in
        let flow' = man.exec ~zone:Z_c_scalar (mk_add_var bytes exp.erange) flow in
        Eval.singleton exp flow'

      | _ -> assert false
      end

    | E_stub_builtin_call(BYTES, { ekind = E_addr addr }) ->
      Eval.singleton (mk_var (mk_bytes_var addr) exp.erange) flow |>
      OptionExt.return


    | E_stub_builtin_call(BYTES, e) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_points_to) e flow |>
        Eval.bind @@ fun pt flow ->

        match ekind pt with
        | E_c_points_to (P_block (base,_)) ->
          eval_base_bytes base exp.erange man flow

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
        | E_c_points_to (P_block (base,_)) ->
          eval_base_bytes base exp.erange man flow >>$ fun bytes flow ->
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
        | E_c_points_to (P_block (ValidVar v,_)) ->
          Eval.singleton (mk_c_cast (mk_c_address_of (mk_var v exp.erange) exp.erange) (T_c_pointer T_c_void) exp.erange) flow

        | E_c_points_to (P_block (String str,_)) ->
          Eval.singleton (mk_c_string str exp.erange) flow

        | E_c_points_to (P_block (ValidAddr addr,_)) ->
          Eval.singleton (mk_c_cast (mk_addr addr exp.erange) (T_c_pointer T_c_void) exp.erange) flow

        | E_c_points_to P_top ->
          Soundness.warn_at exp.erange "ignoring base computation of ⊤ pointer";
          Eval.singleton (mk_top (T_c_pointer T_c_void) exp.erange) flow

        | E_c_points_to P_null ->
          Eval.singleton (mk_c_null exp.erange) flow

        | E_c_points_to P_invalid ->
          Eval.singleton (mk_c_invalid_pointer exp.erange) flow

        | E_c_points_to (P_block (InvalidVar _,_))
        | E_c_points_to (P_block (InvalidAddr _,_)) ->
          Eval.singleton (mk_c_invalid_pointer exp.erange) flow

        | _ -> panic_at exp.erange "base(%a) where %a %a not supported" pp_expr e pp_expr e pp_expr pt
      )

    | E_stub_builtin_call(VALID_PTR, p) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_points_to) p flow >>$ fun pt flow ->
        let range = exp.erange in
        match ekind pt with
        | E_c_points_to(P_block(b, o)) ->
          eval_base_bytes b range man flow >>$ fun size flow ->
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


    | E_stub_attribute({ ekind = E_addr _ }, _) ->
      None

    | E_stub_attribute(p, attr) ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      OptionExt.return |> OptionExt.lift @@ Eval.bind @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (ValidAddr ({ addr_kind = A_stub_resource _ } as addr), _)) ->
          let exp' = { exp with ekind = E_stub_attribute(mk_addr addr exp.erange, attr) }  in
          man.eval exp' flow

        | _ -> panic_at exp.erange
                 "%a.%s where %a not supported"
                 pp_expr p
                 attr
                 pp_expr pt
      end

    | E_stub_resource_mem(p, res) ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      OptionExt.return |> OptionExt.lift @@ Eval.bind @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (ValidAddr { addr_kind = A_stub_resource res' }, _))
        | E_c_points_to (P_block (InvalidAddr ({ addr_kind = A_stub_resource res' },_), _)) ->
          if res = res' then
            Eval.singleton (mk_one exp.erange ~typ:u8) flow
          else
            Eval.singleton (mk_zero exp.erange ~typ:u8) flow

        | E_c_points_to P_top ->
          Eval.singleton (mk_top T_bool exp.erange) flow

        | _ ->
          Eval.singleton (mk_zero exp.erange ~typ:u8) flow
      end

    | E_stub_builtin_call(OFFSET, e) ->
      Some (
        man.eval ~zone:(Z_c_low_level,Z_c_points_to) e flow |>
        Eval.bind @@ fun pt flow ->
        match ekind pt with
        | E_c_points_to(P_block(_,o)) -> Eval.singleton o flow
        | _ -> Eval.singleton (mk_top ul exp.erange) flow
      )

    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
