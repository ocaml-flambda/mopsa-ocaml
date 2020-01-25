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
      let name = "c.libs.cstubs"
    end)

  let interface= {
    iexec = {
      provides = [Z_c];
      uses = [Z_c; Z_c_scalar; Z_c_low_level; Z_u_num; Z_c_points_to]
    };

    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        Z_c, Z_c_points_to;
        Z_c_low_level, Z_c_points_to;
        Z_c_scalar, Z_u_num
      ]
    }
  }

  let alarms = [Common.Alarms.A_c_double_free_cls]

  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow





  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  =
    match skind stmt with
    | S_stub_free { ekind = E_addr (addr) } ->
      Post.return flow |>
      Option.return

    | S_stub_free p ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow >>$? fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (ValidAddr ({ addr_kind = A_stub_resource _ } as addr), _)) ->
          (* Remove the bytes attribute before removing the address *)
          let stmt' = mk_remove_var (mk_bytes_var addr) stmt.srange in
          let flow' = man.exec ~zone:Z_c_scalar stmt' flow in

          let stmt' = mk_free_addr addr stmt.srange in
          let flow' = man.exec stmt' flow' in

          let stmt'' = mk_stub_free (mk_addr addr stmt.srange) stmt.srange in
          man.exec stmt'' flow' |>
          Post.return |>
          Option.return

        | E_c_points_to (P_block (InvalidAddr ({ addr_kind = A_stub_resource _ }, drange), _)) ->
          Common.Alarms.(raise_c_double_free_alarm p drange stmt.srange (Sig.Stacked.Manager.of_domain_man man) flow) |>
          Post.return |>
          Option.return

        | E_c_points_to P_null ->
          Post.return flow |>
          Option.return

        | E_c_points_to P_top ->
          Soundness.warn_at stmt.srange
            "ignoring free statement because of undetermined resource pointer"
          ;
          Post.return flow |>
          Option.return


        | _ ->
          panic_at stmt.srange "resources.common: free(p | p %a) not supported" pp_expr pt
      end

    | S_rename ({ ekind = E_addr ({ addr_kind = A_stub_resource _ } as addr1) },
                { ekind = E_addr ({ addr_kind = A_stub_resource _ } as addr2) })
      ->
      let bytes1 = mk_bytes_var addr1 in
      let bytes2 = mk_bytes_var addr2 in
      man.exec ~zone:Z_c_scalar (mk_rename_var bytes1 bytes2 stmt.srange) flow |>
      man.exec ~zone:Z_c_low_level stmt |>
      man.exec ~zone:Z_c_points_to stmt |>
      Post.return |>
      Option.return

    | S_stub_requires { ekind = E_stub_builtin_call(VALID_PTR, ptr) } ->
      Some (
        let range = stmt.srange in
        let man' = Sig.Stacked.Manager.of_domain_man man in
        man.eval ptr ~zone:(Z_c, Z_c_points_to) flow >>$ fun pt flow ->
        match ekind pt with
        | E_c_points_to P_null ->
          raise_c_null_deref_alarm ptr range man' flow |>
          Result.empty_singleton

        | E_c_points_to P_invalid ->
          raise_c_invalid_deref_alarm ptr range man' flow |>
          Result.empty_singleton

        | E_c_points_to (P_block (InvalidAddr (_,r), offset)) ->
          raise_c_use_after_free_alarm ptr r range man' flow |>
          Result.empty_singleton

        | E_c_points_to (P_block (InvalidVar (v,r), offset)) ->
          raise_c_dangling_deref_alarm ptr v r range man' flow |>
          Result.empty_singleton

        | E_c_points_to P_top ->
          Soundness.warn_at range "ignoring requirement check due to ⊤ pointer %a" pp_expr ptr;
          Post.return flow

        | E_c_points_to (P_block (base, offset)) ->
          if is_expr_forall_quantified offset
          then
            Common.Base.eval_base_size base range man' flow >>$ fun size flow ->
            man.eval ~zone:(Z_c_scalar,Z_u_num) size flow >>$ fun size flow ->
            let min, max = Common.Quantified_offset.bound offset in
            man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
            man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->
            let elm = under_type ptr.etyp |> void_to_char |> (fun t -> mk_z (sizeof_type t) range) in
            let limit = sub size elm range in
            let cond = mk_binop
                (mk_in min (mk_zero range) limit range)
                O_log_and
                (mk_in max (mk_zero range) limit range)
                range
            in
            assume cond
              ~fthen:(fun flow -> Post.return flow)
              ~felse:(fun flow ->
                  raise_c_out_bound_quantified_alarm ~base ~min ~max ~size range man' flow |>
                  Post.return
                )
              ~zone:Z_u_num man flow

          (* Valid base + non-quantified offset *)
          else
            Common.Base.eval_base_size base range man' flow >>$ fun size flow ->
            man.eval ~zone:(Z_c_scalar,Z_u_num) size flow >>$ fun size flow ->
            man.eval ~zone:(Z_c_scalar,Z_u_num) offset flow >>$ fun offset flow ->
            let elm = under_type ptr.etyp |> void_to_char |> (fun t -> mk_z (sizeof_type t) range) in
            let limit = sub size elm range in
            let cond = mk_in offset (mk_zero range) limit range in
            assume cond
              ~fthen:(fun flow -> Post.return flow)
              ~felse:(fun flow ->
                  Common.Alarms.raise_c_out_bound_alarm ~base ~offset ~size range man' flow |>
                  Post.return
                )
              ~zone:Z_u_num man flow

        | _ -> assert false
      )

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
      Option.return |> Option.lift @@ Eval.bind @@ fun exp flow ->

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
      Option.return


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
      Option.return |> Option.lift @@ Eval.bind @@ fun pt flow ->

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
      Option.return |> Option.lift @@ Eval.bind @@ fun pt flow ->

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
