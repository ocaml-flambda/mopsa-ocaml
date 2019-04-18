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

  let name = "c.resources.common"
  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  type zone +=
    | Z_c_resource

  let () =
    register_zone {
      zone = Z_c_resource;
      zone_subset = None;
      zone_name = "C/Resource";
      zone_eval = (fun e -> Process);
    }

  let interface= {
    iexec = {
      provides = [Z_c_resource];
      uses = [Z_c]
    };

    ieval = {
      provides = [Z_c, Z_c_low_level];
      uses = [Z_c, Z_c_points_to]
    }
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow

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
      Post.return flow |>
      Option.return

    | S_stub_free p ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      Option.return |> Option.lift @@ post_eval man @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (A ({ addr_kind = A_stub_resource _ } as addr), _)) ->
          (* Remove the bytes attribute before removing the address *)
          let stmt' = mk_remove (mk_bytes_var addr stmt.srange) stmt.srange in
          let flow' = man.exec stmt' flow in

          let stmt' = mk_free_addr addr stmt.srange in
          let flow' = man.exec stmt' flow' in

          let stmt'' = mk_stub_free (mk_addr addr stmt.srange) stmt.srange in
          man.exec stmt'' flow' |>
          Post.return

        | E_c_points_to P_top ->
          panic_at stmt.srange "resources.common: free(⊺) not supported"

        | _ -> assert false
      end

    | S_rename ({ ekind = E_addr ({ addr_kind = A_stub_resource _ } as addr1) },
                { ekind = E_addr ({ addr_kind = A_stub_resource _ } as addr2) })
      ->
      let bytes1 = mk_bytes_var addr1 stmt.srange in
      let bytes2 = mk_bytes_var addr2 stmt.srange in
      man.exec ~zone:Z_c (mk_rename bytes1 bytes2 stmt.srange) flow |>
      man.exec ~zone:Z_c stmt |>
      Post.return |>
      Option.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)

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
      Option.return

    | E_stub_attribute({ ekind = E_addr _ }, _) ->
      None

    | E_stub_attribute(p, attr) ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      Option.return |> Option.lift @@ Eval.bind @@ fun pt flow ->

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
      Option.return |> Option.lift @@ Eval.bind @@ fun pt flow ->

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
    Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
