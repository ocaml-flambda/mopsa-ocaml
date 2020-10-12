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

(** Domain implementing various checks for protecting memory access *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Stubs.Ast
open Universal.Numeric.Common
open Common.Base
open Common.Points_to
open Common.Alarms


module Domain =
struct

  (** {2 Domain header *)
  (** ================ *)

  include GenStatelessDomainId(struct
      let name = "c.memory.protection"
    end)

  let dependencies = []

  let checks = [ CHK_C_INVALID_MEMORY_ACCESS ]


  (** {2 Utility functions} *)
  (** ===================== *)

  let mk_lval base offset typ mode range =
    let base_addr = match base.base_kind with
      | Var v -> mk_c_address_of (mk_var v ~mode range) range
      | Addr a -> mk_addr a range
      | String (s,kind,t) -> mk_c_string s ~kind range in
    let addr =
      mk_c_cast
        ( add
            (mk_c_cast base_addr (T_c_pointer s8) range)
            offset
            ~typ:(T_c_pointer s8)
            range )
        (T_c_pointer typ)
        range in
    mk_c_deref addr range

  let check_offset_access base offset mode typ range man flow =
    eval_base_size base range man flow >>$ fun size flow ->
    let cond = mk_in offset zero (sub size (mk_z (sizeof_type (void_to_char typ)) range) range) range in
    match eval_num_cond cond man flow with
    | Some true  -> safe_c_memory_access_check range man flow |>
                    Cases.singleton (Some (mk_lval base offset typ mode range))
    | Some false -> raise_c_out_bound_alarm base size offset typ range man flow flow |>
                    Cases.empty
    | None ->
      assume cond
        ~fthen:(fun tflow -> safe_c_memory_access_check range man flow |>
                             Cases.singleton (Some (mk_lval base offset typ mode range)))
        ~felse:(fun eflow -> raise_c_out_bound_alarm base size offset typ range man flow eflow |>
                             Cases.empty)
        man flow

  let check_write_access lval man flow =
    let ptr = mk_c_address_of lval lval.erange in
    resolve_pointer ptr man flow >>$ fun pt flow ->
    match pt with
    | P_null ->
      raise_c_null_deref_alarm ptr man flow |>
      Cases.empty

    | P_invalid ->
      raise_c_invalid_deref_alarm ptr man flow |>
      Cases.empty

    | P_block ({ base_kind = Addr _; base_valid = false; base_invalidation_range = Some r }, offset, _) ->
      raise_c_use_after_free_alarm ptr r man flow |>
      Cases.empty

    | P_block ({ base_kind = Var v; base_valid = false; base_invalidation_range = Some r }, offset, _) ->
      raise_c_dangling_deref_alarm ptr v r man flow |>
      Cases.empty

    | P_block (base, offset, mode) when is_base_readonly base ->
      raise_c_modify_read_only_alarm ptr base man flow |>
      Cases.empty

    | P_block (base, offset, mode) ->
      check_offset_access base offset mode lval.etyp lval.erange man flow

    | P_top ->
      raise_c_memory_access_warning lval.erange man flow |>
      Flow.add_local_assumption (Soundness.A_ignore_modification_undetermined_pointer ptr) lval.erange |>
      Cases.singleton None

    | P_fun _ ->
      assert false

  let check_read_access lval man flow =
    let ptr = mk_c_address_of lval lval.erange in
    resolve_pointer ptr man flow >>$ fun pt flow ->
    match pt with
    | P_null ->
      raise_c_null_deref_alarm ptr man flow |>
      Cases.empty

    | P_invalid ->
      raise_c_invalid_deref_alarm ptr man flow |>
      Cases.empty

    | P_block ({ base_kind = Addr _; base_valid = false; base_invalidation_range = Some r }, offset, _) ->
      raise_c_use_after_free_alarm ptr r man flow |>
      Cases.empty

    | P_block ({ base_kind = Var v; base_valid = false; base_invalidation_range = Some r }, offset, _) ->
      raise_c_dangling_deref_alarm ptr v r man flow |>
      Cases.empty

    | P_block (base, offset, mode) ->
      check_offset_access base offset mode lval.etyp lval.erange man flow

    | P_top ->
      raise_c_memory_access_warning lval.erange man flow |>
      Cases.singleton None

    | P_fun _ ->
      assert false


  (** {2 Transfer functions} *)
  (** ====================== *)

  let init prog man flow = flow

  let exec stmt man flow =
    match skind stmt with
    | S_assign({ekind = E_var _},_) -> None

    | S_assign(lval,rval) when is_c_scalar_type lval.etyp ->
      ( check_write_access lval man flow >>$ fun lval' flow ->
        match lval' with
        | None -> Post.return flow
        | Some x ->
          let stmt' = mk_assign x rval stmt.srange in
          man.exec stmt' ~route:(Below name) flow
      ) |>
      OptionExt.return

    | _ -> None

  let eval exp man flow =
    match ekind exp with
    | E_var _ -> None

    | _ when is_c_lval exp
          && is_c_scalar_type exp.etyp ->
      ( check_read_access exp man flow >>$ fun exp' flow ->
        match exp' with
        | None -> man.eval (mk_top exp.etyp exp.erange) flow
        | Some x -> man.eval x ~route:(Below name) flow
      ) |>
      OptionExt.return

    | _ -> None

  let ask query man flow  = None

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
