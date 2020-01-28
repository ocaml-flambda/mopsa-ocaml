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

(** Generic handler of assigns clauses and primed variables. *)


open Mopsa
open Framework.Core.Sig.Domain.Intermediate
open Universal.Ast
open Stubs.Ast
open Common.Points_to
open Ast
open Zone
open Universal.Zone
open Common.Base
open Common.Alarms
open Common.Points_to



module Domain =
struct

  (** Abstract element *)
  (** ================ *)

  (* We keep the set of primed bases in order to create them once when
     encountering a sequence of assigns clauses. For example, the sequence:

        assigns: a[0];
        assigns: a[1];

     should create a single primed base a'.
  *)

  module PrimedBases = Framework.Lattices.Powerset.Make(struct
      type t = base
      let compare = compare_base
      let print = pp_base
    end)

  type t = PrimedBases.t


  (** Domain identification *)
  (** ===================== *)

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.cstubs.assigns"
    end)

  let interface= {
    iexec = {
      provides = [Z_c];
      uses     = [Z_c];
    };

    ieval = {
      provides = [];
      uses     = [];
    }
  }

  let alarms = []

  let print fmt (a:t) : unit =
    Format.fprintf fmt "assigns: %a@\n" PrimedBases.print a


  (** Lattice operators *)
  (** ================= *)

  let bottom : t = PrimedBases.bottom

  let top : t = PrimedBases.top

  let is_bottom (a:t) : bool = false

  let subset (a:t) (b:t) : bool = PrimedBases.subset a b

  let join (a:t) (b:t) : t = PrimedBases.join a b

  let meet (a:t) (b:t) : t = PrimedBases.meet a b

  let widen ctx (a:t) (b:t) : t = PrimedBases.join a b

  let merge p (a,l) (b,k) = PrimedBases.join a b


  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec_assign_base base offset assigned_offset range man flow =
    assert false

  let exec_stub_assigns target offsets range man flow =
    let ptr = match offsets with
      | [] -> mk_c_address_of target range
      | _ -> target
    in
    man.eval ptr ~zone:(Z_c,Z_c_points_to) flow >>$ fun p flow ->
    let man' = Core.Sig.Stacked.Manager.of_domain_man man in
    match ekind p with
    | E_c_points_to P_null ->
      raise_c_null_deref_alarm ptr range man' flow |>
      Cases.empty_singleton

    | E_c_points_to P_invalid ->
      raise_c_invalid_deref_alarm ptr range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (InvalidAddr (_,r), offset)) ->
      raise_c_use_after_free_alarm ptr r range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (InvalidVar (v,r), offset)) ->
      raise_c_dangling_deref_alarm ptr v r range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (base, offset)) when is_base_readonly base ->
      raise_c_read_only_modification_alarm base range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (base, offset))  ->
      exec_assign_base base offset offsets range man flow

    | E_c_points_to P_top ->
      Soundness.warn_at range "ignoring ⊤ pointer %a" pp_expr (get_orig_expr ptr);
      Cases.empty_singleton flow

    | _ -> assert false



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

  let eval_stub_primed e range man flow =
    assert false

  let eval zone exp man flow =
    match ekind exp with
    | E_stub_primed e ->
      eval_stub_primed e exp.erange man flow |>
      OptionExt.return


    | _ -> None

  let ask _ _ _ = None

  let refine _ _ _ = assert false

end

let () =
  Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain)
