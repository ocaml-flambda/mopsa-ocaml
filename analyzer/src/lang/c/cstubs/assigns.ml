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

  (* We keep the set of assigned bases in order to create a primed copy only
     once when encountering a sequence of assigns clauses. For example, the
     sequence:

        assigns: a[0];
        assigns: a[1];

     should create a single primed base a'.
  *)

  module AssignedBases = Framework.Lattices.Powerset.Make(struct
      type t = base
      let compare = compare_base
      let print = pp_base
    end)

  type t = AssignedBases.t


  (** Domain identification *)
  (** ===================== *)

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.cstubs.assigns"
    end)

  let interface= {
    iexec = {
      provides = [Z_c];
      uses     = [Z_c_low_level];
    };

    ieval = {
      provides = [Z_c,Z_c_low_level];
      uses     = [Z_c,Z_c_low_level];
    }
  }

  let alarms = []

  let print fmt (a:t) : unit =
    Format.fprintf fmt "assigns: %a@\n" AssignedBases.print a


  (** Lattice operators *)
  (** ================= *)

  let bottom : t = AssignedBases.bottom

  let top : t = AssignedBases.top

  let is_bottom (a:t) : bool = false

  let subset (a:t) (b:t) : bool = AssignedBases.subset a b

  let join (a:t) (b:t) : t = AssignedBases.join a b

  let meet (a:t) (b:t) : t = AssignedBases.meet a b

  let widen ctx (a:t) (b:t) : t = AssignedBases.join a b

  let merge p (a,l) (b,k) = AssignedBases.join a b


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow =  set_env T_cur AssignedBases.empty man flow


  (** Auxiliary variables of primed bases *)
  (** =================================== *)

  type var_kind +=
    | V_c_primed_base of base

  let mk_primed_base_var base =
    let vkind = V_c_primed_base base in
    let vname = base_uniq_name base ^ "'" in
    let vtyp = match base.base_kind with
      | Var v  -> v.vtyp
      | Addr a -> T_c_array(s8,C_array_no_length)
      | _      -> assert false
    in
    mkv vname vkind vtyp

  let mk_primed_base_expr base range =
    mk_var (mk_primed_base_var base) range

  let mk_base_expr base range =
    match base.base_kind with
    | Var v  -> mk_var v range
    | Addr a -> mk_addr a range
    | _      -> assert false


  (** Computation of post-conditions *)
  (** ============================== *)

  (** Expand base to a primed copy if not done before *)
  let expand_primed_base base range man flow =
    let a = get_env T_cur man flow in
    if AssignedBases.mem base a then
      Post.return flow
    else
      let a' = AssignedBases.add base a in
      let flow' = set_env T_cur a' man flow in
      let primed = mk_primed_base_expr base range in
      man.post (mk_expand (mk_base_expr base range) [primed] range) ~zone:Z_c_low_level flow'
      
      

  (** Declare an assigned base *)
  let exec_assign_base base target assigned_indices range man flow =
    (* Expand base to a primed copy *)
    expand_primed_base base range man flow >>$ fun () flow ->

    (* Convert the assigned indices to temporary quantified variables *)
    let quant_indices_with_tmps = List.map (fun (a,b) ->
        let tmp = mktmp ~typ:s32 () in
        mk_stub_quantified FORALL tmp (S_interval(a,b)) range, tmp
      ) assigned_indices
    in

    (* Create the assigned lval and cleaners for temporary quantified variables *)
    let lval, cleaners = List.fold_left (fun (acc,cleaners) (i,tmp) ->
        mk_c_subscript_access acc i range,
        mk_remove_var tmp range :: cleaners
      ) (target,[]) quant_indices_with_tmps
    in

    (* Execute `forget lval` *)
    man.post (mk_forget lval range) ~zone:Z_c flow >>$ fun () flow ->
    man.post (mk_block cleaners range) ~zone:Z_c flow
    


  (** Execute `assigns: target[a1..b1]..[an..bn];` *)
  let exec_stub_assigns target assigned_indices range man flow =
    let ptr = match assigned_indices with
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

    | E_c_points_to (P_block ({ base_kind = Addr _; base_valid = false; base_invalidation_range = Some r }, offset)) ->
      raise_c_use_after_free_alarm ptr r range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block ({ base_kind = Var v; base_valid = false; base_invalidation_range = Some r }, offset)) ->
      raise_c_dangling_deref_alarm ptr v r range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (base, offset)) when is_base_readonly base ->
      raise_c_read_only_modification_alarm base range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (base, _))  ->
      exec_assign_base base target assigned_indices range man flow

    | E_c_points_to P_top ->
      Soundness.warn_at range "ignoring ⊤ pointer %a" pp_expr (get_orig_expr ptr);
      Cases.empty_singleton flow

    | _ -> assert false



  (** Rename primed bases to original names *)
  let rename_primed_base base range man flow =
    let unprimed = mk_base_expr base range in
    let primed = mk_primed_base_expr base range in
    let stmt = mk_rename primed unprimed range in
    man.post stmt ~zone:Z_c_low_level flow

  
  (** Rename primed targets to original names *)
  let exec_stub_rename_primed range man flow =
    let a = get_env T_cur man flow in
    let flow = set_env T_cur AssignedBases.empty man flow in
    AssignedBases.fold (fun base acc ->
        Post.bind (rename_primed_base base range man) acc
      ) a (Post.return flow)


  let exec zone stmt man flow  =
    match skind stmt with
    | S_stub_assigns(target,offsets) ->
      exec_stub_assigns target offsets stmt.srange man flow |>
      OptionExt.return

    | S_stub_rename_primed ->
      exec_stub_rename_primed stmt.srange man flow |>
      OptionExt.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)


  let eval_primed_base base offset typ range man flow =
    let primed = mk_primed_base_expr base range in
    (* *(( t* )( ( char* )&primed + offset )) *)
    let e = mk_c_deref (mk_c_cast (add (mk_c_cast (mk_c_address_of primed range) (T_c_pointer s8) range) ~typ:(T_c_pointer s8) offset range) (T_c_pointer typ) range) range in
    Eval.singleton e flow
    

  let eval_stub_primed e range man flow =
    let ptr = mk_c_address_of e range in
    man.eval ptr ~zone:(Z_c,Z_c_points_to) flow >>$ fun p flow ->
    let man' = Core.Sig.Stacked.Manager.of_domain_man man in
    match ekind p with
    | E_c_points_to P_null ->
      raise_c_null_deref_alarm ptr range man' flow |>
      Cases.empty_singleton

    | E_c_points_to P_invalid ->
      raise_c_invalid_deref_alarm ptr range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block ({ base_kind = Addr _; base_valid = false; base_invalidation_range = Some r }, offset)) ->
      raise_c_use_after_free_alarm ptr r range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block ({ base_kind = Var v; base_valid = false; base_invalidation_range = Some r }, offset)) ->
      raise_c_dangling_deref_alarm ptr v r range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (base, offset)) when is_base_readonly base ->
      raise_c_read_only_modification_alarm base range man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (base, offset))  ->
      eval_primed_base base offset e.etyp range man flow

    | E_c_points_to P_top ->
      Soundness.warn_at range "ignoring ⊤ pointer %a" pp_expr (get_orig_expr ptr);
      Cases.empty_singleton flow

    | _ -> assert false


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
