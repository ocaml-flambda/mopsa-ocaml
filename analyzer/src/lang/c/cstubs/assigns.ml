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
open Sig.Abstraction.Stateless
open Universal.Ast
open Stubs.Ast
open Common.Points_to
open Ast
open Common.Base
open Common.Alarms
open Common.Points_to
open Aux_vars


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.cstubs.assigns"
    end)

  let lowlevel = Semantic "C/Lowlevel"

  let checks = []


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = flow



  (** Computation of post-conditions *)
  (** ============================== *)


  (* Collect assigned bases *)
  let assigned_bases assigns range man flow =
    List.fold_left (fun acc assign ->
        let ptr = match assign.assign_offset with
          | [] -> mk_c_address_of assign.assign_target range
          | _ -> assign.assign_target
        in
        let pp = resolve_pointer ptr man flow in
        Cases.fold_result (fun acc p flow ->
            match p with
            | P_block ({ base_valid = true; base_kind = Var _ | Addr _ } as base, _, _) ->
              BaseSet.add base acc
            | _ -> acc
          ) acc pp
      ) BaseSet.empty assigns

  (** Expand base to a primed copy *)
  let expand_primed_base base range man flow =
    let primed = mk_primed_base_expr base range in
    (* XXX It is important to perform the expand on the lowlevel semantics that
       abstracts the contents of the memory, which will have the effect of 
       expanding the contents of the base. Otherwise, performing the expand 
       on the toplevel abstraction will also update the pointers map so that
       pointers pointing to base will point also to base', which is not what
       we want.
       Consequently, this domain should be placed *after* the toplevel C
       abstraction (i.e. c.memory.blocks) in the configuration file.
    *)
    man.exec (mk_expand (mk_base_expr base range) [primed] range) ~route:lowlevel flow


  (** Prepare primed copies of assigned bases *)
  let exec_stub_prepare_all_assigns assigns range man flow =
    (* Expand assigned bases to primed copies *)
    let bases = assigned_bases assigns range man flow in
    BaseSet.fold (fun base acc ->
        Post.bind (expand_primed_base base range man) acc
      ) bases (Post.return flow)


  (** Declare an assigned base *)
  let exec_assign_base base offset mode typ assigned_indices range man flow =
    match assigned_indices with
    | [] ->
      (* Prime the target *)
      let primed_target = mk_primed_address base offset typ range in
      let lval = mk_c_deref primed_target range in
      man.exec (mk_forget lval range) flow

    | _ ->

      (* Convert the assigned indices to temporary quantified variables *)
      let quant_indices_with_tmps = List.map (fun (a,b) ->
          let tmp = mktmp ~typ:s32 () in
          (FORALL, tmp, S_interval(a,b)), tmp, a, b
        ) assigned_indices
      in

      (* Prime the target *)
      let primed_target = mk_primed_address base offset (under_type typ) range in

      (* Create the assigned lval *)
      let adds, assumes, quants, lval, cleaners = List.fold_left (fun (adds,assumes,quants,acc,cleaners) (q,tmp,a,b) ->
          mk_add_var tmp range :: adds,
          mk_assume (mk_in (mk_var tmp range) a b range) range :: assumes,
          q::quants,
          mk_c_subscript_access acc (mk_var tmp range) range,
          mk_remove_var tmp range :: cleaners
        ) ([],[],[],primed_target,[]) quant_indices_with_tmps
      in

      (* Execute `forget lval` *)
      man.exec (mk_block adds range) flow >>%
      man.exec (mk_block assumes range) >>%
      man.exec (mk_forget (mk_stub_quantified_formula quants lval range) range) >>%
      man.exec (mk_block cleaners range)



  (** Execute `assigns: target[a1..b1]..[an..bn];` *)
  let exec_stub_assigns target assigned_indices range man flow =
    let ptr = match assigned_indices with
      | [] -> mk_c_address_of target range
      | _ -> target
    in
    resolve_pointer ptr man flow >>$ fun p flow ->
    match p with
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

    | P_block (base, offset, _) when is_base_readonly base ->
      raise_c_modify_read_only_alarm ptr base man flow |>
      Cases.empty

    | P_block (base, offset, mode)  ->
      exec_assign_base base offset mode target.etyp assigned_indices range man flow

    | P_top ->
      Soundness.warn_at range "ignoring assigns on ⊤ pointer %a" pp_expr (get_orig_expr ptr);
      Post.return flow

    | _ -> assert false



  (** Rename primed bases to original names *)
  let rename_primed_base base range man flow =
    let unprimed = mk_base_expr base range in
    let primed = mk_primed_base_expr base range in
    let stmt = mk_rename primed unprimed range in
    let post1 = man.exec stmt ~route:(Below name) flow in
    (* If this is a weak base, we need to restore the old values. *)
    (* To do that, we remove the primed base from the flow and we join with post1 *)
    if base_mode base = STRONG then
      post1
    else
      let post2 = man.exec (mk_remove primed range) ~route:lowlevel flow in
      Post.join post1 post2


  (** Clean state from primed bases *)
  let exec_stub_clean_all_assigns assigns range man flow =
    (* Rename primed copies to original version *)
    let bases = assigned_bases assigns range man flow in
    BaseSet.fold (fun base acc ->
        Post.bind (rename_primed_base base range man) acc
      ) bases (Post.return flow)



  let exec stmt man flow  =
    match skind stmt with
    | S_stub_prepare_all_assigns al ->
      exec_stub_prepare_all_assigns al stmt.srange man flow |>
      OptionExt.return

    | S_stub_assigns a ->
      exec_stub_assigns a.assign_target a.assign_offset stmt.srange man flow |>
      OptionExt.return

    | S_stub_clean_all_assigns al ->
      exec_stub_clean_all_assigns al stmt.srange man flow |>
      OptionExt.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)


  let eval_primed_base base offset mode typ range man flow =
    let p = mk_primed_address base offset typ range in
    man.eval (mk_c_deref p range) flow


  let eval_stub_primed e range man flow =
    let ptr = mk_c_address_of e range in
    resolve_pointer ptr man flow >>$ fun p flow ->
    match p with
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

    | P_block (base, offset, _) when is_base_readonly base ->
      raise_c_modify_read_only_alarm ptr base man flow |>
      Cases.empty

    | P_block (base, offset, mode)  ->
      eval_primed_base base offset mode e.etyp range man flow

    | P_top ->
      Soundness.warn_at range "ignoring prime of ⊤ pointer %a" pp_expr (get_orig_expr ptr);
      man.eval (mk_top e.etyp range) flow

    | _ -> assert false


  let eval_stub_primed_address e range man flow =
    let ptr = mk_c_address_of e range in
    resolve_pointer ptr man flow >>$ fun p flow ->
    match p with
    | P_null ->
      Eval.singleton (mk_c_null range) flow

    | P_invalid ->
      Eval.singleton (mk_c_invalid_pointer range) flow

    | P_block (base, offset, mode) ->
      man.eval (mk_primed_address base offset e.etyp range) flow

    | P_top ->
      Eval.singleton (mk_top (T_c_pointer e.etyp) range) flow

    | _ -> assert false


  let eval exp man flow =
    match ekind exp with
    | E_stub_primed e ->
      eval_stub_primed e exp.erange man flow |>
      OptionExt.return

    | E_c_address_of {ekind = E_stub_primed e} ->
      eval_stub_primed_address e exp.erange man flow |>
      OptionExt.return

    | E_stub_builtin_call(BYTES, { ekind = E_var({ vkind = V_c_primed_base base },_) }) ->
      eval_base_size base exp.erange man flow |>
      OptionExt.return


    | _ -> None

  let ask _ _ _ = None

end

let () =
  register_stateless_domain (module Domain)
