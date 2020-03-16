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

(** Validator of stub requirements *)


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
      let name = "c.cstubs.requires"
    end)

  let interface= {
    iexec = {
      provides = [ Z_c ];
      uses     = [ Z_c;
                   Z_u_num; ]
    };

    ieval = {
      provides = [];
      uses     = [ Z_c, Z_c_points_to;
                   Z_c, Z_u_num;
                   Z_c_scalar,Z_u_num; ]
    }
  }

  let alarms = [ A_c_out_of_bound;
                 A_c_null_deref;
                 A_c_use_after_free;
                 A_c_invalid_deref;
                 Stubs.Alarms.A_stub_invalid_requires ]

  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow





  (** Computation of post-conditions *)
  (** ============================== *)

  let exec_stub_requires_valid_ptr ptr range man flow =
    let man' = Sig.Stacked.Manager.of_domain_man man in
    man.eval ptr ~zone:(Z_c, Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to P_null ->
      raise_c_null_deref_alarm ptr man' flow |>
      Cases.empty_singleton

    | E_c_points_to P_invalid ->
      raise_c_invalid_deref_alarm ptr man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block ({ base_kind = Addr _; base_valid = false; base_invalidation_range = Some r }, offset, _)) ->
      raise_c_use_after_free_alarm ptr r  man' flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block ({ base_kind = Var v; base_valid = false; base_invalidation_range = Some r }, offset, _)) ->
      raise_c_dangling_deref_alarm ptr v r man' flow |>
      Cases.empty_singleton

    | E_c_points_to P_top ->
      Soundness.warn_at range "ignoring requirement check due to ⊤ pointer %a" pp_expr ptr;
      Post.return flow

    | E_c_points_to (P_block (base, offset, _)) ->
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
          ~felse:(fun eflow ->
              raise_c_quantified_out_bound_alarm base size min max (under_type ptr.etyp) range man' flow eflow |>
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
          ~felse:(fun eflow ->
              raise_c_out_bound_alarm base size offset (under_type ptr.etyp) range man' flow eflow |>
              Post.return
            )
          ~zone:Z_u_num man flow

    | _ -> assert false

  
  let exec_stub_requires_float_class flt cls msg range man flow =
    let man' = Sig.Stacked.Manager.of_domain_man man in
    man.eval ~zone:(Z_c, Z_u_num) flt flow >>$ fun flt flow ->
    let cond = mk_float_class cls flt range in
    assume cond
      ~fthen:(fun flow -> Post.return flow)
      ~felse:(fun eflow ->
          raise_c_invalid_float_class_alarm flt msg range man' flow eflow |>
          Post.return
        )
      ~zone:Z_u_num man flow


  (** 𝕊⟦ requires cond; ⟧ *)
  let exec_stub_requires cond range man flow =
    assume cond
      ~fthen:(fun flow ->
          Post.return flow
        )
      ~felse:(fun flow ->
          Stubs.Alarms.raise_stub_invalid_requires cond range (Sig.Stacked.Manager.of_domain_man man) flow |>
          Post.return
        )
      ~negate:(fun e range ->
          let ee = map_expr
              (fun e ->
                 match ekind e with
                 | E_stub_quantified(FORALL, v, s) ->
                   VisitParts { e with ekind = E_stub_quantified(EXISTS, v, s) }

                 | E_stub_quantified(EXISTS, v, s) ->
                   VisitParts { e with ekind = E_stub_quantified(FORALL, v, s) }

                 | _ -> VisitParts e
              )
              (fun s -> VisitParts s)
              e
          in
          mk_not ee range
        )
      ~zone:Z_c man flow


  (** 𝕊⟦ ?(∃e) ⟧ *)
  let exec_assume_with_exists_quantified e range man flow =
    let vars,ee = fold_map_expr
        (fun acc e ->
           match ekind e with
           | E_stub_quantified(EXISTS, v, S_interval(l,u)) ->
             Keep (VarMap.add v (l,u) acc, mk_var v range)
           | _ -> VisitParts (acc,e)
        )
        (fun acc s -> VisitParts(acc,s))
        VarMap.empty e
    in
    let stmts,vars = VarMap.fold
      (fun v (l,u) (accs,accv) ->
         let ve = mk_var v range in
         ( mk_add ve range ::
           mk_assume (mk_in ve l u ~etyp:T_bool range) range ::
           accs ),
         v::accv
      ) vars ([mk_assume ee range], [])
    in
    man.post (mk_block stmts range ~vars) flow


  let exec zone stmt man flow  =
    match skind stmt with
    | S_assume (e) when is_expr_exists_quantified e ->
      exec_assume_with_exists_quantified e stmt.srange man flow |>
      OptionExt.return

    | S_stub_requires { ekind = E_stub_builtin_call(VALID_PTR, ptr) } ->
      exec_stub_requires_valid_ptr ptr stmt.srange man flow |>
      OptionExt.return

    | S_stub_requires { ekind = E_stub_builtin_call((VALID_FLOAT | FLOAT_INF | FLOAT_NAN) as op, flt) } ->
      let cls, msg = match op with
        | VALID_FLOAT -> float_valid, "valid"
        | FLOAT_INF -> float_inf, "infinity"
        | FLOAT_NAN -> float_nan, "NaN"
        | _ -> assert false
      in
      exec_stub_requires_float_class flt cls msg stmt.srange man flow |>
      OptionExt.return

    | S_stub_requires e ->
      exec_stub_requires e stmt.srange man flow |>
      OptionExt.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)


  let eval zone exp man flow = None

  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
