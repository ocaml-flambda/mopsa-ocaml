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

(** Domain for handling string literals. *)


open Mopsa
open Core.Sig.Stacked.Stateless
open Universal.Ast
open Stubs.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to
open Common.Alarms
open Universal.Numeric.Common


module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  include GenStatelessDomainId(struct
      let name = "c.memory.lowlevel.string_literal"
    end)

  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [
        Z_u_num;
        Z_c_low_level
      ];
    };
    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        Z_c, Z_u_num;
        Z_c_scalar, Z_u_num;
        Z_c_low_level, Z_c_points_to;
      ];
    }
  }

  let alarms = []


  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog man flow = flow


  (** {2 Abstract transformers} *)
  (** ************************* *)


  (** Cases of the abstract transformer for tests *(str + ∀i) != 0 *)
  let assume_quantified_non_zero_cases str offset range man flow =
    (** Get symbolic bounds of the offset *)
    let min, max = Common.Quantified_offset.bound offset in
    man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
    man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->

    let length = mk_z (Z.of_int @@ String.length str) range in

    let mk_bottom flow = Flow.set T_cur man.lattice.bottom man.lattice flow in

    (* Safety condition: [min, max] ⊆ [0, length] *)
    assume (
      mk_binop
        (mk_in min (mk_zero range) length range)
        O_log_and
        (mk_in max (mk_zero range) length range)
        range
    )
      ~fthen:(fun flow ->
          switch [
            (* nonzero case *)
            (* Range condition: max < length

               |--------|***********|---------|--------->
               0       min         max     length

                     ∀ i ∈ [min, max] : s[i] != 0
            *)
            (* Transformation: nop *)
            [
              mk_binop max O_lt length range;
            ],
            (fun flow -> Post.return flow)
            ;

            (* zero case *)
            (* Range condition: length = max

                                          length
               |--------|*******************|--------->
               0       min                 max

                      ∃ i ∈ [min, max] : s[i] == 0
            *)
            (* Transformation: ⊥ *)
            [
              mk_binop max O_eq length range;
            ],
            (fun flow -> Post.return (mk_bottom flow))
            ;
          ] ~zone:Z_u_num man flow
        )
      ~felse:(fun flow ->
          Flow.set_bottom T_cur flow |>
          Post.return
        )
      ~zone:Z_u_num man flow




  (** Cases of the abstract transformer for tests *(str + ∀i) == 0 *)
  let assume_quantified_zero_cases str offset range man flow =
    (** Get symbolic bounds of the offset *)
    let min, max = Common.Quantified_offset.bound offset in
    man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
    man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->

    let length = mk_z (Z.of_int @@ String.length str) range in
    let mk_bottom flow = Flow.bottom_from flow in

    (* Safety condition: [min, max] ⊆ [0, length] *)
    assume (
      mk_binop
        (mk_in min (mk_zero range) length range)
        O_log_and
        (mk_in max (mk_zero range) length range)
        range
    )
      ~fthen:(fun flow ->
          switch [
            (* Range condition: min < length

               |--------|*************|-----|----->
               0       min           max  length

                      ∃ i ∈ [min, max] : s[i] != 0
            *)
            (* Transformation: ⊥ *)
            [
              mk_binop min O_lt length range;
            ],
            (fun flow -> Post.return (mk_bottom flow))
            ;

            (* Range condition: min = length

                      length
               |--------|--------------->
               0      min,max
            *)
            (* Transformation: nop *)
            [
              mk_binop min O_eq length range;
            ],
            (fun flow -> Post.return flow)
            ;


          ] ~zone:Z_u_num man flow
        )
      ~felse:(fun flow ->
          Flow.set_bottom T_cur flow |>
          Post.return
        )
      ~zone:Z_u_num man flow

  (** Get the string and offset pointed by lval. Ignore flows where lval is not a string *)
  let extract_string_base lval range man flow =
    let p =
      let rec doit e =
        match ekind e with
        | E_c_deref p -> p
        | E_c_cast(ee, _) -> doit ee
        | _ -> panic_at range "assume_zero: invalid argument %a" pp_expr lval;
      in
      doit lval
    in
    if Z.(sizeof_type (under_type p.etyp) != one) then
      Cases.empty_singleton flow
    else
      man.eval p ~zone:(Z_c_low_level, Z_c_points_to) flow >>$ fun pt flow ->
      match ekind pt with
      | E_c_points_to (P_block ({ base_kind = String str }, offset, mode)) ->
        Cases.singleton (str,offset) flow

      | _ ->
        Cases.empty_singleton flow

  (** Abstract transformer for tests *(str + ∀offset) op 0 *)
  let assume_quantified_op_zero op str offset range man flow =
    if op = O_ne
    then assume_quantified_non_zero_cases str offset range man flow

    else if op = O_eq
    then assume_quantified_zero_cases str offset range man flow

    else Post.return flow

  (** Abstract transformer for tests *(lval + ∀offset) op 0 *)
  let assume_quantified_zero op lval range man flow =
    extract_string_base lval range man flow >>$ fun (str,offset) flow ->
    assume_quantified_op_zero op str offset range man flow


  (** Test first if n == 0, and then call assume_zero to do the work *)
  let assume_not_sure_quantified_zero op lval n range man flow =
    extract_string_base lval range man flow >>$ fun (str,offset) flow ->
    match c_expr_to_z offset with
    | Some n when Z.(n != zero) -> Post.return flow
    | Some n -> assume_quantified_op_zero op str offset range man flow
    | None ->
      assume (mk_binop n O_eq (mk_zero range) range)
        ~fthen:(fun flow -> assume_quantified_op_zero op str offset range man flow)
        ~felse:(fun flow -> Post.return flow)
        ~zone:Z_c_low_level man flow


  (** Transformers entry point *)
  let exec zone stmt man flow =
    match skind stmt with
    (* 𝕊⟦ *(p + i) == 0 ⟧ *)
    | S_assume({ ekind = E_binop(O_eq, lval, n)})
    | S_assume({ ekind = E_unop(O_log_not, { ekind = E_binop(O_ne, lval, n)} )})
      when is_c_int_type lval.etyp &&
           is_lval_offset_forall_quantified lval &&
           not (is_expr_forall_quantified n)
      ->
      assume_not_sure_quantified_zero O_eq lval n stmt.srange man flow |>
      OptionExt.return

    (* 𝕊⟦ !*(p + i) ⟧ *)
    | S_assume({ ekind = E_unop(O_log_not,lval)})
      when is_c_int_type lval.etyp &&
           is_lval_offset_forall_quantified lval
      ->
      assume_quantified_zero O_eq lval stmt.srange man flow |>
      OptionExt.return

    (* 𝕊⟦ *(p + i) != 0 ⟧ *)
    | S_assume({ ekind = E_binop(O_ne, lval, n)})
    | S_assume({ ekind = E_unop(O_log_not, { ekind = E_binop(O_eq, lval, n)} )})
      when is_c_int_type lval.etyp &&
           is_lval_offset_forall_quantified lval &&
           not (is_expr_forall_quantified n)
      ->
      assume_not_sure_quantified_zero O_ne lval n stmt.srange man flow |>
      OptionExt.return

    (* 𝕊⟦ *(p + i) ⟧ *)
    | S_assume(lval)
      when is_c_int_type lval.etyp &&
           is_lval_offset_forall_quantified lval
      ->
      assume_quantified_zero O_ne lval stmt.srange man flow |>
      OptionExt.return


    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Evaluate a dereference *p *)
  let eval_deref p range man flow =
    man.eval p ~zone:(Z_c_low_level, Z_c_points_to) flow >>$ fun pt flow ->
    match ekind pt with
    | E_c_points_to (P_block ({ base_kind = String str }, offset, mode)) ->
      (* Get the interval of the offset *)
      man.eval offset ~zone:(Z_c_scalar,Z_u_num) flow >>$ fun offset flow ->
      let itv = man.ask (mk_int_interval_query offset) flow in
      (* Maximal interval, to keep itv bounded *)
      let length = Z.of_int (String.length str) in
      let max = I.of_z Z.zero length in
      let itv' = I.meet_bot itv (Bot.Nb max) |> Bot.bot_to_exn in
      (* Get the interval of possible chars *)
      let indexes = I.to_list itv' in
      let char_at i =
        let chr =
          if i = length
          then Z.zero
          else str.[Z.to_int i] |> Char.code |> Z.of_int
        in
        I.cst chr
      in
      let chars = List.fold_left (fun acc i ->
          char_at i :: acc
        ) [char_at (List.hd indexes)] (List.tl indexes)
      in
      let l,u =
        match I.join_list chars |> Bot.bot_to_exn with
        | I.B.Finite l, I.B.Finite u -> l,u
        | _ -> assert false
      in
      if Z.equal l u
      then Eval.singleton (mk_z l ~typ:(under_type p.etyp) range) flow
      else Eval.singleton (mk_z_interval l u ~typ:(under_type p.etyp) range) flow

    | _ ->
      Eval.singleton (mk_top (under_type p.etyp) range) flow


  let eval zone exp man flow =
    match ekind exp with
    | E_c_deref(p) when is_c_int_type exp.etyp &&
                        not (is_pointer_offset_forall_quantified p)
      ->
      eval_deref p exp.erange man flow |>
      OptionExt.return

    | _ -> None


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


end

let () =
  Core.Sig.Stacked.Stateless.register_stack (module Domain)
