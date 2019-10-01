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


(** Domains implementing support for string literals *)


open Mopsa
open Sig.Stacked.Stateless
open Universal.Ast
open Stubs.Ast
open Ast
open Universal.Zone
open Zone
open Common.Points_to
open Common.Base
open Alarms
module Itv = Universal.Numeric.Values.Intervals.Integer.Value


module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)
  
  include GenStatelessDomainId(
    struct
      let name = "c.memory.lowlevel.string_literals"
    end)

  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [Z_u_num];
    };
    ieval = {
      provides = [Z_c_low_level,Z_c_scalar];
      uses = [
        Z_c_low_level,Z_c_points_to;
        Z_c_low_level,Z_u_num;
        Z_c_scalar,Z_u_num;
      ];
    }
  }

  
  (** {2 Initialization} *)
  (** ****************** *)

  let init prog man flow = flow


  (** {2 Post-conditions} *)
  (** ******************* *)


  (** Execute a quantified test of the form: ∀i: s[i] ? e *)
  let exec_assume_quantified_test op lval e range man flow =
    eval_pointed_base_offset (mk_c_address_of lval range) range man flow >>$ fun pp flow ->

    match pp with
    | None ->
      (* Valid pointer but unknown offset *)
      raise_c_alarm AOutOfBound range ~bottom:false man.lattice flow |>
      Post.return

    | Some (base,offset) ->
      (* Get the size of the base *)
      eval_base_size base range man flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) size flow >>$ fun size flow ->

      (** Get symbolic bounds of the offset *)
      let min, max = Common.Quantified_offset.bound offset in
      man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
      man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->

      (* Safety condition: [min, max] ⊆ [0, size - |typ|] *)
      let typ = etyp @@ remove_casts lval in
      let limit = mk_binop size O_minus (mk_z (sizeof_type typ) range) ~etyp:T_int range in
      assume
        (
          mk_binop
            (mk_in min (mk_zero range) limit range)
            O_log_and
            (mk_in max (mk_zero range) limit range)
            range
        ) ~zone:Z_u_num man flow
        ~fthen:(fun flow ->
            (* We give an answer only for the case literal strings *)
            match base with
            | V _ | A _ | Z ->
              Post.return flow

            (* We do not handle for the moment the case of muti-byte deref from a string *)
            | S _ when sizeof_type typ |> Z.gt Z.one ->
              Post.return flow

            | S str ->
              (* Handle only comparisons with zero *)
              assume
                (mk_binop e O_eq (mk_zero range) range)
                ~fthen:(fun flow ->
                    let length = mk_int (String.length str) range in
                    switch [
                      (* Case 1: before the terminating NUL byte *)
                      [
                        mk_binop max O_lt length range
                      ],
                      (fun flow ->
                         match op with
                         | O_eq | O_lt | O_le ->
                           Flow.set T_cur man.lattice.bottom man.lattice flow |>
                           Post.return

                         | O_ne | O_gt | O_ge ->
                           Post.return flow

                         | _ -> assert false
                      );

                      (* Case 2: after the terminating NUL byte *)
                      [
                        mk_binop max O_eq length range
                      ],
                      (fun flow ->
                         match op with
                         | O_ne | O_lt | O_gt ->
                           Flow.set T_cur man.lattice.bottom man.lattice flow |>
                           Post.return

                         | O_eq | O_ge | O_le ->
                           Post.return flow

                         | _ -> assert false
                      )
                    ] ~zone:Z_u_num man flow
                  )
                ~felse:(fun flow -> Post.return flow)
                ~zone:Z_c_low_level man flow
          )
        ~felse:(fun flow ->
            raise_c_alarm AOutOfBound range ~bottom:true man.lattice flow |>
            Post.return
          )


  (** Extract conditions of the form: ∀i: s[i] ? e *)
  let rec extract_quantified_test cond =
    match ekind cond with
    | E_binop(op, e1, e2) when is_comparison_op op &&
                               is_c_num_type e1.etyp &&
                               is_c_num_type e2.etyp
      ->
      begin match is_expr_quantified e1, is_expr_quantified e2 with
        | true, false ->
          if is_c_deref e1 then Some (op, e1, e2) else None

        | false, true ->
          if is_c_deref e2 then Some (flip_comparison_op op, e2, e1) else None

        | _ -> None
      end

    | E_unop(O_log_not, e) ->
      extract_quantified_test e |> Option.bind @@ fun (op,lval,e) ->
      Some (negate_comparison_op op, lval, e)

    | _ when is_c_deref cond &&
             is_c_num_type cond.etyp &&
             is_expr_quantified cond ->
      Some (O_ne, cond, mk_zero cond.erange)


    | _ -> None

      


  let exec zone stmt man flow =
    match skind stmt with
    | S_assume(cond) ->
      extract_quantified_test cond |> Option.lift @@ fun (op,lval,e) ->
      exec_assume_quantified_test op lval e stmt.srange man flow

    | _ -> None


  (** {2 Evaluations} *)
  (** *************** *)

  (** Evaluation of a pointer dereference *)
  let eval_deref p range man flow =
    let elm = under_type p.etyp |> void_to_char in
    eval_pointed_base_offset p range man flow >>$ fun pp flow ->

    match pp with
    | None ->
      (* Valid pointer but unknown offset *)
      raise_c_alarm AOutOfBound range ~bottom:false man.lattice flow |>
      Eval.singleton (mk_top elm range)

    | Some (base,offset) ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) offset flow >>$ fun offset flow ->

      eval_base_size base range man flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) size flow >>$ fun size flow ->

      let elm_size = sizeof_type elm in
      (* Safety condition: offset offset ∈ [ 0 , size - |elm| ] *)
      assume
        (mk_in offset (mk_zero range) (sub size (mk_z elm_size range) range) range)
        ~fthen:(fun flow ->
            match base with
            | V _ | A _ | Z ->
              Eval.singleton (mk_top elm range) flow

            | S _ when Z.gt elm_size Z.one ->
              Eval.singleton (mk_top elm range) flow

            | S str ->
              (* Partition result depending on the position of the terminating NUL byte *)
              let length = String.length str in
              assume
                (mk_binop offset O_eq (mk_int length range) range)
                ~fthen:(fun flow ->
                    Eval.singleton (mk_zero ~typ:elm range) flow
                  )
                ~felse:(fun flow ->
                    (* Construct an interval covering all chars pointed by the offset *)
                    let offset_itv = man.ask (Universal.Numeric.Common.mk_int_interval_query offset) flow |>
                                     Itv.meet (Itv.of_int 0 (length - 1))
                    in
                    if Itv.is_bounded offset_itv then
                      let a, b = Itv.bounds offset_itv in
                      let rec aux i =
                        if Z.gt i b
                        then Itv.bottom

                        else
                          let c = String.get str (Z.to_int i) |>
                                  Char.code
                          in
                          aux (Z.succ i) |>
                          Itv.join (Itv.of_int c c)
                      in
                      let itv = aux a in
                      if Itv.is_bottom itv
                      then Eval.empty_singleton flow
                      else
                        let min,max = aux a |> Itv.bounds in
                        if Z.equal min max
                        then Eval.singleton (mk_z min ~typ:elm range) flow
                        else Eval.singleton (mk_z_interval min max ~typ:elm range) flow
                    else
                      Eval.singleton (mk_top elm range) flow
                  )
                ~zone:Z_u_num man flow
          )
        ~felse:(fun flow ->
            raise_c_alarm AOutOfBound range ~bottom:true man.lattice flow |>
            Eval.empty_singleton
          )
        ~zone:Z_u_num man flow


  let eval zone exp man flow =
    match ekind exp with
    | E_c_deref p when under_type p.etyp |>
                       (fun t ->
                          is_c_num_type t &&
                          sizeof_type t |>
                          Z.equal Z.one
                       )
                       &&
                       not @@ is_expr_quantified p
      ->
      eval_deref p exp.erange man flow |>
      Option.return


    | _ -> None


  (** {2 Queries} *)
  (** *********** *)

  let ask query man flow = None

end

let () = Sig.Stacked.Stateless.register_stack (module Domain)
