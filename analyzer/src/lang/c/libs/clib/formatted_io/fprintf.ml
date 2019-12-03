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

(** Evaluation of fprintf-derived functions *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Universal.Ast
open Ast
open Universal.Zone
open Zone
open Common.Points_to
open Common.Base
open Common.Alarms
open Format_string
open Placeholder


module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.libs.clib.formatted_io.fprint"
    end)


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [];
      uses = []
    };
    ieval = {
      provides = [
        Z_c, Z_c_low_level
      ];
      uses = [
        Z_c,Z_c_points_to;
        Z_c,Z_u_num
      ]
    }
  }

  let alarms = [
    A_c_insufficient_format_args_cls;
    A_c_null_deref_cls;
    A_c_invalid_deref_cls;
    A_c_use_after_free_cls;
    A_c_incorrect_format_arg_cls
  ]


  (** {2 Transfer functions} *)
  (** ====================== *)

  let init prog man flow =  flow


  let exec zone stmt man flow = None

  (** Check the correct type of an argument *)
  let check_arg arg placeholder range man flow =
    match placeholder.op_typ with
    | Int t ->
      let typ = T_c_integer t in
      let flow =
        if not (is_c_int_type arg.etyp) then
          raise_c_incorrect_format_arg_alarm typ arg arg.erange (Sig.Stacked.Manager.of_domain_man man) flow
        else
          flow
      in
      let exp = mk_c_cast arg typ arg.erange in
      man.eval ~zone:(Z_c,Z_u_num) exp flow >>$ fun _ flow ->
      Post.return flow

    | Float t ->
      let typ = T_c_float t in
      let flow =
        if not (is_c_float_type arg.etyp) then
          raise_c_incorrect_format_arg_alarm typ arg arg.erange (Sig.Stacked.Manager.of_domain_man man) flow
        else
          flow
      in
      let exp = mk_c_cast arg typ arg.erange in
      man.eval ~zone:(Z_c,Z_u_num) exp flow >>$ fun _ flow ->
      Post.return flow

    | Pointer ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_incorrect_format_arg_alarm (T_c_pointer void) arg arg.erange (Sig.Stacked.Manager.of_domain_man man) flow
        else
          flow
      in
      Post.return flow

    | String ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_incorrect_format_arg_alarm (T_c_pointer s8) arg arg.erange (Sig.Stacked.Manager.of_domain_man man) flow
        else
          flow
      in
      assert_valid_string arg range man flow


  (** Check that arguments correspond to the format *)
  let check_args format args range man flow =
    parse_output_format format range man flow >>$ fun placeholders flow ->
    let nb_required = List.length placeholders in
    let nb_given = List.length args in
    if nb_required > nb_given then
      let man' = Sig.Stacked.Manager.of_domain_man man in
      raise_c_insufficient_format_args_alarm nb_required nb_given range man' flow |>
      Post.return
    else
      let rec iter placeholders args flow =
        match placeholders, args with
        | [], [] -> Post.return flow

        | ph :: tlp, arg :: tla ->
          check_arg arg ph range man flow >>$ fun () flow ->
          iter tlp tla flow

        | [], arg :: tla ->
          man.eval ~zone:(Z_c,Z_c_scalar) arg flow >>$ fun _ flow ->
          iter [] tla flow

        | _ -> assert false
      in
      iter placeholders args flow



  (** Evaluation entry point *)
  let eval zone exp man flow =
    match ekind exp with

    (* 𝔼⟦ printf(...) ⟧ *)
    | E_c_builtin_call("printf", format :: args)
    | E_c_builtin_call("__printf_chk", format :: args) ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return

    (* 𝔼⟦ fprintf(...) ⟧ *)
    | E_c_builtin_call("fprintf", stream :: format :: args)
    | E_c_builtin_call("__fprintf_chk", stream :: _ :: format :: args) ->
      assert_valid_stream stream exp.erange man flow >>$? fun () flow ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return

      (* 𝔼⟦ sprintf(...) ⟧ *)
    | E_c_builtin_call("sprintf", dst :: format :: args)
    | E_c_builtin_call("__sprintf_chk", dst :: _ :: _ :: format :: args)
    | E_c_builtin_call("__builtin___sprintf_chk", dst :: _ :: _ :: format :: args) ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      memrand dst (mk_zero ~typ:ul exp.erange) (mk_top ul exp.erange) exp.erange man flow >>$? fun () flow ->
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return


    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
