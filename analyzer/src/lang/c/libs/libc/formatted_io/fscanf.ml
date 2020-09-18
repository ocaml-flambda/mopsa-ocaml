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

(** Evaluation of fscanf-derived functions *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Common.Points_to
open Placeholder
open Format_string
open Common.Alarms

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.libs.clib.formatted_io.fscanf"
    end)


  let checks = [
    CHK_C_INSUFFICIENT_FORMAT_ARGS;
    CHK_C_NULL_DEREF;
    CHK_C_INVALID_DEREF;
    CHK_C_USE_AFTER_FREE;
    CHK_C_INVALID_FORMAT_ARG_TYPE
  ]

  (** {2 Transfer functions} *)
  (** ====================== *)

  let init _ _ flow =  flow

  let exec stmt man flow = None


  (** {2 Evaluation entry point} *)
  (** ========================== *)


  let assign_arg arg placeholder range man flow =
    match placeholder.ip_typ with
    | Int t ->
      let typ = T_c_integer t in
      let ptr = T_c_pointer typ in
      let flow =
        if not (is_c_pointer_type arg.etyp) || not (is_c_int_type @@ under_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg ptr man flow
        else
          flow
      in
      assert_valid_ptr arg range man flow >>%
      man.exec (mk_assign (mk_c_deref (mk_c_cast arg ptr range) range) (mk_top typ range) range)

    | Float t ->
      let typ = T_c_float t in
      let ptr = T_c_pointer typ in
      let flow =
        if not (is_c_pointer_type arg.etyp) || not (is_c_float_type @@ under_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg ptr man flow
        else
          flow
      in
      assert_valid_ptr arg range man flow >>%
      man.exec (mk_assign (mk_c_deref (mk_c_cast arg ptr range) range) (mk_top typ range) range)

    | Pointer ->
      assert false

    | String ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg (T_c_pointer s8) man flow
        else
          flow
      in
      let w = match placeholder.ip_width with
        | None -> mk_top ul range
        | Some n -> mk_int (n-1) ~typ:ul range
      in
      memrand arg (mk_zero range) w range man flow

    | WideString ->
      assert false


  (** Assign arbitrary values to arguments *)
  let assign_args format args range man flow =
    parse_input_format format range man flow >>$ fun placeholders flow ->
    match placeholders with
    | None ->
      Flow.add_local_assumption
        Soundness.A_ignore_unsupported_format_string
        range flow |>
      Post.return

    | Some placeholders ->
      let nb_required = List.length placeholders in
      let nb_given = List.length args in
      if nb_required > nb_given then
        raise_c_insufficient_format_args_alarm nb_required nb_given range man flow |>
        Post.return
      else
        let rec iter placeholders args flow =
          match placeholders, args with
          | ph :: tlp, arg :: tla ->
            assign_arg arg ph range man flow >>%
            iter tlp tla
          | _ -> Post.return flow
        in
        iter placeholders args flow



  (** Evaluation entry point *)
  let eval exp man flow =
    match ekind exp with

    (* 𝔼⟦ scanf ⟧ *)
    | E_c_builtin_call("scanf", format :: args) ->
      assign_args format args exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ fscanf ⟧ *)
    | E_c_builtin_call("fscanf", stream :: format :: args) ->
      assert_valid_stream stream exp.erange man flow >>%? fun flow ->
      assign_args format args exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

      (* 𝔼⟦ sscanf ⟧ *)
    | E_c_builtin_call("sscanf", src :: format :: args) ->
      assign_args format args exp.erange man flow >>%? fun flow ->
      assert_valid_string src exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    | _ -> None

  let ask _ _ _  = None

end

let () =
  register_stateless_domain (module Domain)
