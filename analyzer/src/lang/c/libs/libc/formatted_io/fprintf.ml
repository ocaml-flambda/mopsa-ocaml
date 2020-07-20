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
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
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

  let dependencies = []

  let alarms = [
    A_c_insufficient_format_args;
    A_c_null_deref;
    A_c_invalid_deref;
    A_c_use_after_free;
    A_c_invalid_format_arg_type
  ]


  (** {2 Transfer functions} *)
  (** ====================== *)

  let init prog man flow =  flow


  let exec stmt man flow = None

  (** Check the correct type of an argument *)
  let check_arg arg placeholder range man flow =
    match placeholder.op_typ with
    | Int t ->
      let typ = T_c_integer t in
      let flow =
        if not (is_c_int_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg typ man flow
        else
          flow
      in
      let exp = mk_c_cast arg typ arg.erange in
      man.eval exp flow >>$ fun _ flow ->
      Post.return flow

    | Float t ->
      let typ = T_c_float t in
      let flow =
        if not (is_c_float_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg typ man flow
        else
          flow
      in
      let exp = mk_c_cast arg typ arg.erange in
      man.eval exp flow >>$ fun _ flow ->
      Post.return flow

    | Pointer ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg (T_c_pointer void) man flow
        else
          flow
      in
      Post.return flow

    | String ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg (T_c_pointer s8) man flow
        else
          flow
      in
      assert_valid_string arg range man flow

    | WideString ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg (T_c_pointer s8) man flow
        else
          flow
      in
      assert_valid_wide_string arg range man flow


  (** Check that arguments correspond to the format *)
  let check_args ?wide format args range man flow =
    parse_output_format ?wide format range man flow >>$ fun placeholders flow ->
    match placeholders with
    | None ->
      raise_c_invalid_format_arg_type_wo_info_alarm ~bottom:false range man flow |>
      raise_c_insufficient_format_args_wo_info_alarm ~bottom:false range man |>
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
          | [], [] -> Post.return flow

          | ph :: tlp, arg :: tla ->
            check_arg arg ph range man flow >>$ fun () flow ->
            iter tlp tla flow

          | [], arg :: tla ->
            man.eval arg flow >>$ fun _ flow ->
            iter [] tla flow

          | _ -> assert false
        in
        iter placeholders args flow



  (** Evaluation entry point *)
  let eval exp man flow =
    match ekind exp with

    (* 𝔼⟦ printf(...) ⟧ *)
    | E_c_builtin_call("printf", format :: args)
    | E_c_builtin_call("__printf_chk", _ :: format :: args) ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ fprintf(...) ⟧ *)
    | E_c_builtin_call("fprintf", stream :: format :: args)
    | E_c_builtin_call("__fprintf_chk", stream :: _ :: format :: args) ->
      assert_valid_stream stream exp.erange man flow >>$? fun () flow ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ dprintf(...) ⟧ *)
    | E_c_builtin_call("dprintf", file:: format :: args) ->
      assert_valid_file_descriptor file exp.erange man flow >>$? fun () flow ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ sprintf(...) ⟧ *)
    | E_c_builtin_call("sprintf", dst :: format :: args)
    | E_c_builtin_call("__sprintf_chk", dst :: _ :: _ :: format :: args)
    | E_c_builtin_call("__builtin___sprintf_chk", dst :: _ :: _ :: format :: args) ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      memrand dst (mk_zero ~typ:ul exp.erange) (mk_top ul exp.erange) exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ snprintf(...) ⟧ *)
    | E_c_builtin_call("snprintf", dst :: n :: format :: args) ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      strnrand dst n exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ asprintf(...) ⟧ *)
    | E_c_builtin_call("asprintf", dst :: format :: args) ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      asprintf_stub dst exp.erange man flow |>
      Rewrite.return_eval |>
      OptionExt.return

    (* 𝔼⟦ error(...) ⟧ *)
    | E_c_builtin_call("error", status :: errnum :: format :: args) ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      error_error status exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ error_at_line(...) ⟧ *)
    | E_c_builtin_call("error_at_line", status :: errnum :: filename :: linenum :: format :: args) ->
      check_args format args exp.erange man flow >>$? fun () flow ->
      error_error_at_line status filename exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ wprintf(...) ⟧ *)
    | E_c_builtin_call("wprintf", format :: args) ->
      check_args ~wide:true format args exp.erange man flow >>$? fun () flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      Rewrite.return_eval |>
      OptionExt.return

    (* 𝔼⟦ fwprintf(...) ⟧ *)
    | E_c_builtin_call("fwprintf", stream :: format :: args) ->
      assert_valid_stream stream exp.erange man flow >>$? fun () flow ->
      check_args ~wide:true format args exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ swprintf(...) ⟧ *)
    | E_c_builtin_call("swprintf", dst :: n :: format :: args) ->
      check_args ~wide:true format args exp.erange man flow >>$? fun () flow ->
      wcsnrand dst n exp.erange man flow >>$? fun () flow ->
      Rewrite.reval_singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    | _ -> None

  let ask _ _ _  = None

end

let () =
  register_stateless_domain (module Domain)
