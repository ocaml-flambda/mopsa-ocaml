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

  let checks = [
    CHK_C_INSUFFICIENT_FORMAT_ARGS;
    CHK_C_INVALID_FORMAT_ARG_TYPE
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
          safe_c_format_arg_type range man flow
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
          safe_c_format_arg_type range man flow
      in
      let exp = mk_c_cast arg typ arg.erange in
      man.eval exp flow >>$ fun _ flow ->
      Post.return flow

    | Pointer ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg (T_c_pointer void) man flow
        else
          safe_c_format_arg_type range man flow
      in
      Post.return flow

    | String ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg (T_c_pointer s8) man flow
        else
          safe_c_format_arg_type range man flow
      in
      assert_valid_string arg range man flow

    | WideString ->
      let flow =
        if not (is_c_pointer_type arg.etyp) then
          raise_c_invalid_format_arg_type_alarm arg (T_c_pointer s8) man flow
        else
          safe_c_format_arg_type range man flow
      in
      assert_valid_wide_string arg range man flow


  (** Check that arguments correspond to the format *)
  let check_args ?wide format args range man flow =
    parse_output_format ?wide format range man flow >>$ fun oof flow ->
    match oof with
    | None ->
      raise_c_invalid_format_arg_type_warning range man flow |>
      raise_c_insufficient_format_args_warning range man |>
      Post.return

    | Some output_format ->
      let placeholders = List.filter_map (function
          | String s -> None
          | Placeholder p -> Some p) output_format in
      let nb_required = List.length placeholders in
      let nb_given = List.length args in
      if nb_required > nb_given then
        raise_c_insufficient_format_args_alarm nb_required nb_given range man flow |>
        Post.return
      else
        let rec iter placeholders args flow =
          match placeholders, args with
          | [], [] ->
            safe_c_format_args_number range man flow |>
            Post.return

          | ph :: tlp, arg :: tla ->
            check_arg arg ph range man flow >>%
            iter tlp tla

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
      check_args format args exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ fprintf(...) ⟧ *)
    | E_c_builtin_call("fprintf", stream :: format :: args)
    | E_c_builtin_call("__fprintf_chk", stream :: _ :: format :: args) ->
      assert_valid_stream stream exp.erange man flow >>%? fun flow ->
      check_args format args exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ dprintf(...) ⟧ *)
    | E_c_builtin_call("dprintf", file:: format :: args) ->
      assert_valid_file_descriptor file exp.erange man flow >>%? fun flow ->
      check_args format args exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ sprintf(...) ⟧ *)
    | E_c_builtin_call("sprintf", dst :: format :: args)
    | E_c_builtin_call("__sprintf_chk", dst :: _ :: _ :: format :: args)
    | E_c_builtin_call("__builtin___sprintf_chk", dst :: _ :: _ :: format :: args) ->
      check_args format args exp.erange man flow >>%? fun flow ->
      memrand dst (mk_zero ~typ:ul exp.erange) (mk_top ul exp.erange) exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ snprintf(...) ⟧ *)
    | E_c_builtin_call("snprintf", dst :: n :: format :: args) ->
      check_args format args exp.erange man flow >>%? fun flow ->
      strnrand dst n exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ asprintf(...) ⟧ *)
    | E_c_builtin_call("asprintf", dst :: format :: args) ->
      check_args format args exp.erange man flow >>%? fun flow ->
      asprintf_stub dst exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ vasprintf(...) ⟧ *)
    | E_c_builtin_call("vasprintf", dst :: format :: ap :: []) -> (*va_list) ->*)
      (* variadic checking is not performed, we split the analysis in two cases
         1. No % placeholders, constant string: we can do something precise
         2. % placeholders: warn the analysis is unsound in this case *)
      parse_output_format ~wide:false format exp.erange man flow >>$ (fun oof flow ->
          match oof with
          | None ->
            raise_c_invalid_format_arg_type_warning exp.erange man flow |>
            raise_c_insufficient_format_args_warning exp.erange man |>
            Cases.singleton (mk_top (T_c_integer C_signed_int) (erange exp))

          | Some output_format ->
            List.iter (function
                | Placeholder p -> debug "placeholder %a" pp_output_placeholder p
                | String s -> debug "string '%s'" s) output_format;
            let placeholders, strings = List.partition_map (function
                | Placeholder p -> Left p
                | String s -> Right s) output_format in
            if List.length placeholders = 0 && List.length strings <= 1 then
              (* case 1 *)
              vasprintf_stub true (mk_c_string (match strings with [] -> "" | hd :: _ -> hd) exp.erange) dst exp.erange man flow
            else
              (* case 2 *)
              vasprintf_stub false format dst exp.erange man flow >>$ fun ret flow ->
              let range = erange exp in 
              let exception UnsupportedFormat in
              let rec process flow ret_value fmt =
                match fmt with
                | [] -> Cases.singleton ret_value flow 
                | String s :: tl -> process flow (add ~typ:(T_c_integer (C_signed_int)) ret_value (mk_int (String.length s) range) range) tl
                | Placeholder { op_typ = String } :: tl ->
                  man.eval (mk_expr (E_c_var_args ap) ~etyp:(T_c_pointer (T_c_integer (C_signed_char))) range) flow >>$ fun arg flow ->
                  assume (eq arg (mk_c_null range) range) man flow
                    (* if one string is NULL, this counts as an internal error: -1 is returned *)
                    ~fthen:(fun flow -> Cases.singleton (mk_int (-1) ~typ:(T_c_integer C_signed_int) range) flow)
                    ~felse:(fun flow -> 
                        man.eval
                          (mk_c_call (find_c_fundec_by_name "strlen" flow)
                             [arg]
                             range) flow >>$ fun sl flow ->
                        process flow (add ret_value sl range) tl)
                | Placeholder { op_typ = Int t; op_width = None; op_precision=None; } :: tl ->
                  let range_l, range_u = rangeof (T_c_integer t) flow in 
                  process flow (add ret_value (mk_int_interval 1 (max (String.length @@ Z.to_string range_l) (String.length @@ Z.to_string range_u)) range) range) tl
                | _ -> raise UnsupportedFormat
              in
              try
                 process flow (mk_zero ~typ:(T_c_integer (C_signed_int)) range) output_format >>$ fun return flow ->
                 man.exec (mk_assume (eq ret return range) range) flow >>% Eval.singleton ret
              with UnsupportedFormat ->
                Eval.singleton ret flow 
        )
      |> OptionExt.return

    (* 𝔼⟦ error(...) ⟧ *)
    | E_c_builtin_call("error", status :: errnum :: format :: args) ->
      check_args format args exp.erange man flow >>%? fun flow ->
      error_error status exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ error_at_line(...) ⟧ *)
    | E_c_builtin_call("error_at_line", status :: errnum :: filename :: linenum :: format :: args) ->
      check_args format args exp.erange man flow >>%? fun flow ->
      error_error_at_line status filename exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ wprintf(...) ⟧ *)
    | E_c_builtin_call("wprintf", format :: args) ->
      check_args ~wide:true format args exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ fwprintf(...) ⟧ *)
    | E_c_builtin_call("fwprintf", stream :: format :: args) ->
      assert_valid_stream stream exp.erange man flow >>%? fun flow ->
      check_args ~wide:true format args exp.erange man flow >>%? fun flow ->
      Eval.singleton (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ swprintf(...) ⟧ *)
    | E_c_builtin_call("swprintf", dst :: n :: format :: args) ->
      check_args ~wide:true format args exp.erange man flow >>%? fun flow ->
      wcsnrand dst n exp.erange man flow >>%? fun flow ->
      man.eval (mk_top s32 exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ syslog(...) ⟧ *)
    | E_c_builtin_call("syslog", priority :: format :: args) ->
      check_args format args exp.erange man flow >>%? fun flow ->
      Eval.singleton (mk_unit exp.erange) flow |>
      OptionExt.return

    | _ -> None

  let ask _ _ _  = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
