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


(** Entry point for parsing format strings *)

open Mopsa
open Framework.Core.Sig.Domain.Manager
open Universal.Ast
open Ast
open Placeholder
open Ast
open Zone
open Common.Points_to
open Common.Alarms
open Common.Base


(** Evaluate an expression of format string into a string *)
let eval_format_string format range man flow =
  man.eval ~zone:(Z_c,Z_c_points_to) format flow >>$ fun pt flow ->
  let man' = Core.Sig.Stacked.Manager.of_domain_man man in
  match ekind pt with
  | E_c_points_to P_null ->
    raise_c_null_deref_alarm format man' flow |>
    Cases.empty_singleton

  | E_c_points_to P_invalid ->
    raise_c_invalid_deref_alarm format man' flow |>
    Cases.empty_singleton

  | E_c_points_to (P_block ({ base_kind = Addr addr; base_valid = false; base_invalidation_range = Some r}, _, _)) ->
    raise_c_use_after_free_alarm format r man' flow |>
    Cases.empty_singleton

  | E_c_points_to (P_block ({ base_kind = String (fmt,_,t) }, offset, _)) when sizeof_type t = Z.one  ->
    if is_c_expr_equals_z offset Z.zero then
      Cases.singleton fmt flow
    else
      assume (mk_binop offset O_eq (mk_zero (erange offset)) (erange offset))
        ~fthen:(fun flow -> Cases.singleton fmt flow)
        ~felse:(fun flow ->
            Soundness.warn_at range "unsupported format string: non-constant";
            Cases.empty_singleton flow)
        ~zone:Z_c_scalar man flow

  | _ ->
    Soundness.warn_at range "unsupported format string: non-constant";
    Cases.empty_singleton flow


(** Parse a format according to parser *)
let parse_format parser (format:expr) range man flow =
  eval_format_string format range man flow >>$ fun fmt flow ->
  let lex = Lexing.from_string fmt in
  try
    let placeholders = parser Lexer.read lex in
    Cases.singleton placeholders flow
  with _ ->
    (* lexer / parser error *)
    Soundness.warn_at range "unsupported format string: char %i of \"%s\""
      lex.lex_start_p.pos_cnum fmt;
    Cases.empty_singleton flow

(** Parse an output format *)
let parse_output_format (format:expr) range man flow =
  parse_format Parser.parse_output_format format range man flow

(** Parse an input format *)
let parse_input_format (format:expr) range man flow =
  parse_format Parser.parse_input_format format range man flow
