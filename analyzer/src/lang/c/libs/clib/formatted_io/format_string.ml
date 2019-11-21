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


(** Parser entry point for parsing format strings *)

open Mopsa
open Framework.Core.Sig.Domain.Manager
open Ast
open Placeholder
open Ast
open Zone
open Common.Points_to
open Common.Alarms


let eval_format_string format range man flow =
  man.eval ~zone:(Z_c,Z_c_points_to) format flow >>$ fun pt flow ->
  let man' = Core.Sig.Stacked.Manager.of_domain_man man in
  match ekind pt with
  | E_c_points_to P_null ->
    raise_c_null_deref_alarm format range man' flow |>
    Result.empty_singleton

  | E_c_points_to P_invalid ->
    raise_c_invalid_deref_alarm format range man' flow |>
    Result.empty_singleton

  | E_c_points_to (P_block (D (_,r), _)) ->
    raise_c_use_after_free_alarm format r range man' flow |>
    Result.empty_singleton

  | E_c_points_to (P_block (S fmt, offset)) when is_c_expr_equals_z offset Z.zero ->
    Result.singleton fmt flow

  | _ ->
    Soundness.warn_at range "unsupported format string";
    Result.empty_singleton flow


let parse_fprintf_format (format:expr) range man flow =
  eval_format_string format range man flow >>$ fun fmt flow ->
  let placeholders = Parser.parse_fprintf Lexer.read (Lexing.from_string fmt) in
  Result.singleton placeholders flow



let parse_fscanf_format (format:expr) range man flow =
  eval_format_string format range man flow >>$ fun fmt flow ->
  let placeholders = Parser.parse_fscanf Lexer.read (Lexing.from_string fmt) in
  Result.singleton placeholders flow
