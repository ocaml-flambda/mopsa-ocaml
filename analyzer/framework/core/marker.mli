(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2021 The MOPSA Project.                               *)
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

(** Trace markers *)

open Ast.Stmt
open Ast.Visitor
open Mopsa_utils.Location

type marker = ..

type marker_info = {
  marker_name : (marker -> string) -> marker -> string;
  marker_print : (Format.formatter -> marker -> unit) -> Format.formatter -> marker -> unit;
  marker_compare : (marker -> marker -> int) -> marker -> marker -> int;
}

val compare_marker : marker -> marker -> int

val pp_marker : Format.formatter -> marker -> unit

val get_marker_name : marker -> string

val register_marker : marker_info -> unit

type stmt_kind +=
  | S_add_marker of marker

val mk_add_marker : marker -> range -> stmt

val enable_marker : string -> unit

val disable_marker : string -> unit

val is_marker_enabled : marker -> bool
