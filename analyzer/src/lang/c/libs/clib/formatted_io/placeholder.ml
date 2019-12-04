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

(** Types corresponding to placeholders of format strings *)

open Ast

(** C type of a placeholder *)
type placeholder_type =
  | Int of c_integer_type
  | Float of c_float_type
  | Pointer
  | String

(** Placeholder for output streams, e.g. printf *)
type output_placeholder = {
  op_width: int option;
  op_precision: int option;
  op_typ: placeholder_type;
}

(** Placeholder for input streams, e.g. scanf *)
type intput_placeholder = {
  ip_width: int option;
  ip_typ: placeholder_type;
}
  

  
