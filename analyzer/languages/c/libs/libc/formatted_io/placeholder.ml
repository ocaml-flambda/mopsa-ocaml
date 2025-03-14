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
open Format
open Framework.Core.Ast.Typ

(** C type of a placeholder *)
type placeholder_type =
  | Int of c_integer_type
  | Float of c_float_type
  | Pointer
  | String
  | WideString

let pp_placeholder_type fmt = function
  | Int ct -> fprintf fmt "Int %a" pp_typ (T_c_integer ct)
  | Float ft -> fprintf fmt "Float %a" pp_typ (T_c_float ft)
  | Pointer -> fprintf fmt "Ptr"
  | String -> fprintf fmt "String"
  | WideString -> fprintf fmt "WideString"

(** Placeholder for output streams, e.g. printf *)
type output_placeholder = {
  op_width: int option;
  op_precision: int option;
  op_typ: placeholder_type;
}

let pp_output_placeholder fmt op =
  Format.fprintf fmt "{%a;_;_}" pp_placeholder_type op.op_typ 

type output_format =
   | String of string
   | Placeholder of output_placeholder

(** Placeholder for input streams, e.g. scanf *)
type input_placeholder = {
  ip_width: int option;
  ip_typ: placeholder_type;
}
  
let pp_input_placeholder fmt ip =
  Format.fprintf fmt "{%a;_;_}" pp_placeholder_type ip.ip_typ 

  
