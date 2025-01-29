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

include Arg_complete

type arg = {
  key: string;
  doc: string;
  category: string;
  default: string;
  spec: spec;
}

(* Extract relevant tuple for Stdlib.Arg (from above record) *)
let argext_to_arg arg = arg.key, Arg_complete.arg_spec arg.spec, arg.doc

(* Lifting to list *)
let argext_to_arg_list = List.map argext_to_arg


(* FIXME: Set_string_list: if called multiple times, should we accumulate? How to bash-complete? *)

(* Lifter to parse comma-separated string list from Stdlib.Arg.String spec *)
let string_list_lifter (spec_f : string list -> unit) : string -> unit =
  fun s -> spec_f (String.split_on_char ',' s)

(* Lifter to store comma-separated string list in ref, using Stdlib.Arg.string spec *)
let set_string_list_lifter (r : string list ref) : string -> unit =
  fun s -> r := String.split_on_char ',' s
