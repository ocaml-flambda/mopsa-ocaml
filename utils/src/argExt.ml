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

(** Command-line options, extends [Arg] module of standard library *)

type spec =
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Set of bool ref
  | Clear of bool ref

  | Int of (int -> unit)
  | Set_int of int ref

  | String of (string -> unit)
  | Set_string of string ref

  | Set_string_list of string list ref

  | Symbol of string list * (string -> unit)

type arg = {
  key: string;
  doc: string;
  category: string;
  default: string;
  spec: spec;
}

let from_spec =
  function
  | Unit f -> Arg.Unit f
  | Bool f -> Arg.Bool f
  | Set b -> Arg.Set b
  | Clear b -> Arg.Clear b
  | Int f -> Arg.Int f
  | Set_int n -> Arg.Set_int n
  | String f -> Arg.String f
  | Set_string s -> Arg.Set_string s
  | Set_string_list l -> Arg.String (fun s -> l := s :: !l)
  | Symbol (l, f) -> Arg.Symbol (l, f)

let from_arg (arg:arg) : (Arg.key*Arg.spec*Arg.doc) =
  arg.key, from_spec arg.spec, arg.doc

let parse (args:arg list) (handler:string -> unit) (usage:string) : unit =
  let args = List.map from_arg args in
  Arg.parse args handler usage
