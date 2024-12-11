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

(** Path of domains inside the abstraction DAG *)

open Mopsa_utils

type accessor = ..

type path = accessor list

let accessor_compare_chain : accessor TypeExt.compare_chain =
  TypeExt.mk_compare_chain (fun ax1 ax2 -> Stdlib.compare ax1 ax2)

let accessor_print_chain : accessor TypeExt.print_chain =
  TypeExt.mk_print_chain (fun fmt ax -> Exceptions.panic "unregistered path accessor")

let compare_accessor ax1 ax2 = TypeExt.compare accessor_compare_chain ax1 ax2

let pp_accessor fmt ax = TypeExt.print accessor_print_chain fmt ax

let register_accessor info = TypeExt.register info accessor_compare_chain accessor_print_chain

let compare_path p1 p2 = Compare.list compare_accessor p1 p2

let pp_path fmt = function
  | [] -> Format.pp_print_string fmt "ε"
  | p  ->
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "::")
      pp_accessor fmt p

let empty_path : path = []

module PathMap = MapExt.Make(struct
    type t = path
    let compare = compare_path
  end)

module PathSet = SetExt.Make(struct
    type t = path
    let compare = compare_path
  end)
