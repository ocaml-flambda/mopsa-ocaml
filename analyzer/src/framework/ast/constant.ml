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

(** Extensible type of constants *)

open Typ

type constant = ..

type constant +=
  | C_top of typ (** top value of a specific type *)

let constant_compare_chain = TypeExt.mk_compare_chain (fun c1 c2 ->
    match c1, c2 with
    | C_top t1, C_top t2 -> compare_typ t1 t2
    | _ -> compare c1 c2
  )

let constant_pp_chain = TypeExt.mk_print_chain (fun fmt c ->
    match c with
    | C_top T_any -> Format.fprintf fmt "⊤"
    | C_top t -> Format.fprintf fmt "⊤:%a" pp_typ t
    | _ -> Exceptions.panic "constant_pp_chain: unknown constant"
  )

let register_constant (info: constant TypeExt.info) : unit =
  TypeExt.register info constant_compare_chain constant_pp_chain

let register_constant_compare cmp = TypeExt.register_compare cmp constant_compare_chain

let register_constant_pp pp = TypeExt.register_print pp constant_pp_chain


let compare_constant o1 o2 = TypeExt.compare constant_compare_chain o1 o2

let pp_constant fmt constant = TypeExt.print constant_pp_chain fmt constant
