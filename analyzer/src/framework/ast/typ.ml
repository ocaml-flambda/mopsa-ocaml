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

(** Extensible type of types *)


type typ = ..

(** Basic types *)
type typ +=
  | T_any (** Generic unknown type. *)

let typ_compare_chain = TypeExt.mk_compare_chain (fun t1 t2 ->
    match t1, t2 with
    | T_any, T_any -> 0
    | _ -> compare t1 t2
  )

let typ_pp_chain = TypeExt.mk_print_chain (fun fmt typ ->
    match typ with
    | T_any -> Format.pp_print_string fmt "?"
    | _ -> Exceptions.panic "typ_pp_chain: unknown type"
  )

let register_typ (info: typ TypeExt.info) : unit =
  TypeExt.register info typ_compare_chain typ_pp_chain

let register_typ_compare cmp = TypeExt.register_compare cmp typ_compare_chain

let register_typ_pp pp = TypeExt.register_print pp typ_pp_chain

let compare_typ t1 t2 = TypeExt.compare typ_compare_chain t1 t2

let pp_typ fmt typ = TypeExt.print typ_pp_chain fmt typ
