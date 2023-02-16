(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

open Universal.Ast
open Mopsa

module Rangeset = Set.Make(struct type t = range
    let compare = compare_range end)

let is_data_container_chain = ref (fun (a:addr_kind) -> false)

let register_is_data_container (is_dc:('a -> bool) -> 'a -> bool) =
  is_data_container_chain := is_dc !is_data_container_chain

let is_data_container ak = !is_data_container_chain ak

let join_akind_chain = ref (fun (ak1: addr_kind) (ak2: addr_kind) -> failwith "join_akind NotImplemented")

let register_join_akind (join_akind_f: ('a -> 'a -> 'a) -> 'a -> 'a -> 'a) =
  join_akind_chain := join_akind_f !join_akind_chain

let join_akind ak1 ak2 = !join_akind_chain ak1 ak2
