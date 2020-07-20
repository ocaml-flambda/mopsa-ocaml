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


(** Wiring - Links between a semantic dependency and its providers *)

open Semantic

type wirings
(** Wirings table providing the domains that implement a given semantic
    dependency *) 

type domain = string
(** Domains are designated by their names *)

val empty_wirings : wirings
(** Empty wirings table *)

val find_wirings : semantic -> wirings -> domain list
(** Find providers of a given semantic dependency *)

val add_wiring : semantic -> domain -> wirings -> wirings
(** Add a wiring linking a semantic and its provider *)

val add_wirings : semantic -> domain list -> wirings -> wirings
(** Add a set of wirings linking a semantic and its providers *)

val join_wirings : wirings -> wirings -> wirings
(** Join two wirings table *)

val pp_wirinings : Format.formatter -> wirings -> unit
(** Print a wiring table *)
