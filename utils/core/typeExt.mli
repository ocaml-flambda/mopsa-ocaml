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

(** Common functions for extensible types *)

type 'a compare = ('a -> 'a -> int) -> 'a -> 'a -> int

type 'a compare_chain

type 'a print = (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

type 'a print_chain

val mk_compare_chain : ('a -> 'a -> int) -> 'a compare_chain

val mk_print_chain : (Format.formatter -> 'a -> unit) -> 'a print_chain

val register_compare : 'a compare -> 'a compare_chain -> unit

val register_print : 'a print -> 'a print_chain -> unit

type 'a info = {
  compare : 'a compare;
  print   : 'a print;
}

val register : 'a info -> 'a compare_chain -> 'a print_chain -> unit

val compare : 'a compare_chain -> 'a -> 'a -> int

val print : 'a print_chain -> Format.formatter -> 'a -> unit
