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

type 'a compare_chain = ('a -> 'a -> int) ref

type 'a print = (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

type 'a print_chain = (Format.formatter -> 'a -> unit) ref

let mk_compare_chain (default:'a -> 'a -> int) : 'a compare_chain =
  ref default

let mk_print_chain (default:Format.formatter -> 'a -> unit) : 'a print_chain =
  ref default

let register_compare (compare:'a compare) (chain:' compare_chain) : unit =
  chain := compare !chain

let register_print (print:'a print) (chain:' print_chain) : unit =
  chain := print !chain

type 'a info = {
  compare : 'a compare;
  print   : 'a print;
}

let register info compare_chain print_chain =
  register_compare info.compare compare_chain;
  register_print info.print print_chain

let compare (chain:'a compare_chain) x y =
  !chain x y

let print (chain:'a print_chain) fmt x =
  !chain fmt x
