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

(** Common functions for implementing an extensible chain of compare
    and print functions *)

type 'a compare_chain = ('a -> 'a -> int) ref

type 'a print_chain = (Format.formatter -> 'a -> unit) ref

let mk_compare_chain (default:'a -> 'a -> int) : 'a compare_chain =
  ref default

let mk_print_chain (default:Format.formatter -> 'a -> unit) : 'a print_chain =
  ref default

type 'a info = {
  compare : ('a -> 'a -> int) -> 'a -> 'a -> int;
  print   : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit;
}

let register info (compare_chain, print_chain) =
  compare_chain := info.compare !compare_chain;
  print_chain := info.print !print_chain

let compare (chain:'a compare_chain) x y =
  !chain x y

let print (chain:'a print_chain) fmt x =
  !chain fmt x
