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

(** Tokens identifying control flows *)

open Lattice
open Context


type token = ..
(** Flow tokens are used to distinguish between different control flows *)

type token += T_cur
(** Token of current (active) execution flow *)

val register_token : token TypeExt.info -> unit
(** Register a new token with its compare and print functions *)

val compare_token : token -> token -> int
(** Compare two tokens with a total order *)

val pp_token : Format.formatter -> token -> unit
(** Pretty printer of tokens *)

module TokenMap :
sig

  type +'a t

  val bottom : 'a t

  val top : 'a t

  val singleton: token -> 'a -> 'a t

  val is_bottom : 'a lattice -> 'a t -> bool

  val is_top : 'a lattice -> 'a t -> bool

  val subset : 'a lattice -> 'a t -> 'a t -> bool

  val join : 'a lattice -> 'a t -> 'a t -> 'a t

  val join_list : 'a lattice -> 'a t list -> 'a t

  val meet : 'a lattice -> 'a t -> 'a t -> 'a t

  val meet_list : 'a lattice -> 'a t list -> 'a t

  val widen : 'a lattice -> uctx -> 'a t -> 'a t -> 'a t

  val print : 'a lattice -> Format.formatter -> 'a t -> unit

  val mem : token -> 'a t -> bool

  val find : token -> 'a t -> 'a

  val find_opt : token -> 'a t -> 'a option

  val get : token -> 'a lattice -> 'a t -> 'a

  val set : token -> 'a -> 'a lattice -> 'a t -> 'a t

  val copy : token -> token -> 'a lattice -> 'a t -> 'a t -> 'a t

  val add : token -> 'a -> 'a lattice -> 'a t -> 'a t

  val remove : token -> 'a t -> 'a t

  val filter : (token -> 'a -> bool) -> 'a t -> 'a t

  val map : (token -> 'a -> 'b) -> 'a t -> 'b t

  val fold : ('b -> token -> 'a -> 'b)  -> 'b -> 'a t -> 'b

  val merge : (token -> 'a option -> 'a option -> 'a option) -> 'a lattice -> 'a t -> 'a t -> 'a t

  val neutral2 : (token -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val absorb2 : (token -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

end
