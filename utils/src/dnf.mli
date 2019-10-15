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

(** Disjunctive normal form. *)

type 'a t


val singleton : 'a -> 'a t


val mk_and : 'a t -> 'a t -> 'a t
  

val mk_or : 'a t -> 'a t -> 'a t


val mk_neg : ('a -> 'a t) -> 'a t -> 'a t


val map : ('a -> 'b) -> 'a t -> 'b t


val iter : ('a -> unit) -> 'a t -> unit


val apply : ('a -> 'b) -> ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) -> 'a t -> 'b


val apply_list : ('a -> 'b) -> ('c list -> 'd) -> ('b list -> 'c) -> 'a t -> 'd


val bind : ('a -> 'b t) -> 'a t -> 'b t


val fold : ('b -> 'a -> 'b) -> ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b


val map_fold : ('c -> 'a -> 'b * 'c) -> 'c -> 'a t -> 'b t * 'c


val fold_apply : ('b -> 'a -> 'b * 'c) -> ('c -> 'c -> 'c) -> ('c -> 'c -> 'c) -> 'b -> 'a t -> 'b * 'c


val choose :'a t -> 'a option


val to_list : 'a t -> 'a list list

val from_list : 'a list list -> 'a t


val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit


val merge : ('a -> 'b -> 'a t option * 'b t option) -> 'a t -> 'b t -> 'a t option * 'b t option


val merge_fold  : ('c -> 'a -> 'b -> 'a t option * 'b t option * 'c) -> 'c -> 'a t -> 'b t -> 'a t option * 'b t option * 'c


val cardinal : 'a t -> int
