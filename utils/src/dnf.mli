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

val mk_true : 'a t

val mk_false : 'a t

val is_true : 'a t -> bool

val is_false : 'a t -> bool

val is_empty : 'a t -> bool

val mk_and : 'a t -> 'a t -> 'a t

val mk_or : 'a t -> 'a t -> 'a t

val mk_neg : ('a -> 'a t) -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val map_conjunction : ('a list -> 'b list) -> 'a t -> 'b t

val map_disjunction : ('a list -> 'b list) -> 'a t -> 'b t

val iter : ('a -> unit) -> 'a t -> unit

val reduce : ('a -> 'b) -> join:('b -> 'b -> 'b) -> meet:('b -> 'b -> 'b) -> 'a t -> 'b

val fold_reduce : ('a -> 'b -> 'a * 'c) -> join:('c -> 'c -> 'c) -> meet:('c -> 'c -> 'c) -> 'a -> 'b t -> 'a * 'c

val reduce_conjunction : ('a list -> 'b) -> join:('b -> 'b -> 'b) -> 'a t -> 'b

val fold_reduce_conjunction : ('a -> 'b list -> 'a * 'c) -> join:('c -> 'c -> 'c) -> 'a -> 'b t -> 'a * 'c

val reduce_disjunction : ('a list -> 'b) -> meet:('b -> 'b -> 'b) -> 'a t -> 'b

val fold_reduce_disjunction : ('a -> 'b list -> 'a * 'c) -> meet:('c -> 'c -> 'c) -> 'a -> 'b t -> 'a * 'c

val bind : ('a -> 'b t) -> 'a t -> 'b t

val fold_bind : ('a -> 'b -> 'a * 'c t) -> 'a -> 'b t -> 'a * 'c t

val bind_conjunction : ('a list -> 'b t) -> 'a t -> 'b t

val fold_bind_conjunction : ('a -> 'b list -> 'a * 'c t) -> 'a -> 'b t -> 'a * 'c t

val bind_disjunction : ('a list -> 'b t) -> 'a t -> 'b t

val fold_bind_disjunction : ('a -> 'b list -> 'a * 'c t) -> 'a -> 'b t -> 'a * 'c t

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val partition : ('a -> bool) -> 'a t -> 'a t option * 'a t option

val choose :'a t -> 'a option

val to_list : 'a t -> 'a list list

val from_list : 'a list list -> 'a t

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val cardinal : 'a t -> int
