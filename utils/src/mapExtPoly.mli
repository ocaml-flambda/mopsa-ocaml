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

(** Maps with polymorphic keys and values *)

type ('k,+'a) t

type 'k compare = 'k -> 'k -> int

val singleton : compare:'k compare -> 'k -> 'a -> ('k,'a) t

val empty : compare:'k compare -> ('k,'a) t

val is_empty : ('k,'a) t -> bool

val add : 'k -> 'a -> ('k,'a) t -> ('k,'a) t

val find : 'k -> ('k,'a) t -> 'a

val mem : 'k -> ('k,'a) t -> bool

val min_binding : ('k,'a) t -> ('k *'a)

val max_binding : ('k,'a) t -> ('k *'a)

val remove_min_binding : ('k,'a) t -> ('k,'a) t

val merge : ('k,'a) t -> ('k,'a) t -> ('k,'a) t

val remove : 'k -> ('k,'a) t -> ('k,'a) t

val iter : ('k -> 'a -> unit) -> ('k,'a) t -> unit

val map : ('a -> 'b) -> ('k,'a) t -> ('k,'b) t

val mapi : ('k -> 'a -> 'b) -> ('k,'a) t -> ('k,'b) t

val fold : ('k -> 'a -> 'b -> 'b) -> ('k,'a) t -> 'b -> 'b

val for_all : ('k -> 'a -> bool) -> ('k,'a) t -> bool

val exists : ('k -> 'a -> bool) -> ('k,'a) t -> bool

val filter : ('k -> 'a -> bool) -> ('k,'a) t -> ('k,'a) t

val partition : ('k -> 'a -> bool) -> ('k,'a) t -> ('k,'a) t * ('k,'a) t
