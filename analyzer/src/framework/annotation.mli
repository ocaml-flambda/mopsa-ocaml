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

(** Annotations are used to add extra-information to flows.
    They are implemented as polymorphic maps so that annotations can be defined
    on top of the global abstraction.
*)

type ('a, _) key = ..
(** Type [('a, 'b) key] defines a key for a cell of type ['b] that can
   be polymorphic on ['a] *)

type 'a annot
(** Type of annotations over a parameter type ['a] *)


(*==========================================================================*)
(**                      {2 Registering  keys}                              *)
(*==========================================================================*)

type (_, _) eq = Eq : ('a, 'a) eq
(** Equality witness *)

type ('a, 'b) sf = {
  eq : 'c. ('a, 'c) key -> ('b, 'c) eq option;
  print : Format.formatter -> 'b -> unit;
}
(** [('a, 'b) sf] defines a stateful annotation with an equality
   witness checker and a pretty printer *)

val register_stateful_annot : ('a, 'b) sf -> 'a annot -> 'a annot
(** [register_stateful_annot w a] registers a new kind of stateful annotations to an
   annotation map [a], defined with a witness checker [w] *)


type 'b sl = {
  eq : 'a 'c. ('a, 'c) key -> ('b, 'c) eq option;
  print : Format.formatter -> 'b -> unit;
}
(** [('a, 'b) sl] defines a stateless annotation with an equality
   witness checker and a pretty printer *)

val register_stateless_annot : 'b sl -> unit -> unit
(** [register_stateless_annot w ()] registers a new kind of stateless annotations to an
   annotation map [a], defined with a witness checker [w] *)


(*==========================================================================*)
(**                     {2 Managing annotations}                            *)
(*==========================================================================*)

exception Key_not_found

val empty : 'a annot
(** Empty annotation *)

val add : ('a, 'b) key -> 'b -> 'a annot -> 'a annot
(** [add k v a] returns a new annotation equal to [m] having a binding
   from key [k] to value [v]. Previous binding is removed if
   present. Raises [Key_not_found] if [k] was not registered in [m]
   *)

val find : ('a, 'b) key -> 'a annot -> 'b
(** [find k m] returns the value bound to [k] in [m]. Raises
    [Not_found] if the binding is not found. *)

val remove : ('a, 'b) key -> 'a annot -> 'a annot
(** [remove k m] removes binding with key [k] in [m]. Does nothing if
    there was no such binding. *)

val mem : ('a, 'b) key -> 'a annot -> bool
(** [mem k m] returns [true] if key [k] is bound to an annotation in
   [m], or [false] otherwise. *)

val cardinal : 'a annot -> int
(** Number of values in a map *)

val print : Format.formatter -> 'a annot -> unit
(** Pretty print an annotation *)
