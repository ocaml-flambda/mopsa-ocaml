(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Annotations are used to add extra-information to abstract
   elements. They are implemented as polymorphic maps to allow define
   annotations on top of the global abstraction.
*)

type ('a, _) key = ..
(** [('a, 'b) key] is a key for cells of type ['b] that can be
   polymorphic on ['a] *)

type 'a t
(** Type of annotations over an abstract type ['a] *)


(*==========================================================================*)
(**                      {2 Registering  keys}                              *)
(*==========================================================================*)

type (_, _) eq
(** Equality predicate of two types *)


type ('a, 'b) w = {
  eq : 'c. ('a, 'c) key -> ('b, 'c) eq option;
}
(** [('a, 'b) w] defines an equality witness of a key [('a, 'b) key] *)

val register : ('a, 'b) w -> 'a t -> 'a t
(** [register w a] adds a new equality witness [w] to the annotation map [m] *)


(*==========================================================================*)
(**                     {2 Managing annotations}                            *)
(*==========================================================================*)

exception Key_not_found

val empty : 'a t
(** Empty annotation *)

val add : ('a, 'b) key -> 'b -> 'a t -> 'a t
(** [add k v a] returns a new annotation equal to [m] having a binding
   of key [k] to value [v]. Previous binding is removed if
   present. Raises [Key_not_found] if [k] was not registered in [m]
   *)

val find : ('a, 'b) key -> 'a t -> 'b
(** [find k m] returns the values bound to [k] in [m]. Raises
    [Not_found] if the binding is not found. *)
