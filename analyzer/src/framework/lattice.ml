(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open Context


(** Signature of a lattice module.*)
module type LATTICE =
sig

  (** {2 Structure} *)

  type t
  (** Type of the elements of the lattice. *)

  val bottom: t
  (** Least element of the lattice. *)

  val top: t
  (** Greatest element of the lattice. *)


  (** {2 Predicates} *)

  val is_bottom: t -> bool
  (** Test whether a value is bottom or not. *)

  val is_top: t -> bool
  (** Test whether a value is top or not. *)

  val leq: t -> t -> bool
  (** Partial order relation.
      [leq a1 a2] tests whether [a1] is related to (or included in) [a2].
  *)

  (** {2 Operators} *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [join a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widening: context -> t -> t -> t
  (** [widening a1 a2] computes an upper bound of [a1] and [a2] that ensures
      stabilization of ascending chains. *)


  (** {2 Printing} *)

  val print: Format.formatter -> t -> unit
  (** Printer of an element of the lattice. *)


end

(** Lattice manager. *)
type 'a lattice_manager = {
  bottom : 'a;
  top : 'a;
  is_bottom : 'a -> bool;
  is_top : 'a -> bool;
  leq : 'a -> 'a -> bool;
  join : 'a -> 'a -> 'a;
  meet : 'a -> 'a -> 'a;
  widening : context -> 'a -> 'a -> 'a;
  print : Format.formatter -> 'a -> unit;
}
