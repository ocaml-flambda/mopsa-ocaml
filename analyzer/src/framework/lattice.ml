(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Signature module of a lattice. *)
module type LATTICE =
sig

  (** {2 Structure} *)

  type t
  (** Type of the abstract elements of the lattice. *)

  val bottom: t
  (** Least element of the lattice. *)

  val top: t
  (** Greatest element of the lattice. *)


  (** {2 Predicates} *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val is_top: t -> bool
  (** [is_top a] tests whether [a] is top or not. *)

  val leq: t -> t -> bool
  (** Partial order relation.
      [leq a1 a2] tests whether [a1] is related to (or included in) [a2].
  *)

  (** {2 Operators} *)

  val join: 'a Annotation.t -> t -> t -> t
  (** [join annot a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: 'a Annotation.t -> t -> t -> t
  (** [join annot a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: 'a Annotation.t -> t -> t -> t
  (** [widening annot a1 a2] computes an upper bound of [a1] and [a2] that ensures
      stabilization of ascending chains. *)


  (** {2 Printing} *)

  val print: Format.formatter -> t -> unit
  (** Printer of an element of the lattice. *)


end
