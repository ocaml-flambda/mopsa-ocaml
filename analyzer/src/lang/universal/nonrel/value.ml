(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Value abstraction for non-relational domains. *)

open Framework.Lattice
open Framework.Ast

module type VALUE =
sig

  (*==========================================================================*)
                        (** {2 Lattice structure} *)
  (*==========================================================================*)

  include LATTICE

  val of_constant : constant -> t
  (** Create a singleton abstract value from a constant. *)


  (*==========================================================================*)
                          (** {2 Unary operators} *)
  (*==========================================================================*)

  val fwd_unop : operator -> t -> t
  (** Forward evaluation of unary operators. *)


  val bwd_unop : operator -> t -> t -> t
  (** Backward evaluation of unary operators. *)


  (*==========================================================================*)
                          (** {2 Binary operators} *)
  (*==========================================================================*)

  val fwd_binop : operator -> t -> t -> t
  (** Forward evaluation of binary operators. *)

  val bwd_binop : operator -> t -> t -> t -> t * t
  (** Backward evaluation of binary operators. *)


  (*==========================================================================*)
                         (** {2 Boolean comparisons} *)
  (*==========================================================================*)

  val fwd_filter : operator -> t -> t -> bool
  (** Forward evaluation of boolean comparisons. *)

  val bwd_filter : operator -> t -> t -> t * t
  (** Backward evaluation of boolean comparisons. *)

  val assume_true : t -> t
  (** Filter values that can evaluate to true. *)

  val assume_false : t -> t
  (** Filter values that can evaluate to false. *)

  val can_be_true : t -> bool
  (** Test whether a value can evaluate to true. *)

  val can_be_false : t -> bool
  (** Test whether a value can evaluate to false. *)

end
