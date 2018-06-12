(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of values. *)

module type VALUE =
sig

  (*==========================================================================*)
                        (** {2 Lattice structure} *)
  (*==========================================================================*)

  include Lattice.LATTICE

  val of_constant : Ast.constant -> t
  (** Create a singleton abstract value from a constant. *)

  val of_bool : bool option -> t

  (*==========================================================================*)
                          (** {2 Forward semantics} *)
  (*==========================================================================*)

  val fwd_unop : Ast.operator -> t -> t
  (** Forward evaluation of unary operators. *)

  val fwd_binop : Ast.operator -> t -> t -> t
  (** Forward evaluation of binary operators. *)


  (*==========================================================================*)
                          (** {2 Backward operators} *)
  (*==========================================================================*)

  val bwd_unop : Ast.operator -> t -> t -> t
  (** Backward evaluation of unary operators. *)

  val bwd_binop : Ast.operator -> t -> t -> t -> t * t
  (** Backward evaluation of binary operators. *)


  (*==========================================================================*)
                         (** {2 Boolean comparisons} *)
  (*==========================================================================*)

  val fwd_filter : Ast.operator -> t -> t -> bool
  (** Forward evaluation of boolean comparisons. *)

  val bwd_filter : Ast.operator -> t -> t -> t * t
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
