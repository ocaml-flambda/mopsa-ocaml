(**
  Relation - Relations (or multimaps) between ordered sets.

  Copyright (C) 2018 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)

open SetExtSig

module Make(Ord: OrderedType) : S with type elt = Ord.t
(** Generic functor to build a set data-type from ordered elements.
 *)     

val printer_default : set_printer
(** Print as set: {elem1,...,elemn}. *)

module StringSet   : S with type elt = string
module IntSet      : S with type elt = int
module Int32Set    : S with type elt = int32
module Int64Set    : S with type elt = int64
module ZSet        : S with type elt = Z.t
(** A few useful set instances. *)
           
