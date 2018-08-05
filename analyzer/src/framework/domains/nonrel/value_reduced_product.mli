(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** N-ary reduced product of non-relational value abstractions. *)

open Value

type 't pool

(** Pool manager defines point-wise lattice operators for the product
      value abstraction. It also provides get/set functions to access
      individual values abstractions via keys *)
type 'a pool_man = {
  bottom : 'a;
  top : 'a;
  is_bottom : 'a -> bool;
  subset : 'a -> 'a -> bool;
  join : 'b. 'b Annotation.t -> 'a -> 'a -> 'a;
  meet : 'b. 'b Annotation.t -> 'a -> 'a -> 'a;
  widen : 'b. 'b Annotation.t -> 'a -> 'a -> 'a;
  print : Format.formatter -> 'a -> unit;
  get : 't. 't value -> 'a -> 't;
  set : 't. 't value -> 't -> 'a -> 'a;
}

(** Signature for reduction rules *)
module type REDUCTION =
sig

  (** Reduction operator called after each point-wise application of
     transfer functions (unop, bwd_unop, etc.) *)
  val reduce : 'a pool_man -> 'a -> 'a
end


type _ value +=
  | V_empty : unit value
  | V_reduced_product : 'a value * 'b value -> ('a * 'b) value


val register_reduction : string -> (module REDUCTION) -> unit
val find_reduction : string -> (module REDUCTION)

val of_string : string list -> string list -> string -> (module Value.VALUE)

(* Utility functions *)
type 'a fld = {
  doit : 't. 'a -> 't value -> 'a;
}

val fold : 'a fld -> 'a -> 't pool -> 'a
