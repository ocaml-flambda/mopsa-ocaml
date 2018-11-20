(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** A simple graph library to represent control-flow graphs.
    Functor and module signature.
 *)

open GraphSig


(*==========================================================================*)
                 (** {2 Ordered, hashable data types} *)
(*==========================================================================*)


(** Generic functor to lift any type to an ID_TYPE module.
    Uses the polymorphic comparison, equality, and hashing.
 *)
module IdGeneric(T : sig type t end) : ID_TYPE with type t = T.t

(** Useful base cases. *)
module IdInt    : ID_TYPE with type t = int
module IdString : ID_TYPE with type t = string
module IdUnit   : ID_TYPE with type t = unit

(** Product. *)         
module IdPair(A:ID_TYPE)(B:ID_TYPE) : ID_TYPE with type t = A.t * B.t

  

(*==========================================================================*)
                    (** {2 Nested lists} *)
(*==========================================================================*)


(** Printers. *)
val pp_nested_list: (Format.formatter -> 'a -> unit) -> Format.formatter ->
                    'a nested_list -> unit

val pp_nested_list_list: (Format.formatter -> 'a -> unit) -> Format.formatter ->
                         'a nested_list list -> unit


     
(*==========================================================================*)
                  (** {2 Graph Functor} *)
(*==========================================================================*)


(** Main functor. *)
module Make(P:P) : S with module P = P




