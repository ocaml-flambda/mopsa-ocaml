(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Functor abstract domains. *)

module type FUNCTOR = functor(_ : Stateful.DOMAIN) -> Stateful.DOMAIN

let functors : (string * (module FUNCTOR)) list ref = ref []

let register_domain name modl = functors := (name, modl) :: !functors

let find_domain name = List.assoc name !functors

let return x = Some x
let fail = None
