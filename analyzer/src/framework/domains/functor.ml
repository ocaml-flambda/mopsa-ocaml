(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Functor abstract domains. *)

open Domain

module type S =
sig
  val name : string
  module Make : functor(_ : DOMAIN) -> DOMAIN
end

let functors : (module S) list ref = ref []

let register_domain f = functors := f :: !functors

let rec find_domain name =
  let rec aux = function
  | [] -> raise Not_found
  | hd :: tl ->
    let module F = (val hd : S) in
    if F.name = name then (module F : S) else aux tl
  in
  aux !functors
