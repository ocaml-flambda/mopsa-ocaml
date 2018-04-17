(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Build a global abstract domain from a json configuration file.

   The supported syntax for a [domain] is as follows:
   - a string ["name"] denotes a leaf domain (level 0 or 1).

   - an object [\{"fun": "name", "arg": domain\}] is a functor domain
   (level 0 or 1) identified by its ["name"] and applied to
   an argument [domain].

   - an object [\{"product": \[domain list\]\}] creates a reduced product over
   a list of domains.

   - an object [\{"iter": \[domain list\]\}] iterates over a list of domains and
returns the first non-empty result.

*)

open Yojson.Basic
open Yojson.Basic.Util

let debug fmt = Debug.debug ~channel:"config" fmt

(** {2 Flat domains} *)

let rec build_domain = function
  | `String(name) -> build_leaf name
  | `Assoc(obj) when List.mem_assoc "fun" obj -> build_functor obj
  | `Assoc(obj) when List.mem_assoc "iter" obj -> build_iter @@ List.assoc "iter" obj
  | `Assoc(obj) when List.mem_assoc "product" obj -> build_product @@ List.assoc "product" obj
  | _ -> assert false

and build_leaf name =
  try Domains.Stateful.find_domain name
  with Not_found ->
    Debug.fail "Domain %s not found" name

and build_iter json =
  let domains = json |> to_list |> List.map build_domain in
  let rec aux :
    (module Domains.Stateful.DOMAIN) list ->
    (module Domains.Stateful.DOMAIN)
    = function
      | [] -> assert false
      | [d] -> d
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : Domains.Stateful.DOMAIN) in
        let module Tail = (val tl : Domains.Stateful.DOMAIN) in
        let module Dom = Domains.Composers.Iter.Make(Head)(Tail) in
        (module Dom : Domains.Stateful.DOMAIN)
  in
  aux domains

and build_product json =
  assert false

and build_functor assoc =
  let arg = List.assoc "arg" assoc in
  let a = build_domain arg in
  let module A = (val a : Domains.Stateful.DOMAIN) in
  let f = List.assoc "fun" assoc |> to_string in
  try
    let f = Domains.Stateful.find_functor f in
    let module F = (val f) in
    let module D = F(A) in
    (module D : Domains.Stateful.DOMAIN)
  with Not_found ->
    Debug.fail "Functor %s not found" f


let parse (file: string) : (module Domains.Stateful.DOMAIN) =
  let json = Yojson.Basic.from_file file in
  build_domain json
