(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Configuration parser.

    Build the global abstract domain from a json configuration file.

    The supported syntax for a [domain] is as follows:

    - a string denotes a leaf domain or a non-relational value abstraction.

    - [\{"functor": domain, "arg": domain\}] creates a functor domain 
    parameterized by an argument domain.

    - [\{"iter": \[domain list\]\}] uses the iterator composer to
    combine a list of domains in sequence.
    
    - [\{"product": \[string list\], "reductions": \[string list\]\}] 
    constructs a reduced product of a set of abstract domains and 
    non-relational value abstractions with some reduction rules 
*)

open Yojson.Basic
open Yojson.Basic.Util

let debug fmt = Debug.debug ~channel:"framework.config" fmt

let rec build_domain = function
  | `String(name) -> build_leaf name
  | `Assoc(obj) when List.mem_assoc "iter" obj -> build_iter @@ List.assoc "iter" obj
  | `Assoc(obj) when List.mem_assoc "product" obj -> build_product obj
  | `Assoc(obj) when List.mem_assoc "functor" obj -> build_functor obj
  | _ -> assert false

and build_leaf name =
  try Domain.find_domain name
  with Not_found ->
  try
    let v = Value.find_value name in
    let module V = (val v) in
    let module D = Domains.Nonrel.Make(V) in
    (module D)
  with Not_found ->
    Debug.fail "Domain %s not found" name
      

and build_iter json =
  let domains = json |> to_list |> List.map build_domain in
  let rec aux :
    (module Domain.DOMAIN) list ->
    (module Domain.DOMAIN)
    = function
      | [] -> assert false
      | [d] -> d
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : Domain.DOMAIN) in
        let module Tail = (val tl : Domain.DOMAIN) in
        let module Dom = Domains.Iter.Make(Head)(Tail) in
        (module Dom : Domain.DOMAIN)
  in
  aux domains

and build_product assoc =
  let pool = List.assoc "product" assoc |> to_list |> List.map to_string in
  let rules = List.assoc "reductions" assoc |> to_list |> List.map to_string in
  let module D = (val Domains.Reduced_product.Factory.make pool rules) in
  (module D)

and build_functor assoc =
  let arg = List.assoc "arg" assoc in
  let a = build_domain arg in
  let module A = (val a : Domain.DOMAIN) in
  let f = List.assoc "functor" assoc |> to_string in
  try
    let f = Domains.Functor.find_domain f in
    let module F = (val f) in
    let module D = F.Make(A) in
    (module D : Domain.DOMAIN)
  with Not_found ->
    Debug.fail "Functor %s not found" f


let parse (file: string) : (module Domain.DOMAIN) =
  let json = Yojson.Basic.from_file file in
  build_domain json
