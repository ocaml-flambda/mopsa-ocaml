(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Build the global abstract domain from a json configuration file.

   The supported syntax for a [domain] is as follows: - a string
   ["name"] denotes a leaf domain (stateful of stateless).

   - an object [\{"iter": \[domain list\]\}] uses the Iter combiner to
   build a product of a list of domains that are used in sequence.

   - an object [\{"stack": domain, "over": domain\}] creates a stacked
   domain over a given sub-domain.

   - an object [\{"non-rel": value\}] refers to a non-relational
   abstraction domain over an argument value abstraction.

*)

open Yojson.Basic
open Yojson.Basic.Util

let debug fmt = Debug.debug ~channel:"framework.config" fmt

(** {2 Flat domains} *)

let rec build_domain = function
  | `String(name) -> build_leaf name
  | `Assoc(obj) when List.mem_assoc "iter" obj -> build_iter @@ List.assoc "iter" obj
  | `Assoc(obj) when List.mem_assoc "stack" obj -> build_stack obj
  | `Assoc(obj) when List.mem_assoc "non-rel" obj -> build_non_rel @@ List.assoc "non-rel" obj
  | _ -> assert false

and build_leaf name =
  try Domain.find_domain name
  with Not_found -> Debug.fail "Domain %s not found" name

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

and build_non_rel json =
  let value = json |> to_string |> (fun name ->
      try Value.find_value name
      with Not_found -> Debug.fail "Value %s not found" name
    )
  in
  let module V = (val value : Value.VALUE) in
  let module D = Domains.Nonrel.Make(V) in
  (module D : Domain.DOMAIN)


and build_stack assoc =
  let sub = List.assoc "over" assoc in
  let a = build_domain sub in
  let module S = (val a : Domain.DOMAIN) in
  let u = List.assoc "stack" assoc |> to_string in
  try
    let d = Domains.Stacked.find_domain u in
    let module D = (val d) in
    let module R = Domains.Stacked.Make(D)(S) in
    (module R : Domain.DOMAIN)
  with Not_found ->
    Debug.fail "Stack domain  %s not found" u




let parse (file: string) : (module Domain.DOMAIN) =
  let json = Yojson.Basic.from_file file in
  build_domain json
