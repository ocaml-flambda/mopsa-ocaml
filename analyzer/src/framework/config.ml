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

   - an object [\{"fold": \[domain list\]\}] propagates the result of a domain
   to the next one.

   - an object [\{"iter": \[domain list\]\}] iterates over a list of domains and
returns the first non-empty result.

   - an object [\{"stack": \[domain list\]\}] combines a list of domains
   hierarchically in a stack.

*)

open Yojson.Basic
open Yojson.Basic.Util

let debug fmt = Debug.debug ~channel:"config" fmt

let config_file = ref ""

(** {2 Flat domains} *)

let rec build_domain = function
  | `String(name) ->
    build_leaf name
  | `Assoc(obj) when List.mem_assoc "fun" obj ->
    build_functor obj
  | `Assoc(obj) when List.mem_assoc "fold" obj ->
    build_fold @@ List.assoc "fold" obj
  | `Assoc(obj) when List.mem_assoc "iter" obj ->
    build_iter @@ List.assoc "iter" obj
  | `Assoc(obj) when List.mem_assoc "stack" obj ->
    build_stack @@ List.assoc "stack" obj
  | _ -> assert false

and build_leaf name =
  try Domains.Global.find_domain name
  with Not_found ->
    Debug.fail "Domain %s not found" name

and build_fold json =
  let domains = json |> to_list |> List.map build_domain in
  let rec aux :
    (module Domains.Global.DOMAIN) list ->
    (module Domains.Global.DOMAIN)
    = function
      | [] -> assert false
      | [d] -> d
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : Domains.Global.DOMAIN) in
        let module Tail = (val tl : Domains.Global.DOMAIN) in
        let module Dom = Domains.Composers.Fold.Make(Head)(Tail) in
        (module Dom : Domains.Global.DOMAIN)
  in
  aux domains


and build_iter json =
  let domains = json |> to_list |> List.map build_domain in
  let rec aux :
    (module Domains.Global.DOMAIN) list ->
    (module Domains.Global.DOMAIN)
    = function
      | [] -> assert false
      | [d] -> d
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : Domains.Global.DOMAIN) in
        let module Tail = (val tl : Domains.Global.DOMAIN) in
        let module Dom = Domains.Composers.Iter.Make(Head)(Tail) in
        (module Dom : Domains.Global.DOMAIN)
  in
  aux domains

and build_functor assoc =
  let arg = List.assoc "arg" assoc in
  let a = build_domain arg in
  let module A = (val a : Domains.Global.DOMAIN) in
  let f = List.assoc "fun" assoc |> to_string in
  try
    let f = Domains.Global.find_functor f in
    let module F = (val f) in
    let module D = F(A) in
    (module D : Domains.Global.DOMAIN)
  with Not_found ->
    Debug.fail "Functor %s not found" f


and build_stack json =
  let domains_json = json |> to_list in
  let d = List.fold_left (fun sub json ->
      let s = build_stack_domain json in
      let module S = (val s: Domains.Global.STACK_DOMAIN) in
      let module Sub = (val sub : Domains.Global.DOMAIN) in
      let module D = Domains.Composers.Stacker.Make(S)(Sub) in
      (module D : Domains.Global.DOMAIN)
    ) (module Domains.Global.EmptyDomain) domains_json in
  let module D = (val d : Domains.Global.DOMAIN) in
  (module D : Domains.Global.DOMAIN)


(** {2 Stack domains} *)

and build_stack_domain : json -> (module Domains.Global.STACK_DOMAIN) =
  function
  | `String(name) -> build_stack_leaf name
  | `Assoc(obj) when List.mem_assoc "fold" obj ->
    build_stack_fold @@ List.assoc "fold" obj
  | `Assoc(obj) when List.mem_assoc "iter" obj ->
    build_stack_iter @@ List.assoc "iter" obj
  | _ -> assert false

and build_stack_leaf (name: string) : (module Domains.Global.STACK_DOMAIN) =
  try
    let d = Domains.Global.find_domain name in
    let module D = (val d) in
    let module S = Domains.Global.MakeStack(D) in
    (module S : Domains.Global.STACK_DOMAIN)
  with Not_found ->
    try Domains.Global.find_stack_domain name
    with Not_found ->
      Debug.fail "Stack domain %s not found" name

and build_stack_fold json : (module Domains.Global.STACK_DOMAIN) =

  let domains = json |> to_list |> List.map build_stack_domain in
  let rec aux :
    (module Domains.Global.STACK_DOMAIN) list ->
    (module Domains.Global.STACK_DOMAIN)
    = function
      | [] -> assert false
      | [d] -> d
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : Domains.Global.STACK_DOMAIN) in
        let module Tail = (val tl : Domains.Global.STACK_DOMAIN) in
      let module Dom = Domains.Composers.Fold.MakeStack(Head)(Tail) in
        (module Dom : Domains.Global.STACK_DOMAIN)
  in
  aux domains


and build_stack_iter json : (module Domains.Global.STACK_DOMAIN) =
  let domains = json |> to_list |> List.map build_stack_domain in
  let rec aux :
    (module Domains.Global.STACK_DOMAIN) list ->
    (module Domains.Global.STACK_DOMAIN)
    = function
      | [] -> assert false
      | [d] -> d
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : Domains.Global.STACK_DOMAIN) in
        let module Tail = (val tl : Domains.Global.STACK_DOMAIN) in
        let module Dom = Domains.Composers.Iter.MakeStack(Head)(Tail) in
        (module Dom : Domains.Global.STACK_DOMAIN)
  in
  aux domains


let parse (file: string) : (module Domains.Global.DOMAIN) =
  let json = Yojson.Basic.from_file file in
  build_domain json
