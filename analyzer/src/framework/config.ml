(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Configuration parser *)

open Yojson.Basic
open Yojson.Basic.Util

let debug fmt = Debug.debug ~channel:"framework.config" fmt

let rec build_domain = function
  | `String(name) -> build_leaf name
  | `Assoc(obj) when List.mem_assoc "iter" obj -> build_iter @@ List.assoc "iter" obj
  | `Assoc(obj) when List.mem_assoc "product" obj -> build_product obj
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


let parse (file: string) : (module Domain.DOMAIN) =
  let json = Yojson.Basic.from_file file in
  build_domain json
