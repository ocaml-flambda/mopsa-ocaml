(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Configuration parser. *)

open Yojson.Basic
open Yojson.Basic.Util


(** Path to the current configuration *)
let opt_config = ref ""


(** {2 Configuration file} *)
(** ********************** *)

(** Return the path of the configuration file *)
let resolve_config_file config =
  if Sys.file_exists config && not (Sys.is_directory config) then config
  else
    let file = Filename.concat (Setup.get_configs_dir ()) config in
    if Sys.file_exists file && not (Sys.is_directory file) then file
    else Exceptions.panic "unable to find configuration file %s" config


(** {2 Domain builders} *)
(** ******************* *)

let rec build_domain = function
  | `String(name) -> build_leaf name
  | `Assoc(obj) when List.mem_assoc "iter" obj -> build_iter @@ List.assoc "iter" obj
  | `Assoc(obj) when List.mem_assoc "product" obj -> build_product obj
  | `Assoc(obj) when List.mem_assoc "functor" obj -> build_functor obj
  | `Assoc(obj) when List.mem_assoc "stack" obj -> build_stack obj
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
    Exceptions.panic "Domain %s not found" name


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
  let sub =
      try List.assoc "over" assoc |> build_domain
      with Not_found -> (module Domains.Empty : Domain.DOMAIN)
  in
  let module D = (val Domains.Reduced_product.Factory.make pool rules sub) in
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
    Exceptions.panic "Functor %s not found" f

and build_stack assoc =
  let d1 = List.assoc "stack" assoc in

  let d1 =
    try Domains.Stacked.find_domain (to_string d1)
    with Not_found ->
      let d1 = build_domain d1 in
      let module D1 = (val d1) in
      let module D1 = Domains.Stacked.MakeStacked(D1) in
      (module D1)
  in
  
  let module D1 = (val d1 : Domains.Stacked.S) in

  let d2 = List.assoc "over" assoc in
  let d2 = build_domain d2 in
  let module D2 = (val d2 : Domain.DOMAIN) in

  let module D = Domains.Stacked.Make(D1)(D2) in
  (module D : Domain.DOMAIN)

(** {2 Toplevel attributes} *)
(** *********************** *)

let get_language json =
  match json with
  | `Assoc(obj) when List.mem_assoc "language" obj ->
    List.assoc "language" obj |> to_string
  | _ -> Exceptions.panic "language declaration not found in configuration file"

let get_domain json =
  match json with
  | `Assoc(obj) when List.mem_assoc "domain" obj ->
    List.assoc "domain" obj
  | _ -> Exceptions.panic "domain declaration not found in configuration file"


(** {2 Entry points} *)
(** **************** *)

let parse () : string * (module Domain.DOMAIN) =
  let file = resolve_config_file !opt_config in
  let json = Yojson.Basic.from_file file in
  let language = get_language json in
  let domain = get_domain json in
  language, build_domain domain

let language () : string =
  let file = resolve_config_file !opt_config in
  let json = Yojson.Basic.from_file file in
  get_language json

let domains () : string list =
  if !opt_config = "" then Domain.names ()
  else
    let file = resolve_config_file !opt_config in
    let json = Yojson.Basic.from_file file in
    let rec iter = function
      | `String(name) -> [name]

      | `Assoc(obj) when List.mem_assoc "iter" obj ->
        List.assoc "iter" obj |>
        to_list |>
        List.fold_left (fun acc obj ->
            iter obj @ acc
          ) []

      | `Assoc(obj) when List.mem_assoc "product" obj ->
        List.assoc "product" obj |>
        to_list |>
        List.fold_left (fun acc obj ->
            iter obj @ acc
          ) []

      | `Assoc(obj) when List.mem_assoc "functor" obj ->
        iter (List.assoc "functor" obj) @
        iter (List.assoc "arg" obj)

      | `Assoc(obj) when List.mem_assoc "stack" obj ->
        iter (List.assoc "stack" obj) @
        iter (List.assoc "over" obj)

      | _ -> assert false
    in
    iter (get_domain json)
