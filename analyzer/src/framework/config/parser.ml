(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Configuration parser. *)

open Core
open Sig.Lowlevel.Domain
open Sig.Lowlevel.Stacked
open Sig.Lowlevel.Value
open Abstraction
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
    let file = Filename.concat (Paths.get_configs_dir ()) config in
    if Sys.file_exists file && not (Sys.is_directory file) then file
    else Exceptions.panic "unable to find configuration file %s" config


(** {2 Domain builders} *)
(** ******************* *)

let rec domain = function
  | `String(name) -> leaf_domain name
  | `Assoc(obj) when List.mem_assoc "seq" obj -> domain_seq obj
  | `Assoc(obj) when List.mem_assoc "apply" obj -> apply obj
  | `Assoc(obj) when List.mem_assoc "nonrel" obj -> nonrel obj
  | _ -> assert false

and leaf_domain name : (module DOMAIN) =
  try find_domain name
  with Not_found -> Exceptions.panic "domain %s not found" name

and domain_seq assoc : (module DOMAIN) =
  let domains = List.assoc "seq" assoc |>
                to_list |>
                List.map domain
  in
  let rec aux :
    (module DOMAIN) list ->
    (module DOMAIN)
    = function
      | [] -> assert false
      | [d] -> d
      | r ->
        let a,b = ListExt.split r in
        let aa, bb = aux a, aux b in
        let module A = (val aa : DOMAIN) in
        let module B = (val bb : DOMAIN) in
        let module Dom = Combiners.Domain.Sequence.Make(A)(B) in
        (module Dom : DOMAIN)
  in
  aux domains

and apply assoc : (module DOMAIN) =
  let s = List.assoc "apply" assoc |> stack in
  let d = List.assoc "on" assoc |> domain in
  let module S = (val s : STACK) in
  let module D = (val d : DOMAIN) in
  let module R = Combiners.Apply.Make(S)(D) in
  (module R : DOMAIN)

and nonrel assoc : (module DOMAIN) =
  let v = List.assoc "nonrel" assoc |> value in
  let module V = (val v : VALUE) in
  let module D =
    Sig.Intermediate.Domain.MakeLowlevelDomain(
      Sig.Simplified.Domain.MakeIntermediate(
        Combiners.Value.Nonrel.Make(V)
      )
    )
  in
  (module D : DOMAIN)

and value = function
  | `String name -> value_leaf name
  | `Assoc obj when List.mem_assoc "disjoint" obj -> value_disjoint obj
  | _ -> assert false

and value_leaf name =
  try find_value name
  with Not_found -> Exceptions.panic "value %s not found" name

and value_disjoint assoc : (module VALUE) =
  let values = List.assoc "disjoint" assoc |>
                to_list |>
                List.map value
  in
  let rec aux :
    (module VALUE) list ->
    (module VALUE)
    = function
      | [] -> assert false
      | [d] -> d
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : VALUE) in
        let module Tail = (val tl : VALUE) in
        let module Dom = Combiners.Value.Disjoint.Make(Head)(Tail) in
        (module Dom : VALUE)
  in
  aux values

and stack = function
  | `String(name) -> leaf_stack name
  | `Assoc(obj) when List.mem_assoc "seq" obj -> stack_seq obj
  | `Assoc(obj) when List.mem_assoc "compose" obj -> compose obj
  | x -> Exceptions.panic "parsing error: unsupported stack declaration:@ %a"
           (pretty_print ~std:true) x

and leaf_stack name : (module STACK) =
  try find_stack name
  with Not_found -> Exceptions.panic "stack %s not found" name


and stack_seq assoc : (module STACK) =
  let stacks = List.assoc "seq" assoc |>
                to_list |>
                List.map stack
  in
  let rec aux :
    (module STACK) list ->
    (module STACK)
    = function
      | [] -> assert false
      | [d] -> d
      | r ->
        let a,b = ListExt.split r in
        let aa, bb = aux a, aux b in
        let module A = (val aa : STACK) in
        let module B = (val bb : STACK) in
        let module Dom = Combiners.Stacked.Sequence.Make(A)(B) in
        (module Dom : STACK)
  in
  aux stacks

and compose assoc : (module STACK) =
  let stacks = List.assoc "compose" assoc |>
                to_list |>
                List.map stack
  in
  let rec aux :
    (module STACK) list ->
    (module STACK)
    = function
      | [] -> assert false
      | [s] -> s
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : STACK) in
        let module Tail = (val tl : STACK) in
        let module Dom = Combiners.Stacked.Compose.Make(Head)(Tail) in
        (module Dom : STACK)
  in
  aux stacks


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

let parse () : string * (module DOMAIN) =
  let file = resolve_config_file !opt_config in
  let json = Yojson.Basic.from_file file in
  let language = get_language json in
  let json = get_domain json in
  language, domain json

let language () : string =
  let file = resolve_config_file !opt_config in
  let json = Yojson.Basic.from_file file in
  get_language json

let domains () : string list =
  if !opt_config = ""
  then Sig.Lowlevel.Domain.names () @
       Sig.Lowlevel.Stacked.names ()
  else
    let file = resolve_config_file !opt_config in
    let json = Yojson.Basic.from_file file in
    let rec iter = function
      | `String(name) -> [name]

      | `Assoc(obj) when List.mem_assoc "seq" obj ->
        List.assoc "seq" obj |>
        to_list |>
        List.fold_left (fun acc obj ->
            iter obj @ acc
          ) []

      | `Assoc(obj) when List.mem_assoc "nonrel" obj ->
        iter (List.assoc "nonrel" obj)

      | `Assoc(obj) when List.mem_assoc "apply" obj ->
        iter (List.assoc "apply" obj) @
        iter (List.assoc "on" obj)

      | `Assoc(obj) when List.mem_assoc "compose" obj ->
        List.assoc "compose" obj |>
        to_list |>
        List.fold_left (fun acc obj ->
            iter obj @ acc
          ) []

      | _ -> assert false
    in
    iter (get_domain json)
