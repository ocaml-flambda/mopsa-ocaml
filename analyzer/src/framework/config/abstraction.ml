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
open Domain.Sig
open Domain.Value
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
  | `Assoc(obj) when List.mem_assoc "seq" obj -> seq obj
  | `Assoc(obj) when List.mem_assoc "apply" obj -> apply obj
  | `Assoc(obj) when List.mem_assoc "nonrel" obj -> nonrel obj
  | _ -> assert false

and leaf_domain name =
  try Domain.Sig.find_domain name
  with Not_found -> Exceptions.panic "Domain %s not found" name


and seq assoc =
  let domains = List.assoc "seq" assoc |>
                to_list |>
                List.map domain
  in
  let rec aux :
    (module Domain.Sig.DOMAIN) list ->
    (module Domain.Sig.DOMAIN)
    = function
      | [] -> assert false
      | [d] -> d
      | hd :: tl ->
        let tl = aux tl in
        let module Head = (val hd : Domain.Sig.DOMAIN) in
        let module Tail = (val tl : Domain.Sig.DOMAIN) in
        let module Dom = Combiners.Sequence.Make(Head)(Tail) in
        (module Dom : Domain.Sig.DOMAIN)
  in
  aux domains

and apply assoc =
  let s = List.assoc "apply" assoc |> stack in
  let d = List.assoc "on" assoc |> domain in
  let module S = (val s : STACK) in
  let module D = (val d : DOMAIN) in
  let module R = Combiners.Apply.Make(S)(D) in
  (module R : DOMAIN)

and nonrel assoc =
  let v = List.assoc "nonrel" assoc |> value in
  let module V = (val v : VALUE) in
  let module D = Domains.Leaf.Make(Domains.Nonrel.Make(V)) in
  (module D : DOMAIN)

and value = function
  | `String name -> value_leaf name
  | _ -> assert false

and value_leaf name =
  try Domain.Value.find_value name
  with Not_found -> Exceptions.panic "Value %s not found" name

and stack = function
  | `String(name) -> leaf_stack name
  | `Assoc(obj) when List.mem_assoc "compose" obj -> compose obj
  | _ -> assert false

and leaf_stack name =
  try Domain.Sig.find_stack name
  with Not_found -> Exceptions.panic "Stack %s not found" name

and compose assoc =
  let s1 = List.assoc "compose" assoc |> stack in
  let s2 = List.assoc "with" assoc |> stack in
  let module S1 = (val s1 : STACK) in
  let module S2 = (val s2 : STACK) in
  let module S = Combiners.Compose.Make(S1)(S2) in
  (module S)


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

let parse () : string * (module Domain.Sig.DOMAIN) =
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
  if !opt_config = "" then Domain.Sig.names ()
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
        List.assoc "nonrel" obj |>
        to_list |>
        List.fold_left (fun acc obj ->
            iter obj @ acc
          ) []

      | `Assoc(obj) when List.mem_assoc "apply" obj ->
        iter (List.assoc "apply" obj) @
        iter (List.assoc "on" obj)

      | `Assoc(obj) when List.mem_assoc "compose" obj ->
        iter (List.assoc "compose" obj) @
        iter (List.assoc "with" obj)

      | _ -> assert false
    in
    iter (get_domain json)
