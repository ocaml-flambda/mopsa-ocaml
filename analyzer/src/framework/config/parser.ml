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
open Abstraction
open Yojson.Basic
open Yojson.Basic.Util
open Typer


let debug fmt = Debug.debug ~channel:"framework.config.parser" fmt


(** Path to the current configuration *)
let opt_config = ref ""


(** {2 Configuration file} *)
(** ********************** *)

(** Return the path of the configuration file *)
let resolve_config_file config =
  let config =
    try Sys.getenv "MOPSACONFIG"
    with Not_found -> config
  in
  if Sys.file_exists config && not (Sys.is_directory config) then config
  else
    let file = Filename.concat (Paths.get_configs_dir ()) config in
    if Sys.file_exists file && not (Sys.is_directory file) then file
    else Exceptions.panic "unable to find configuration file %s" config


(** {2 Specification of transformers} *)
(** ********************************* *)

let spec = {
  chain = (fun abstraction operator signature ->
      match abstraction, operator,signature with
      | _, _, S_lowlevel -> true

      | A_domain, O_seq, S_intermediate -> true
      | A_domain, O_seq, S_stateless -> true

      | A_stack, O_seq, S_intermediate -> true

      | _, _, _ -> false
    );

  apply = (fun signature ->
      match signature with
      | S_lowlevel -> true
      | _ -> false
    );

  product = (fun abstraction signature ->
      match abstraction, signature with
      | _, S_lowlevel -> true

      | A_domain, S_simplified -> true

      | _ -> false
    );

}



(** {2 Domain builders} *)
(** ******************* *)


let rec domain_lowlevel config : (module Sig.Domain.Lowlevel.DOMAIN) =
  match config.structure with
  | S_leaf name -> Sig.Domain.Lowlevel.find_domain name
  | S_chain(op, l) -> domain_chain_lowlevel op l
  | S_apply(s,d) -> domain_apply_lowlevel s d
  | S_cast c -> domain_cast_lowlevel c
  | _ -> assert false

and domain_chain_lowlevel (op:operator) (l:config list) : (module Sig.Domain.Lowlevel.DOMAIN) =
  match l with
  | [] -> assert false
  | [d] -> domain_lowlevel d
  | r ->
    let a,b = ListExt.split r in
    let aa, bb = domain_chain_lowlevel op a, domain_chain_lowlevel op b in
    let module A = (val aa : Sig.Domain.Lowlevel.DOMAIN) in
    let module B = (val bb : Sig.Domain.Lowlevel.DOMAIN) in
    match op with
    | O_seq ->
      let module C = Transformers.Domain.Lowlevel.Sequence.Make(A)(B) in
      (module C)
    | _ -> assert false

and domain_apply_lowlevel (stack:config) (domain:config) : (module Sig.Domain.Lowlevel.DOMAIN) =
  let s = stack_lowlevel stack in
  let d = domain_lowlevel domain in
  let module S = (val s : Sig.Stacked.Lowlevel.STACK) in
  let module D = (val d : Sig.Domain.Lowlevel.DOMAIN) in
  let module DD = Transformers.Domain.Lowlevel.Apply.Make(S)(D) in
  (module DD)

and domain_cast_lowlevel (config:config) : (module Sig.Domain.Lowlevel.DOMAIN) =
  match config.signature with
  | S_intermediate ->
    let s = domain_intermediate config in
    let module S = (val s : Sig.Domain.Intermediate.DOMAIN) in
    let module SS = Sig.Domain.Intermediate.MakeLowlevelDomain(S) in
    (module SS)

  | S_simplified ->
    let s = domain_simplified config in
    let module S = (val s : Sig.Domain.Simplified.DOMAIN) in
    let module SS = Sig.Domain.Intermediate.MakeLowlevelDomain(
        Sig.Domain.Simplified.MakeIntermediate(S)
      )
    in
    (module SS)

  | S_stateless ->
    let s = domain_stateless config in
    let module S = (val s : Sig.Domain.Stateless.DOMAIN) in
    let module SS = Sig.Domain.Intermediate.MakeLowlevelDomain(
        Sig.Domain.Stateless.MakeIntermediate(S)
      )
    in
    (module SS)

  | _ -> assert false


and domain_intermediate (config:config) : (module Sig.Domain.Intermediate.DOMAIN) =
  match config.structure with
  | S_leaf name -> Sig.Domain.Intermediate.find_domain name
  | S_chain (op,l) -> domain_chain_intermediate op l
  | S_cast d -> domain_cast_intermediate d
  | _ -> assert false

and domain_chain_intermediate (op:operator) (l:config list) : (module Sig.Domain.Intermediate.DOMAIN) =
  match l with
  | [] -> assert false
  | [d] -> domain_intermediate d
  | r ->
    let a,b = ListExt.split r in
    let aa, bb = domain_chain_intermediate op a, domain_chain_intermediate op b in
    let module A = (val aa : Sig.Domain.Intermediate.DOMAIN) in
    let module B = (val bb : Sig.Domain.Intermediate.DOMAIN) in
    match op with
    | O_seq ->
      let module C = Transformers.Domain.Intermediate.Sequence.Make(A)(B) in
      (module C)
    | _ -> assert false

and domain_cast_intermediate (config:config) : (module Sig.Domain.Intermediate.DOMAIN) =
  match config.signature with
  | S_simplified ->
    let s = domain_simplified config in
    let module S = (val s : Sig.Domain.Simplified.DOMAIN) in
    let module SS = Sig.Domain.Simplified.MakeIntermediate(S) in
    (module SS)

  | S_stateless ->
    let s = domain_stateless config in
    let module S = (val s : Sig.Domain.Stateless.DOMAIN) in
    let module SS = Sig.Domain.Stateless.MakeIntermediate(S) in
    (module SS)

  | _ -> assert false


and domain_simplified (config:config) : (module Sig.Domain.Simplified.DOMAIN) =
  match config.structure with
  | S_leaf name -> Sig.Domain.Simplified.find_domain name
  | S_product(l,r) -> domain_simplified_product l r
  | S_nonrel v -> domain_simplified_nonrel v
  | _ -> assert false

and domain_simplified_product (l:config list) (rules:string list) : (module Sig.Domain.Simplified.DOMAIN) =
  let ll = List.map domain_simplified l in
  let rules = List.map Sig.Domain.Reduction.find_reduction rules in
  Transformers.Domain.Simplified.Product.make ll rules

and domain_simplified_nonrel (v:config) : (module Sig.Domain.Simplified.DOMAIN) =
  let vv = value_lowlevel v in
  let module V = (val vv : Sig.Value.Lowlevel.VALUE) in
  let module VV = Transformers.Value.Nonrel.Make(V) in
  (module VV)

and domain_stateless (config:config) : (module Sig.Domain.Stateless.DOMAIN) =
  match config.structure with
  | S_leaf name -> Sig.Domain.Stateless.find_domain name
  | S_chain(op,l) -> domain_chain_stateless op l
  | _ -> assert false


and domain_chain_stateless (op:operator) (l:config list) : (module Sig.Domain.Stateless.DOMAIN) =
  match l with
  | [] -> assert false
  | [d] -> domain_stateless d
  | r ->
    let a,b = ListExt.split r in
    let aa, bb = domain_chain_stateless op a, domain_chain_stateless op b in
    let module A = (val aa : Sig.Domain.Stateless.DOMAIN) in
    let module B = (val bb : Sig.Domain.Stateless.DOMAIN) in
    match op with
    | O_seq ->
      let module C = Transformers.Domain.Stateless.Sequence.Make(A)(B) in
      (module C)
    | _ -> assert false


(** {2 Stack builders} *)
(** ******************* *)

and stack_lowlevel config : (module Sig.Stacked.Lowlevel.STACK) =
  match config.structure with
  | S_leaf name -> Sig.Stacked.Lowlevel.find_stack name
  | S_chain(op, l) -> stack_chain_lowlevel op l
  | S_cast c -> stack_cast_lowlevel c
  | _ -> assert false


and stack_chain_lowlevel (op:operator) (l:config list) : (module Sig.Stacked.Lowlevel.STACK) =
  match l with
  | [] -> assert false
  | [s] -> stack_lowlevel s
  | r ->
    let a,b = ListExt.split r in
    let aa, bb = stack_chain_lowlevel op a, stack_chain_lowlevel op b in
    let module A = (val aa : Sig.Stacked.Lowlevel.STACK) in
    let module B = (val bb : Sig.Stacked.Lowlevel.STACK) in
    match op with
    | O_seq ->
      let module C = Transformers.Stacked.Lowlevel.Sequence.Make(A)(B) in
      (module C)

    | O_compose ->
      let module C = Transformers.Stacked.Lowlevel.Compose.Make(A)(B) in
      (module C)

    | _ -> assert false


and stack_cast_lowlevel (config:config) : (module Sig.Stacked.Lowlevel.STACK) =
  match config.signature with
  | S_intermediate ->
    let s = stack_intermediate config in
    let module S = (val s : Sig.Stacked.Intermediate.STACK) in
    let module SS = Sig.Stacked.Intermediate.MakeLowlevelStack(S) in
    (module SS)

  | S_stateless ->
    let s = stack_stateless config in
    let module S = (val s : Sig.Stacked.Stateless.STACK) in
    let module SS = Sig.Stacked.Intermediate.MakeLowlevelStack(
        Sig.Stacked.Stateless.MakeIntermediate(S)
      )
    in
    (module SS)

  | _ -> assert false

and stack_intermediate (config:config) : (module Sig.Stacked.Intermediate.STACK) =
  match config.structure with
  | S_leaf name -> Sig.Stacked.Intermediate.find_stack name
  | S_chain (op,l) -> stack_chain_intermediate op l
  | S_cast c -> stack_cast_intermediate c
  | _ -> assert false


and stack_chain_intermediate (op:operator) (l:config list) : (module Sig.Stacked.Intermediate.STACK) =
  match l with
  | [] -> assert false
  | [s] -> stack_intermediate s
  | r ->
    let a,b = ListExt.split r in
    let aa, bb = stack_chain_intermediate op a, stack_chain_intermediate op b in
    let module A = (val aa : Sig.Stacked.Intermediate.STACK) in
    let module B = (val bb : Sig.Stacked.Intermediate.STACK) in
    match op with
    | O_seq ->
      let module C = Transformers.Stacked.Intermediate.Sequence.Make(A)(B) in
      (module C)

    | _ -> assert false

and stack_cast_intermediate (config:config) : (module Sig.Stacked.Intermediate.STACK) =
  match config.signature with
  | S_stateless ->
    let s = stack_stateless config in
    let module S = (val s : Sig.Stacked.Stateless.STACK) in
    let module SS = Sig.Stacked.Stateless.MakeIntermediate(S) in
    (module SS)

  | _ -> assert false


and stack_stateless (config:config) : (module Sig.Stacked.Stateless.STACK) =
  match config.structure with
  | S_leaf name -> Sig.Stacked.Stateless.find_stack name
  | _ -> assert false


(** {2 Value builders} *)
(** ******************* *)

and value_lowlevel config : (module Sig.Value.Lowlevel.VALUE) =
  match config.structure with
  | S_leaf name -> Sig.Value.Lowlevel.find_value name
  | S_chain(op, l) -> value_chain_lowlevel op l
  | S_cast c -> value_cast_lowlevel c
  | S_product(l,r) -> value_product_lowlevel l r
  | _ -> assert false


and value_chain_lowlevel (op:operator) (l:config list) : (module Sig.Value.Lowlevel.VALUE) =
  match l with
  | [] -> assert false
  | [v] -> value_lowlevel v
  | r ->
    let a,b = ListExt.split r in
    let aa, bb = value_chain_lowlevel op a, value_chain_lowlevel op b in
    let module A = (val aa : Sig.Value.Lowlevel.VALUE) in
    let module B = (val bb : Sig.Value.Lowlevel.VALUE) in
    match op with
    | O_disjoint ->
      let module C = Transformers.Value.Lowlevel.Disjoint.Make(A)(B) in
      (module C)

    | _ -> assert false


and value_product_lowlevel (l:config list) (rules:string list) : (module Sig.Value.Lowlevel.VALUE) =
  let ll = List.map value_lowlevel l in
  let rules = List.map Sig.Value.Reduction.find_reduction rules in
  Transformers.Value.Lowlevel.Product.make ll rules


and value_cast_lowlevel (config:config) : (module Sig.Value.Lowlevel.VALUE) =
  match config.signature with
  | S_intermediate ->
    let v = value_intermediate config in
    let module V = (val v : Sig.Value.Intermediate.VALUE) in
    let module VV = Sig.Value.Intermediate.MakeLowlevel(V) in
    (module VV)

  | S_simplified ->
    let v = value_simplified config in
    let module V = (val v : Sig.Value.Simplified.VALUE) in
    let module VV = Sig.Value.Intermediate.MakeLowlevel(Sig.Value.Simplified.MakeIntermediate(V)) in
    (module VV)

  | _ -> assert false

and value_intermediate (config:config) : (module Sig.Value.Intermediate.VALUE) =
  match config.structure with
  | S_leaf name -> Sig.Value.Intermediate.find_value name
  | _ -> assert false

and value_simplified (config:config) : (module Sig.Value.Simplified.VALUE) =
  match config.structure with
  | S_leaf name -> Sig.Value.Simplified.find_value name
  | _ -> assert false


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

let parse file : string * (module Sig.Domain.Lowlevel.DOMAIN) =
  let file = resolve_config_file file in

  let json = Yojson.Basic.from_file file in
  let language = get_language json in
  let json = get_domain json in

  let config = Typer.domain spec json in
  let config'= Typer.cast S_lowlevel config in
  debug "analysis configuration:@, %a" Typer.pp_config config';

  language, domain_lowlevel config'

let language file : string =
  let file = resolve_config_file file in
  let json = Yojson.Basic.from_file file in
  get_language json

let domains file : string list =
  if file = "" then
    Sig.Domain.Lowlevel.names () @
    Sig.Domain.Intermediate.names () @
    Sig.Domain.Simplified.names () @
    Sig.Domain.Stateless.names () @

    Sig.Stacked.Lowlevel.names () @
    Sig.Stacked.Intermediate.names () @
    Sig.Stacked.Stateless.names () @

    Sig.Value.Lowlevel.names () @
    Sig.Value.Intermediate.names () @
    Sig.Value.Simplified.names ()

  else
    let file = resolve_config_file file in
    let json = Yojson.Basic.from_file file in
    let domain = json |> member "domain" in
    let rec name_visitor = Visitor.{
        leaf = (fun name -> [name]);
        seq = (fun l -> List.map get_names l |> List.flatten);
        nonrel = (fun v -> get_names v);
        apply = (fun s d -> get_names s @ get_names d);
        compose = (fun l -> List.map get_names l |> List.flatten);
        product = (fun l r -> List.map get_names l |> List.flatten);
        disjoint = (fun l -> List.map get_names l |> List.flatten);
    }
    and get_names json = Visitor.visit name_visitor json in
    get_names domain
