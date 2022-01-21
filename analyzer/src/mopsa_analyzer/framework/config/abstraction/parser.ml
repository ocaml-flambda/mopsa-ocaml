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

(** Parser of configuration files *)


open Mopsa_utils
open Yojson.Basic
open Yojson.Basic.Util
open Syntax
open Visitor
open Sig.Abstraction.Stacked
open Sig.Abstraction.Stacked_functor
open Sig.Abstraction.Domain
open Sig.Abstraction.Functor
open Sig.Abstraction.Simplified
open Sig.Abstraction.Simplified_functor
open Sig.Abstraction.Stateless
open Sig.Abstraction.Value
open Sig.Abstraction.Value_functor
open Sig.Reduction.Exec
open Sig.Reduction.Eval
open Sig.Reduction.Simplified
open Sig.Reduction.Value


(** {2 Configuration file} *)
(** ********************** *)

(** Path to the current configuration *)
let opt_config = ref ""



let all_domains () =
  stacked_domain_names () @
  standard_domain_names () @
  simplified_domain_names () @
  stateless_domain_names () @
  value_abstraction_names () @
  stacked_functor_names () @
  domain_functor_names () @
  simplified_functor_names () @
  value_functor_names ()

let all_reductions () =
  simplified_value_reductions () @
  eval_reductions () @
  exec_reductions () @
  simplified_reductions ()

(** {2 Domain parser} *)
(** ***************** *)

let debug fmt = Debug.debug ~channel:"framework.config.abstraction.parser" fmt

let rec parse_domain json : domain =
  json |> visit {
    leaf = (fun semantic name ->
        let d =
          try D_stacked(find_stacked_domain name)     with Not_found ->
          try D_domain(find_standard_domain name)     with Not_found ->
          try D_stateless(find_stateless_domain name) with Not_found ->
          try D_simplified(find_simplified_domain name)
          with Not_found -> Exceptions.panic "Domain '%s' not found@.Available domains: %a" name (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string) (all_domains ())
        in
        mk_domain d ~semantic
      );
    switch = (fun semantic jsons ->
        mk_domain (D_switch (List.map parse_domain jsons)) ~semantic
      );
    compose = (fun semantic jsons ->
        mk_domain (D_compose (List.map parse_domain jsons)) ~semantic
      );
    product = (fun semantic jsons reductions ->
        mk_domain (D_product (List.map parse_domain jsons, List.map parse_domain_reduction reductions)) ~semantic
      );
    nonrel = (fun semantic json ->
        mk_domain (D_nonrel(parse_value json)) ~semantic
      );
    apply = (fun semantic funct arg ->
        mk_domain (D_functor(parse_domain_functor funct, parse_domain arg)) ~semantic
      );
    union = (fun semantic json -> assert false);
  }

and parse_domain_reduction (name:string) : domain_reduction =
  try DR_eval(find_eval_reduction name) with Not_found ->
  try DR_exec(find_exec_reduction name) with Not_found ->
  try DR_simplified(find_simplified_reduction name)
  with Not_found -> Exceptions.panic "Domain reduction '%s' not found@.Available reductions: %a" name (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string) (all_reductions ())

and parse_domain_functor name : domain_functor =
  try F_stacked(find_stacked_functor name) with Not_found ->
  try F_domain(find_domain_functor name) with Not_found ->
  try F_simplified(find_simplified_functor name)
  with Not_found -> Exceptions.panic "Domain functor '%s' not found" name

and parse_value json : value =
  json |> visit {
    leaf = (fun _ name -> try V_value (find_value_abstraction name) with Not_found -> Exceptions.panic "Value '%s' not found" name);
    union = (fun _ jsons -> V_union (List.map parse_value jsons));
    product = (fun _ jsons reductions -> V_product (List.map parse_value jsons, List.map parse_value_reduction reductions));
    apply = (fun _ funct arg -> V_functor(parse_value_functor funct, parse_value arg));

    switch = (fun _ jsons -> assert false);
    compose = (fun _ jsons -> assert false);
    nonrel = (fun _ json -> assert false);
  }

and parse_value_reduction (name:string) : value_reduction =
  try find_value_reduction name
  with Not_found -> Exceptions.panic "Value reduction '%s' not found" name

and parse_value_functor (name:string) : value_functor =
  try find_value_functor name
  with Not_found -> Exceptions.panic "Value functor '%s' not found" name

(** {2 Toplevel attributes} *)
(** *********************** *)

let get_language json =
  match json with
  | `Assoc(obj) when List.mem_assoc "language" obj ->
    List.assoc "language" obj |> to_string
  | _ -> Exceptions.panic "language declaration not found in configuration file"

let get_domain_json json =
  match json with
  | `Assoc(obj) when List.mem_assoc "domain" obj ->
    List.assoc "domain" obj
  | _ -> Exceptions.panic "domain declaration not found in configuration file"


(** {2 Entry points} *)
(** **************** *)

let parse file : abstraction =
  let json = Yojson.Basic.from_file file in
  let language = get_language json in
  let domain_json = get_domain_json json in
  let domain = parse_domain domain_json in
  debug "abstraction: %a" pp_domain domain;
  { domain; language }

let language file : string =
  let json = Yojson.Basic.from_file file in
  get_language json

let domains file : string list =
  if file = "" then
    all_domains ()
  else
    let json = Yojson.Basic.from_file file in
    let domain = json |> member "domain" in
    let rec name_visitor = Visitor.{
        leaf = (fun _ name -> [name]);
        switch = (fun _ l -> List.map get_names l |> List.flatten);
        nonrel = (fun _ v -> get_names v);
        apply = (fun _ f d -> f :: get_names d);
        compose = (fun _ l -> List.map get_names l |> List.flatten);
        product = (fun _ l r -> List.map get_names l |> List.flatten);
        union = (fun _ l -> List.map get_names l |> List.flatten);
    }
    and get_names json = Visitor.visit name_visitor json in
    get_names domain
