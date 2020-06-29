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

(** Builder - create a domain from an abstraction tree *)

open Syntax
open Core
open Sig.Abstraction
open Stacked
open Value


(** {2 Values} *)
(** ********** *)

let rec make_value (value:value) : (module VALUE) =
  match value with
  | V_value v                     -> v
  | V_union values                -> Combiners.Value.Union.make (List.map make_value values)
  | V_product (values,reductions) -> Combiners.Value.Product.make (List.map make_value values) reductions
  | V_functor(f, vv) ->
    let module F = (val f) in
    (module F.Functor(val (make_value vv)))



(** {2 Simplified domains} *)
(** ********************** *)

let rec is_simplified_domain = function
  | D_simplified _               -> true
  | D_nonrel _                   -> true
  | D_product (dl,_)             -> List.for_all is_simplified_domain dl
  | D_functor(F_simplified _, d) -> is_simplified_domain d
  |  _                           -> false

let rec make_simplified_domain (domain:domain) : (module Simplified.SIMPLIFIED) =
  match domain with
  | D_simplified d                -> d
  | D_nonrel value                -> make_nonrel_domain value
  | D_product(domains,reductions) -> make_simplified_product domains reductions
  | D_functor(F_simplified f, d) ->
    let module F = (val f) in
    (module F.Functor(val (make_simplified_domain d) : Simplified.SIMPLIFIED))
  | _ -> Exceptions.panic "Invalid configuration of simplified domain: %a" pp_domain domain

and make_nonrel_domain (value:value) : (module Simplified.SIMPLIFIED) =
  let module V = (val (make_value value)) in
  (module Combiners.Value.Nonrel.Make(V) : Simplified.SIMPLIFIED)

and make_simplified_product (domains:domain list) (reductions:domain_reduction list) : (module Simplified.SIMPLIFIED) =
  Combiners.Domain.Simplified_product.make
    (List.map make_simplified_domain domains)
    (List.fold_left
       (fun acc -> function DR_simplified r -> r :: acc | _ -> acc)
       [] reductions)


(** {2 Stateless domains} *)
(** ********************** *)


let rec is_stateless_domain = function
  | D_stateless _   -> true
  | D_sequence (dl) -> List.for_all is_simplified_domain dl
  |  _              -> false

let rec make_stateless_domain (domain:domain) : (module Stateless.STATELESS) =
  match domain with
  | D_stateless d -> d
  | D_sequence dl -> Combiners.Domain.Stateless_sequence.make (List.map make_stateless_domain dl)
  | _ -> Exceptions.panic "Invalid configuration of stateless domain: %a" pp_domain domain


(** {2 Standard domains} *)
(** ******************** *)

let is_standard_domain = function
  | D_domain _ -> true
  | D_functor(F_domain _, _) -> true
  | _ -> false

let rec make_standard_domain (domain:domain) : (module Domain.DOMAIN) =
  if is_simplified_domain domain then
    (module Simplified.MakeDomain(val (make_simplified_domain domain))) else
  if is_stateless_domain domain then
    (module Stateless.MakeDomain(val (make_stateless_domain domain)))
  else
    match domain with
    | D_domain d -> d
    | D_functor(F_domain f, d) ->
      let module F = (val f) in
      (module F.Functor(val (make_standard_domain d) : Domain.DOMAIN))
    | _ -> Exceptions.panic "Invalid configuration of standard domain: %a" pp_domain domain



(** {2 Stacked domains} *)
(** ******************** *)

let rec prepare_stacked_sequence (domains:domain list) : (module STACKED) list =
  match domains with
  | [] -> []
  | D_stateless _ :: _ ->
    let rec extract_stateless_prefix = function
      | [] -> [], []
      | D_stateless _ as hd :: tl ->
        let a,b = extract_stateless_prefix tl in
        hd::a,b
      | l -> [],l
    in
    let l1,l2 = extract_stateless_prefix domains in
    let hd = make_stateless_domain (D_sequence l1) in
    make_stacked_domain (D_stateless hd) :: prepare_stacked_sequence l2
  | hd::tl -> make_stacked_domain hd :: prepare_stacked_sequence tl

and make_stacked_domain (domain:domain) : (module STACKED) =
  match domain with
  | D_stacked d     -> d
  | D_domain d      -> (module Domain.MakeStacked(val d))
  | D_simplified d  -> (module Domain.MakeStacked(Simplified.MakeDomain(val d)))
  | D_stateless d   -> (module Domain.MakeStacked(Stateless.MakeDomain(val d)))
  | D_nonrel(value) -> (module Domain.MakeStacked(Simplified.MakeDomain(val (make_nonrel_domain value))))

  | D_functor(F_stacked f,d) ->
    let module F = (val f) in
    (module F.Functor(val (make_stacked_domain d)))

  | D_functor(F_domain f,d) ->
    let module F = (val f) in
    (module Domain.MakeStacked(F.Functor(val (make_standard_domain d))))

  | D_functor(F_simplified f,d) ->
    let module F = (val f) in
    (module Domain.MakeStacked(Simplified.MakeDomain(F.Functor(val (make_simplified_domain d)))))

  | D_compose domains  -> Combiners.Domain.Compose.make (List.map make_stacked_domain domains)
  | D_sequence domains -> Combiners.Domain.Sequence.make (prepare_stacked_sequence domains)

  | D_product(domains,reductions) ->
    if List.for_all is_simplified_domain domains
    then
      (module Domain.MakeStacked(Simplified.MakeDomain(val (make_simplified_product domains reductions))))
    else
      Combiners.Domain.Product.make
        (List.map make_stacked_domain domains)
        (List.fold_left
           (fun acc -> function DR_eval r -> r :: acc | _ -> acc)
           [] reductions)
        (List.fold_left
           (fun acc -> function DR_exec r -> r :: acc | _ -> acc)
           [] reductions)


let make (abstraction:abstraction) : (module STACKED) =
  make_stacked_domain abstraction.domain
