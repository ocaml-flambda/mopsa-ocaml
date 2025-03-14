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

(** Syntax tree of configuration files *)

open Sig.Abstraction.Stacked
open Sig.Abstraction.Domain
open Sig.Abstraction.Simplified
open Sig.Abstraction.Simplified_functor
open Sig.Abstraction.Stateless
open Sig.Abstraction.Partitioning
open Sig.Abstraction.Value
open Sig.Abstraction.Value_functor
open Sig.Reduction.Exec
open Sig.Reduction.Eval
open Sig.Reduction.Value
open Sig.Reduction.Simplified

open Format


type abstraction = {
  domain: domain;
  language : string;
}

and domain = {
  domain_kind : domain_kind;
  domain_semantic : string option;
}

and domain_kind =
  | D_stacked    of (module STACKED)
  | D_domain     of (module DOMAIN)
  | D_simplified of (module SIMPLIFIED)
  | D_stateless  of (module STATELESS)
  | D_functor    of domain_functor * domain
  | D_nonrel     of value
  | D_switch   of domain list
  | D_compose    of domain list
  | D_product    of domain list * domain_reduction list

and domain_functor =
  | F_simplified of (module SIMPLIFIED_FUNCTOR)
  | F_stacked of domain
  | F_partitioning of (module PARTITIONING)

and value =
  | V_value   of (module VALUE)
  | V_functor of value_functor * value
  | V_union   of value list
  | V_product of value list * value_reduction list

and value_functor = (module VALUE_FUNCTOR)

and domain_reduction =
  | DR_exec       of (module EXEC_REDUCTION)
  | DR_eval       of (module EVAL_REDUCTION)
  | DR_simplified of (module SIMPLIFIED_REDUCTION)

and value_reduction = (module VALUE_REDUCTION)

let mk_domain ?(semantic=None) kind = { domain_kind = kind; domain_semantic = semantic } 

let pp_value_reduction fmt (r:value_reduction) =
  let module R = (val r) in
  pp_print_string fmt R.name

let pp_domain_reduction fmt = function
  | DR_exec r ->
    let module R = (val r) in
    pp_print_string fmt R.name

  | DR_eval r ->
    let module R = (val r) in
    pp_print_string fmt R.name

  | DR_simplified r ->
    let module R = (val r) in
    pp_print_string fmt R.name

let rec pp_value fmt = function
  | V_value v->
    let module V = (val v) in
    pp_print_string fmt V.name

  | V_functor(f,v) ->
    let module F = (val f) in
    fprintf fmt "%s(%a)" F.name pp_value v

  | V_union vl ->
    fprintf fmt "(%a)"
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " ∪ ")
         pp_value) vl

  | V_product (vl,[]) ->
    fprintf fmt "(%a)"
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " ∩ ")
         pp_value ) vl

  | V_product (vl,rl) ->
    fprintf fmt "(%a ↓ %a)"
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " ∩ ")
         pp_value ) vl
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " | ")
         pp_value_reduction ) rl



let rec pp_domain_functor fmt = function
  | F_stacked f ->
    pp_domain fmt f

  | F_simplified f ->
    let module F = (val f) in
    pp_print_string fmt F.name

  | F_partitioning f ->
    let module F = (val f) in
    pp_print_string fmt F.name


and pp_domain fmt d =
  match d.domain_semantic with
  | None -> pp_domain_kind fmt d.domain_kind
  | Some semantic -> fprintf fmt "[%s] %a" semantic pp_domain_kind d.domain_kind

and pp_domain_kind fmt = function
  | D_stacked d ->
    let module D = (val d) in
    pp_print_string fmt ("[S]" ^ D.name)

  | D_domain d ->
    let module D = (val d) in
    pp_print_string fmt ("[D]" ^ D.name)

  | D_simplified d ->
    let module D = (val d) in
    pp_print_string fmt ("[L]" ^ D.name)

  | D_stateless d ->
    let module D = (val d) in
    pp_print_string fmt ("[U]" ^ D.name)

  | D_functor(f,d) ->
    fprintf fmt "%a(%a)" pp_domain_functor f pp_domain d

  | D_nonrel v ->
    fprintf fmt "nonrel(%a)" pp_value v

  | D_switch dl ->
    fprintf fmt "(%a)"
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " ; ")
         pp_domain) dl

  | D_compose dl ->
    fprintf fmt "(%a)"
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " o ")
         pp_domain) dl

  | D_product(dl,[]) ->
    fprintf fmt "(%a)"
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " ∧ ")
         pp_domain) dl

  | D_product(dl,rl) ->
    fprintf fmt "(%a ↓ %a)"
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " ∧ ")
         pp_domain) dl
      (pp_print_list
         ~pp_sep:(fun fmt () -> pp_print_string fmt " | ")
         pp_domain_reduction) rl
