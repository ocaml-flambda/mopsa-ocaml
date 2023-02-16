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


(** Build a domain from a configuration *)

open Mopsa_utils
open Core.Route
open Syntax
open Sig.Combiner.Stacked
open Sig.Combiner.Domain
open Sig.Combiner.Stateless
open Sig.Combiner.Simplified
open Sig.Abstraction.Value


(** {2 Values} *)
(** ********** *)

let rec make_value (value:value) : (module VALUE) =
  match value with
  | V_value v                     -> v
  | V_union values                -> Combiners.Value.Union.make (List.map make_value values)
  | V_product (values,reductions) -> Combiners.Value.Product.make (List.map make_value values) reductions
  | V_functor(f, vv)              ->
    let module F = (val f) in
    (module F.Functor(val (make_value vv)))




(** {2 Simplified domains} *)
(** ********************** *)

let make_nonrel_domain (value:value) : (module SIMPLIFIED_COMBINER) =
  let module V = (val (make_value value)) in
  (module SimplifiedToCombiner(Combiners.Value.Nonrel.Make(V)))

let rec is_simplified_domain d =
  match d.domain_kind with
  | D_simplified _               -> true
  | D_nonrel _                   -> true
  | D_product (dl,_)             -> List.for_all is_simplified_domain dl
  | D_functor(F_simplified _, d) -> is_simplified_domain d
  |  _                           -> false

let rec make_simplified_product (domains:domain list) (reductions:domain_reduction list) : (module SIMPLIFIED_COMBINER) =
  Combiners.Domain.Simplified_product.make
    (List.map make_simplified_domain domains)
    ~rules:(List.fold_left
       (fun acc -> function DR_simplified r -> r :: acc | _ -> acc)
       [] reductions)

and make_simplified_domain_without_semantic (domain:domain) : (module SIMPLIFIED_COMBINER) =
  match domain.domain_kind with
  | D_simplified d                -> (module SimplifiedToCombiner(val d))
  | D_nonrel value                -> make_nonrel_domain value
  | D_product(domains,reductions) -> make_simplified_product domains reductions
  | D_functor(F_simplified f, d) ->
    let module F = (val f) in
    (module SimplifiedToCombiner
         (F.Functor
            (CombinerToSimplified
               (val make_simplified_domain d : SIMPLIFIED_COMBINER))))
  | _ -> Exceptions.panic "Invalid configuration of simplified domain: %a" pp_domain domain

and make_simplified_domain (domain:domain) : (module SIMPLIFIED_COMBINER) =
  match domain.domain_semantic with
  | None -> make_simplified_domain_without_semantic domain
  | Some semantic ->
    let module D = (val (make_simplified_domain_without_semantic domain)) in
    (module
      (struct
        include D
        let semantics = Core.Semantic.SemanticSet.add semantic D.semantics
        let routing_table = add_routes (Semantic semantic) domains D.routing_table
      end)
      : SIMPLIFIED_COMBINER
    )


(** {2 Stateless domains} *)
(** ********************** *)


let rec is_stateless_domain d =
  match d.domain_kind with
  | D_stateless _   -> true
  | D_switch (dl) -> List.for_all is_simplified_domain dl
  |  _              -> false

let rec make_stateless_domain_without_semantic (domain:domain) : (module STATELESS_COMBINER) =
  match domain.domain_kind with
  | D_stateless d -> (module StatelessToCombiner(val d))
  | D_switch dl -> Combiners.Domain.Stateless_switch.make (List.map make_stateless_domain dl)
  | _ -> Exceptions.panic "Invalid configuration of stateless domain: %a" pp_domain domain

and make_stateless_domain (domain:domain) : (module STATELESS_COMBINER) =
  match domain.domain_semantic with
  | None -> make_stateless_domain_without_semantic domain
  | Some semantic ->
    let module D = (val (make_stateless_domain_without_semantic domain)) in
    (module
      (struct
        include D
        let semantics = Core.Semantic.SemanticSet.add semantic D.semantics
        let routing_table = add_routes (Semantic semantic) domains D.routing_table
      end)
      : STATELESS_COMBINER
    )



(** {2 Standard domains} *)
(** ******************** *)

let is_standard_domain d =
  match d.domain_kind with
  | D_domain _ -> true
  | D_functor(F_domain _, _) -> true
  | _ -> false

let rec make_standard_domain (domain:domain) : (module DOMAIN_COMBINER) =
  if is_simplified_domain domain then
    (module SimplifiedToStandard(val (make_simplified_domain domain))) else
  if is_stateless_domain domain then
    (module StatelessToDomain(val (make_stateless_domain domain)))
  else
    match domain.domain_kind with
    | D_domain d -> (module DomainToCombiner(val d))
    | D_functor(F_domain f, d) ->
      let module F = (val f) in
      (module DomainToCombiner
           (F.Functor
              (CombinerToDomain
                 (val make_standard_domain d : DOMAIN_COMBINER))))
    | _ -> Exceptions.panic "Invalid configuration of standard domain: %a" pp_domain domain



(** {2 Stacked domains} *)
(** ******************** *)

let rec prepare_stacked_switch (domains:domain list) : (module STACKED_COMBINER) list =
  match domains with
  | [] -> []
  | { domain_kind = D_stateless _ } :: _ ->
    let rec extract_stateless_prefix = function
      | [] -> [], []
      | { domain_kind = D_stateless _ } as hd :: tl ->
        let a,b = extract_stateless_prefix tl in
        hd::a,b
      | l -> [],l
    in
    let l1,l2 = extract_stateless_prefix domains in
    let hd = make_stateless_domain (mk_domain (D_switch l1)) in
    (module StandardToStacked(StatelessToDomain(val hd))) :: prepare_stacked_switch l2
  | hd::tl -> make_stacked_domain hd :: prepare_stacked_switch tl

and make_stacked_domain_without_semantic (domain:domain) : (module STACKED_COMBINER) =
  match domain.domain_kind with
  | D_stacked d     -> (module StackedToCombiner(val d))
  | D_domain d      -> (module StandardToStacked(DomainToCombiner(val d)))
  | D_simplified d  -> (module StandardToStacked(SimplifiedToStandard(SimplifiedToCombiner(val d))))
  | D_stateless d   -> (module StandardToStacked(StatelessToDomain(StatelessToCombiner(val d))))
  | D_nonrel(value) -> (module StandardToStacked(SimplifiedToStandard(val (make_nonrel_domain value))))

  | D_functor(F_stacked f,d) ->
    let module F = (val f) in
    (module StackedToCombiner
         (F.Functor
            (CombinerToStacked
               (val make_stacked_domain d : STACKED_COMBINER))))

  | D_functor(F_domain f,d) ->
    let module F = (val f) in
    (module StandardToStacked
         (DomainToCombiner
            (F.Functor
               (CombinerToDomain
                  (val make_standard_domain d : DOMAIN_COMBINER)))))

  | D_functor(F_simplified f,d) ->
    let module F = (val f) in
    (module StandardToStacked
         (SimplifiedToStandard
            (SimplifiedToCombiner
               (F.Functor
                  (CombinerToSimplified
                     (val make_simplified_domain d : SIMPLIFIED_COMBINER))))))

  | D_compose domains  -> Combiners.Domain.Compose.make (List.map make_stacked_domain domains)
  | D_switch domains -> Combiners.Domain.Switch.make (prepare_stacked_switch domains)

  | D_product(domains,reductions) ->
    if List.for_all is_simplified_domain domains
    then
      (module StandardToStacked(SimplifiedToStandard(val (make_simplified_product domains reductions))))
    else
      Combiners.Domain.Product.make
        (List.map make_stacked_domain domains)
        ~eval_rules:(List.fold_left
           (fun acc -> function DR_eval r -> r :: acc | _ -> acc)
           [] reductions)
        ~exec_rules:(List.fold_left
           (fun acc -> function DR_exec r -> r :: acc | _ -> acc)
           [] reductions)

and make_stacked_domain (domain:domain) : (module STACKED_COMBINER) =
  match domain.domain_semantic with
  | None -> make_stacked_domain_without_semantic domain
  | Some semantic ->
    let module D = (val (make_stacked_domain_without_semantic domain)) in
    (module
      (struct
        include D
        let semantics = Core.Semantic.SemanticSet.add semantic D.semantics
        let routing_table = add_routes (Semantic semantic) domains D.routing_table
      end)
      : STACKED_COMBINER
    )

let from_json (domain:domain) : (module STACKED_COMBINER) =
  let d = make_stacked_domain domain in
  (* Add an empty domain below the abstraction to ensure that leave domains
     have always a [Below] route *)
  Combiners.Domain.Compose.make [d; (module EmptyDomain)]
