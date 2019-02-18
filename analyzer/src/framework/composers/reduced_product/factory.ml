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

(** N-ary reduced product of abstract abstractions. *)

open Core
open Ast
open Manager
open Domain
open Value
open Pool


(** Value product *)
(** ************* *)

type vp = V : 'a value_pool -> vp

let type_value (type a) (v : (module VALUE with type t = a)) =
    let module V = (val v) in
    (module V : VALUE with type t = a)

let rec type_value_pool : (module VALUE) list -> vp = function
  | [] -> V Nil
  | hd :: tl ->
    let module V = (val hd) in
    let v = type_value (module V) in
    let V tl = type_value_pool tl in
    V (Cons (v, tl))

let make_value_product
    (pool: (module VALUE) list)
    (value_rules: (module Reductions.Value_reduction.REDUCTION) list)
  : (module DOMAIN) =
  let V pool = type_value_pool pool in

  let create_product (type a) (pool: a value_pool) =
    let module V = Products.Value_product.Make(struct
        type t = a
        let pool = pool
        let value_rules = value_rules
      end) in
    let module D = Domains.Nonrel.Make(V) in
    (module D : DOMAIN)
  in

  create_product pool



(** Domain product *)
(** ************** *)

type dp = D : 'a domain_pool -> dp

let type_domain (type a) (d : (module Stacked.S with type t = a)) =
    let module D = (val d) in
    (module D : Stacked.S with type t = a)

let rec type_domain_pool : (module Stacked.S) list -> dp = function
  | [] -> D Nil
  | hd :: tl ->
    let module D = (val hd) in
    let d = type_domain (module D) in
    let D tl = type_domain_pool tl in
    D (Cons (d, tl))

let make_domain_product
    (pool: (module Stacked.S) list)
    (over: (module DOMAIN))
    (post_rules: (module Reductions.Post_reduction.REDUCTION) list)
    (eval_rules: (module Reductions.Eval_reduction.REDUCTION) list)
  : (module DOMAIN) =
  let D pool = type_domain_pool pool in
  let module Over = (val over) in

  let create_product (type u) (pool: u domain_pool) =
    let module D = Products.Domain_product.Make
        (Over)
        (struct
          type t = u
          type v = unit
          let pool = pool
          let post_rules = post_rules
          let eval_rules = eval_rules
          let nonrel_man (man:('a, t * Over.t) man) : ('a, v) nonrel_man = {
            pool = Nil;
            get_var_value = (fun _ _ _ -> assert false);
            set_var_value = (fun _ _ _ a -> a);
          }
        end)
    in
    (module D : DOMAIN)
  in

  create_product pool


let make_mixed_product
    (domain_pool: (module Stacked.S) list)
    (value_pool: (module VALUE) list)
    (over: (module DOMAIN))
    (post_rules: (module Reductions.Post_reduction.REDUCTION) list)
    (eval_rules: (module Reductions.Eval_reduction.REDUCTION) list)
    (value_rules: (module Reductions.Value_reduction.REDUCTION) list)
  : (module DOMAIN) =
  let V vpool = type_value_pool value_pool in
  let D dpool = type_domain_pool domain_pool in
  let module Over = (val over) in

  let create_product (type a b) (pool: a domain_pool) (vpool: b value_pool) =

    let module V = Products.Value_product.Make(struct
        type t = b
        let pool = vpool
        let value_rules = value_rules
      end)
    in

    let module NR = Domains.Nonrel.Make(V) in
    let module SNR = Stacked.MakeStacked(NR) in

    let module D = Products.Domain_product.Make
        (Over)
        (struct
          type t = SNR.t * a
          type v = b
          let pool : t domain_pool = Cons((module SNR), pool)
          let post_rules = post_rules
          let eval_rules = eval_rules
          let nonrel_man (man:('a, t * Over.t) man) : ('a, v) nonrel_man = {
            pool = vpool;
            get_var_value = (fun id var a -> man.get a |>
                                             fst |>
                                             fst |>
                                             NR.find var |>
                                             V.man.get_value id
                            );
            set_var_value = (fun id var v a ->
                let ((nr, tl), o) = man.get a in
                let vv = NR.find var nr in
                let vv' = V.man.set_value id v vv |>
                          V.reduce
                in
                let nr' = NR.add var vv'.Channel.value nr in
                (* FIXME: reduction channels produced by [reduce] are lost here! *)
                man.set ((nr', tl), o) a
              );
          }
        end)
    in
    (module D : DOMAIN)
  in

  create_product dpool vpool


let find_safe finder d =
  try
    finder d
  with
  | Not_found ->
    Exceptions.panic "Domain %s not found" d


let make (pool: string list) (rules: string list) (over: (module DOMAIN)) : (module DOMAIN) =
  let stack_domain_pool, tl = List.partition Stacked.mem_domain pool in
  let domain_pool, value_pool = List.partition Domain.mem_domain tl in

  let stack_domain_pool = List.map (find_safe Stacked.find_domain) stack_domain_pool in
  let domain_pool = List.map (find_safe Domain.find_domain) domain_pool |>
                    List.map (fun d ->
                        let module D = (val d : DOMAIN) in
                        let module D' = Stacked.MakeStacked(D) in
                        (module D' : Stacked.S)
                      )
  in
  let domain_pool = stack_domain_pool @ domain_pool in

  let value_pool = List.map (find_safe Value.find_value) value_pool in

  let post_rules, other_rules = List.partition (fun rule -> List.mem_assoc rule !Reductions.Post_reduction.reductions) rules in
  let eval_rules, value_rules = List.partition (fun rule -> List.mem_assoc rule !Reductions.Eval_reduction.reductions) other_rules in

  let post_rules = List.map (find_safe Reductions.Post_reduction.find_reduction) post_rules in
  let eval_rules = List.map (find_safe Reductions.Eval_reduction.find_reduction) eval_rules in
  let value_rules = List.map (find_safe Reductions.Value_reduction.find_reduction) value_rules in

  match domain_pool, value_pool with
  | [], [] -> Exceptions.panic "reduced product: empty pool"
  | [], _ -> make_value_product value_pool value_rules
  | _, [] -> make_domain_product domain_pool over post_rules eval_rules
  | _, _ -> make_mixed_product domain_pool value_pool over post_rules eval_rules value_rules
