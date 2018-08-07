(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** N-ary reduced product of abstract abstractions. *)

open Essentials
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

let make_value_product (pool: (module VALUE) list) (rules: (module Reductions.Value_reduction.REDUCTION) list) : (module DOMAIN) =    
  let V pool = type_value_pool pool in

  let create_product (type a) (pool: a value_pool) =
    let module V = Products.Value_product.Make(struct
        type t = a
        let pool = pool
        let display = "?"
        let rules = rules
      end) in
    let module D = Nonrel.Make(V) in
    (module D : DOMAIN)
  in
  
  create_product pool



(** Domain product *)
(** ************** *)

type dp = D : 'a domain_pool -> dp

let type_domain (type a) (d : (module DOMAIN with type t = a)) =
    let module D = (val d) in
    (module D : DOMAIN with type t = a)

let rec type_domain_pool : (module DOMAIN) list -> dp = function
  | [] -> D Nil
  | hd :: tl ->
    let module D = (val hd) in
    let d = type_domain (module D) in
    let D tl = type_domain_pool tl in
    D (Cons (d, tl))

let make_domain_product (pool: (module DOMAIN) list) (state_rules: (module Reductions.State_reduction.REDUCTION) list) (eval_rules: (module Reductions.Eval_reduction.REDUCTION) list) : (module DOMAIN) =    
  let D pool = type_domain_pool pool in

  let create_product (type u) (pool: u domain_pool) =
    let module D = Products.Domain_product.Make(struct
        type t = u
        type v = unit
        let pool = pool
        let state_rules = state_rules
        let eval_rules = eval_rules
        let nonrel_man (man:('a, t) man) : ('a, v) nonrel_man = {
          pool = Nil;
          get = (fun _ _ _ -> assert false);
          set = (fun _ _ _ a -> a);
        }
      end) in
    (module D : DOMAIN)
  in
  
  create_product pool


let make_mixed_product
    (domain_pool: (module DOMAIN) list)
    (value_pool: (module VALUE) list)
    (state_rules: (module Reductions.State_reduction.REDUCTION) list)
    (eval_rules: (module Reductions.Eval_reduction.REDUCTION) list)
    (value_rules: (module Reductions.Value_reduction.REDUCTION) list)
  : (module DOMAIN) =
  let V vpool = type_value_pool value_pool in
  let nr = make_value_product value_pool value_rules in

  let domain_pool' = nr :: domain_pool in
  let D pool = type_domain_pool domain_pool' in

  let create_product (type a b) (pool: a domain_pool) (vpool: b value_pool) =
    let module D = Products.Domain_product.Make(struct
        type t = a
        type v = b
        let pool = pool
        let state_rules = state_rules
        let eval_rules = eval_rules
        let nonrel_man (man:('a, t) man) : ('a, v) nonrel_man = {
          pool = vpool;
          get = (fun _ _ _ -> assert false);
          set = (fun _ _ _ a -> a);
        }
      end) in
    (module D : DOMAIN)
  in
  
  create_product pool vpool




let make (pool: string list) (rules: string list) : (module DOMAIN) =
  let domain_pool, value_pool = List.partition Domain.mem_domain  pool in

  let domain_pool = List.map Domain.find_domain domain_pool in
  let value_pool = List.map Value.find_value value_pool in
  
  let state_rules, other_rules = List.partition (fun rule -> List.mem_assoc rule !Reductions.State_reduction.reductions) rules in
  let eval_rules, value_rules = List.partition (fun rule -> List.mem_assoc rule !Reductions.Eval_reduction.reductions) other_rules in

  let state_rules = List.map Reductions.State_reduction.find_reduction state_rules in
  let eval_rules = List.map Reductions.Eval_reduction.find_reduction eval_rules in
  let value_rules = List.map Reductions.Value_reduction.find_reduction value_rules in

  match domain_pool, value_pool with
  | [], [] -> Debug.fail "reduced product: empty pool"
  | [], _ -> make_value_product value_pool value_rules
  | _, [] -> make_domain_product domain_pool state_rules eval_rules
  | _, _ -> make_mixed_product domain_pool value_pool state_rules eval_rules value_rules
