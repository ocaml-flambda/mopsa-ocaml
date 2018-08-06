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

type vp = V : 'a value_pool -> vp

let make_value_product (pool: (module VALUE) list) (rules: (module Reductions.Value_reduction.VALUE_REDUCTION) list) : (module DOMAIN) =
  let type_value (type a) (v : (module VALUE with type t = a)) =
    let module V = (val v) in
    (module V : VALUE with type t = a)
  in
  
  let rec type_pool : (module VALUE) list -> vp = function
    | [] -> V Nil
    | hd :: tl ->
      let module V = (val hd) in
      let v = type_value (module V) in
      let V tl = type_pool tl in
      V (Cons (v, tl))
  in
  
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
  
  let V pool = type_pool pool in
  create_product pool


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
  | _ -> assert false
 
  (* let type_domain (type a) (d : (module DOMAIN with type t = a)) =
   *   let module D = (val d) in
   *   (module D : DOMAIN with type t = a)
   * in
   * 
   * let rec type_pool : (module DOMAIN) list -> xpool = function
   *   | [] -> P Nil
   *   | hd :: tl ->
   *     let module D = (val hd) in
   *     let d = type_domain (module D) in
   *     let P tl = type_pool tl in
   *     P (Cons (d, tl))
   * in
   * 
   * let create_product (type a) (pool: a pool) =
   *   let module D = Make(struct type t = a let pool = pool let state_rules = state_rules let eval_rules = eval_rules end) in
   *   (module D : DOMAIN)
   * in
   * 
   * let P pool = type_pool pool in
   * create_product pool *)
