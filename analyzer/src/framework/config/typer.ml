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

(** Typer of configuration files *)

open Visitor


(** {2 Types used by the typer} *)
(** *************************** *)

(** Abstractions present in MOPSA *)
type abstraction =
  | A_domain
  | A_stack
  | A_value

(** Signature levels of an abstraction *)
type signature =
  | S_lowlevel
  | S_intermediate
  | S_simplified
  | S_stateless

(** Configuration of an abstraction *)
type config = {
  abstraction : abstraction;
  signature   : signature;
  structure : structure;
}

(** Structure of a configuration *)
and structure =
  | S_leaf    of string (** Leaf configuration with a name *)
  | S_chain   of operator * config list (** Chain configurations connected by an operator *)
  | S_apply   of config * config (** Application of a stack on a domain *)
  | S_product of config list * string list (** Reduced product *)
  | S_nonrel  of config (** Non-relational domain *)
  | S_cast    of config (** Cast of a configuration to a lower signature *)

(** Operator of a chain transformer *)
and operator =
  | O_seq
  | O_compose
  | O_disjoint


(** Extract the abstraction of a configuration *)
let abstraction config = config.abstraction

(** Extract the signature of a configuration *)
let signature config = config.signature



(** {2 Analyzer specification} *)
(** ************************** *)

(** A specification determines which transformers are provided by the framework *)
type spec = {
  chain   : abstraction -> operator -> signature -> bool;
  apply   : signature -> bool;
  product : abstraction -> signature -> bool;
  nonrel  : signature -> bool;
}


(** {2 Unification of signatures} *)
(** ***************************** *)

let is_same_signature (c1:config) (c2:config) : bool =
  c1.signature = c2.signature

(** Split a list of configurations into two lists. The first list
    contains the head configurations having the same signature, and the
    second contains the remaining ones. *)
let rec split (l:config list) : config list * config list =
  match l with
  | [] -> [], []
  | [n] -> [n], []
  | hd :: snd :: tl ->
    let l1, l2 = split l in
    if is_same_signature hd snd then
      hd :: snd :: l1, l2
    else
      hd :: l1, snd :: l2

(** Downgrade a signature by one level *)
let downgrade = function
  | S_lowlevel -> S_lowlevel
  | S_intermediate -> S_lowlevel
  | S_simplified -> S_intermediate
  | S_stateless -> S_simplified

(** Check if a signature [s1] can be downgraded to [s2] *)
let can_downgrade_signature s1 s2 =
  match s1, s2 with
  | S_lowlevel, _ -> false
  | _, S_lowlevel -> true

  | S_intermediate, _ -> false
  | _, S_intermediate -> true

  | S_simplified, _ -> false
  | _, S_simplified -> true

  | S_stateless, _ -> false

(** Cast a configuration [config] to signature [s] *)
let cast s config =
  if can_downgrade_signature (signature config) s
  then
    {
      config with
      signature = s;
      structure = S_cast config
    }
  else
    config

(** Unify the signature of two configurations *)
let unify c1 c2 =
  if is_same_signature c1 c2
  then c1, c2
  else
    let c1 = cast (signature c2) c1  in
    let c2 = cast (signature c1) c2 in
    c1, c2

(** Return the smallest signature of a list of configurations *)
let smallest_signature (l:config list) : signature =
  List.fold_left (fun min n ->
      let s = signature n in
      if can_downgrade_signature min s then s else min
    ) S_stateless l

(** Find the highest signature below [s] verifying the predicate [pred] *)
let rec find_available_signature s pred =
  match pred s with
  | true -> s
  | false -> find_available_signature (downgrade s) pred

(** Create a chain of a list of unified configurations *)
let unified_chain spec op l =
  let s = List.hd l |> signature in
  let a = List.hd l |> abstraction in
  if spec.chain a op s then
    {
      abstraction = a;
      signature = s;
      structure = S_chain (op, l)
    }
  else
    let s' = find_available_signature s (spec.chain a op) in
    let l' = List.map (cast s') l in
    {
      abstraction = a;
      signature = s';
      structure = S_chain (op, l')
    }


(** Create a chain of a list of configurations *)
let rec chain spec op l =
  let l1, l2 = split l in
  match l2 with
  | [] -> unified_chain spec op l1
  | _ ->
    let n1 = unified_chain spec op l1 in
    let n2 = chain spec op l2 in
    let n1, n2 = unify n1 n2 in
    unified_chain spec op [n1; n2]


(** Create a product of a list of configurations *)
let product spec l r =
  let s = smallest_signature l in
  let a = List.hd l |> abstraction in
  let s' = find_available_signature s (spec.product a) in
  let l' = List.map (cast s') l in
  {
    abstraction = a;
    signature = s';
    structure = S_product (l',r);
  }

let apply spec stack domain =
  let stack, domain = unify stack domain in
  let s = signature stack in
  let s' = find_available_signature s spec.apply in
  let stack = cast s' stack in
  let domain = cast s' domain in
  {
    abstraction = A_domain;
    signature = s';
    structure = S_apply (stack, domain);
  }

let nonrel spec value =
  let s = signature value in
  let s' = find_available_signature s spec.nonrel in
  let value = cast s' value in
  {
    abstraction = A_domain;
    signature = s';
    structure = S_nonrel value;
  }
  
let leaf_domain name = assert false

let leaf_stack name = assert false

let leaf_value name = assert false


(** {2 Domain typer} *)
(** **************** *)

(** Configuration visitor for a domain object *)
let rec domain_visitor spec = {
    leaf = leaf_domain;
    seq = (fun l -> List.map (domain spec) l |> chain spec O_seq);
    compose = (fun l -> assert false);
    apply = (fun s d -> apply spec (stack spec s) (domain spec d));
    nonrel = (fun v -> nonrel spec (value spec v));
    product = (fun l r -> product spec (List.map (domain spec) l) r);
    disjoint = (fun l -> assert false);
  }

(** Create a domain configuration from a json object *)
and domain spec json : config =
  visit (domain_visitor spec) json


(** {2 Stacks} *)
(** ********** *)

(** Configuration visitor for a stack object *)
and stack_visitor spec = {
    leaf = leaf_stack;
    seq = (fun l -> List.map (stack spec) l |> chain spec O_seq);
    compose = (fun l -> List.map (stack spec) l |> chain spec O_compose);
    apply = (fun s d -> assert false);
    nonrel = (fun v -> assert false);
    product = (fun l r -> assert false);
    disjoint = (fun l -> assert false);
  }

(** Create a stack configuration from a json object *)
and stack spec json : config =
  visit (stack_visitor spec) json


(** {2 Values} *)
(** ********** *)

(** Configuration visitor for a value object *)
and value_visitor spec = {
    leaf = leaf_value;
    seq = (fun l -> List.map (value spec) l |> chain spec O_seq);
    compose = (fun l -> assert false);
    apply = (fun s d -> assert false);
    nonrel = (fun v -> assert false);
    product = (fun l r -> product spec (List.map (value spec) l) r);
    disjoint = (fun l -> List.map (value spec) l |> chain spec O_disjoint);
  }

(** Create a value configuration from a json object *)
and value spec json : config =
  visit (value_visitor spec) json
