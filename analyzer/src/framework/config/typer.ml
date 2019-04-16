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


let debug fmt = Debug.debug ~channel:"framework.config.typer" fmt


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

let signature config = config.signature

let abstraction config = config.abstraction


(** {2 Pretty printers} *)
(** ******************* *)

open Format

let pp_abstraction fmt = function
  | A_domain -> pp_print_string fmt "𝒟"
  | A_stack -> pp_print_string fmt "𝒮"
  | A_value -> pp_print_string fmt "𝒱"

let pp_signature fmt = function
  | S_lowlevel -> pp_print_string fmt "0"
  | S_intermediate -> pp_print_string fmt "1"
  | S_simplified -> pp_print_string fmt "2"
  | S_stateless-> pp_print_string fmt "3"

let pp_operator fmt = function
  | O_seq -> pp_print_string fmt " ; "
  | O_compose -> pp_print_string fmt " o "
  | O_disjoint -> pp_print_string fmt " ∨ "

let rec pp_config fmt config =
  match config.structure with
  | S_leaf name ->
    pp_print_string fmt name

  | S_chain(op,l) ->
    fprintf fmt "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> pp_operator fmt op) pp_config) l

  | S_apply(s,d) ->
    fprintf fmt "%a(%a)" pp_config s pp_config d

  | S_product(l,[]) ->
    fprintf fmt "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ∧ ") pp_config) l

  | S_product(l,r) ->
    fprintf fmt "(%a | %a)"
      (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ∧ ") pp_config) l
      (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " : ") pp_print_string) r

  | S_nonrel v ->
    fprintf fmt "𝐕(%a)" pp_config v

  | S_cast c ->
    fprintf fmt "(%a) %a" pp_signature config.signature pp_config c


(** {2 Analyzer specification} *)
(** ************************** *)

(** A specification determines which transformers are provided by the framework *)
type spec = {
  chain   : abstraction -> operator -> signature -> bool;
  apply   : signature -> bool;
  product : abstraction -> signature -> bool;
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
    if is_same_signature hd snd then
      let l1, l2 = split (snd :: tl) in
      hd :: l1, l2
    else
      [hd], snd :: tl

(** Downgrade a signature by one level *)
let downgrade = function
  | S_lowlevel -> S_lowlevel
  | S_intermediate -> S_lowlevel
  | S_simplified -> S_intermediate
  | S_stateless -> S_intermediate

let subset (s1:signature) (s2:signature) =
  match s1, s2 with
  | _, S_lowlevel -> true
  | S_lowlevel, _ -> false

  | _, S_intermediate -> true
  | S_intermediate, _ -> false

  | S_simplified, S_simplified -> true

  | S_stateless, S_simplified -> true

  | _ -> false


let strict_subset (s1:signature) (s2:signature) =
  if s1 = s2 then false
  else subset s1 s2


(** Cast a configuration [config] to signature [s] *)
let cast s config =
  if strict_subset config.signature s
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
    let c1 = cast c2.signature c1  in
    let c2 = cast c1.signature c2 in
    c1, c2

(** Return the smallest signature of a list of configurations *)
let smallest_signature (l:config list) : signature =
  List.fold_left (fun min c ->
      let s = c.signature in
      if strict_subset min s then s else min
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
  let value = cast S_lowlevel value in
  {
    abstraction = A_domain;
    signature = S_simplified;
    structure = S_nonrel value;
  }

let leaf_domain name =
  let signature =
    if Core.Sig.Domain.Lowlevel.mem_domain name then S_lowlevel
    else if Core.Sig.Domain.Intermediate.mem_domain name then S_intermediate
    else if Core.Sig.Domain.Simplified.mem_domain name then S_simplified
    else if Core.Sig.Domain.Stateless.mem_domain name then S_stateless
    else Exceptions.panic "domain %s not found" name
  in
  {
    abstraction = A_domain;
    signature = signature;
    structure = S_leaf name
  }

let leaf_stack name =
  let signature =
    if Core.Sig.Stacked.Lowlevel.mem_stack name then S_lowlevel
    else if Core.Sig.Stacked.Intermediate.mem_stack name then S_intermediate
    else if Core.Sig.Stacked.Stateless.mem_stack name then S_stateless
    else Exceptions.panic "stack %s not found" name
  in
  {
    abstraction = A_stack;
    signature = signature;
    structure = S_leaf name
  }


let leaf_value name =
  let signature =
    if Core.Sig.Value.Lowlevel.mem_value name then S_lowlevel
    else if Core.Sig.Value.Intermediate.mem_value name then S_intermediate
    else if Core.Sig.Value.Simplified.mem_value name then S_simplified
    else Exceptions.panic "value %s not found" name
  in
  {
    abstraction = A_value;
    signature = signature;
    structure = S_leaf name
  }



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
