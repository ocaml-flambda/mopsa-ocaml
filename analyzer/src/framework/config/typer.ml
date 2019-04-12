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


(** {2 Types} *)
(** ********* *)

type abstraction =
  | A_domain
  | A_stack
  | A_value

type signature =
  | S_lowlevel
  | S_intermediate
  | S_simplified
  | S_stateless

type operator =
  | O_seq
  | O_compose
  | O_disjoint

type config = {
  abstraction : abstraction;
  signature   : signature;
  kind : kind;
}

and kind =
  | K_leaf of string
  | K_chain of operator * config list
  | K_apply of config * config
  | K_product of config list * string list
  | K_nonrel of config
  | K_cast of config


let abstraction config = config.abstraction

let signature config = config.signature

let kind config = config.kind



(** {2 Available transformers} *)
(** ************************** *)

type arch = {
  chain : abstraction -> operator -> signature -> bool;
  apply : signature -> bool;
  product : abstraction -> signature -> bool;
  nonrel : signature -> bool;
}


(** {2 Unification} *)
(** *************** *)

let is_same_signature n1 n2 =
  n1.signature = n2.signature

let rec split l =
  match l with
  | [] -> [], []
  | [n] -> [n], []
  | hd :: snd :: tl ->
    let l1, l2 = split l in
    if is_same_signature hd snd then
      hd :: snd :: l1, l2
    else
      hd :: l1, snd :: l2

let downgrade = function
  | S_lowlevel -> S_lowlevel
  | S_intermediate -> S_lowlevel
  | S_simplified -> S_intermediate
  | S_stateless -> S_simplified

let can_downgrade_signature s1 s2 =
  match s1, s2 with
  | S_lowlevel, _ -> false
  | _, S_lowlevel -> true

  | S_intermediate, _ -> false
  | _, S_intermediate -> true

  | S_simplified, _ -> false
  | _, S_simplified -> true

  | S_stateless, _ -> false

let cast s config =
  if can_downgrade_signature (signature config) s
  then
    {
      config with
      signature = s;
      kind = K_cast config
    }
  else
    config

let unify n1 n2 =
  if is_same_signature n1 n2
  then n1, n2
  else
    let n1 = cast (signature n2) n1  in
    let n2 = cast (signature n1) n2 in
    n1, n2

let smallest_signature l =
  List.fold_left (fun min n ->
      let s = signature n in
      if can_downgrade_signature min s then s else min
    ) S_stateless l

let rec find_available_signature s pred =
  match pred s with
  | true -> s
  | false -> find_available_signature (downgrade s) pred

let unified_chain arch op l =
  let s = List.hd l |> signature in
  if arch.chain op s then
    {
      abstraction = List.hd l |> abstraction;
      signature = s;
      kind = K_chain (op, l)
    }
  else
    let s' = find_available_signature s (arch.chain op) in
    let l' = List.map (cast s') l in
    {
      abstraction = List.hd l' |> abstraction;
      signature = s';
      kind = K_chain (op, l')
    }


let rec chain arch op l =
  let l1, l2 = split l in
  match l2 with
  | [] -> unified_chain arch op l1
  | _ ->
    let n1 = unified_chain arch op l1 in
    let n2 = chain arch op l2 in
    let n1, n2 = unify n1 n2 in
    unified_chain arch op [n1; n2]


let product arch l r =
  let s = smallest_signature l in
  let s' = find_available_signature s arch.product in
  let l' = List.map (cast s') l in
  {
    abstraction = List.hd l' |> abstraction;
    signature = s';
    kind = K_product (l',r);
  }


(** {2 Domains} *)
(** *********** *)

let rec domain_visitor arch = {
    leaf = (fun name ->
        {
          abstraction = A_domain;
          signature = S_lowlevel;
          kind = K_leaf name;
        }
      );

    seq = (fun l -> List.map (domain_typer arch) l |> chain arch O_seq);

    compose = (fun l -> assert false);

    apply = (fun s d ->
        {
          abstraction = A_domain;
          signature = S_lowlevel;
          kind = K_apply (stack_typer arch s, domain_typer arch d);
        }
      );

    nonrel = (fun v -> {
          abstraction = A_domain;
          signature = S_lowlevel;
          kind = K_nonrel (value_typer arch v);
        }
      );

    product = (fun l r -> product arch (List.map (domain_typer arch) l) r);

  }

and domain_typer arch json : config =
  visit (domain_visitor arch) json


(** {2 Stacks} *)
(** ********** *)

and stack_visitor arch = {
    leaf = (fun name ->
        {
          abstraction = A_stack;
          signature = S_lowlevel;
          kind = K_leaf name;
        }
      );

    seq = (fun l -> List.map (stack_typer arch) l |> chain arch O_seq);

    compose = (fun l -> List.map (stack_typer arch) l |> chain arch O_compose);

    apply = (fun s d -> assert false);

    nonrel = (fun v -> assert false);

    product = (fun l r -> assert false);

  }

and stack_typer arch json : config =
  visit (stack_visitor arch) json


(** {2 Values} *)
(** ********** *)

and value_visitor arch = {
    leaf = (fun name ->
        {
          abstraction = A_value;
          signature = S_lowlevel;
          kind = K_leaf name;
        }
      );

    seq = (fun l -> List.map (value_typer arch) l |> chain arch O_seq);

    compose = (fun l -> assert false);

    apply = (fun s d -> assert false);

    nonrel = (fun v -> assert false);

    product = (fun l r -> product arch (List.map (value_typer arch) l) r);

  }

and value_typer arch json : config =
  visit (value_visitor arch) json


(** {2 Entry point} *)
(** *************** *)

let typer arch json : config =
  domain_typer arch json
