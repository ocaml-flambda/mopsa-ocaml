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

let unify_list l =
  let smallest_signature = List.fold_left (fun min n ->
      let s = signature n in
      if can_downgrade_signature min s then s else min
    ) S_stateless l
  in
  List.map (cast smallest_signature) l

let unified_chain op l = {
  abstraction = List.hd l |> abstraction;
  signature = List.hd l |> signature;
  kind = K_chain (op, l)
}


let rec chain op l =
  let l1, l2 = split l in
  match l2 with
  | [] -> unified_chain op l1
  | _ ->
    let n1 = unified_chain op l1 in
    let n2 = chain op l2 in
    let n1, n2 = unify n1 n2 in
    unified_chain op [n1; n2]


let product l r =
  let l = unify_list l in
  {
    abstraction = List.hd l |> abstraction;
    signature = List.hd l |> signature;
    kind = K_product (l,r);
  }


(** {2 Domains} *)
(** *********** *)

let rec domain_visitor = {
    leaf = (fun name ->
        {
          abstraction = A_domain;
          signature = S_lowlevel;
          kind = K_leaf name;
        }
      );

    seq = (fun l -> List.map domain_typer l |> chain O_seq);

    compose = (fun l -> assert false);

    apply = (fun s d ->
        {
          abstraction = A_domain;
          signature = S_lowlevel;
          kind = K_apply (stack_typer s, domain_typer d);
        }
      );

    nonrel = (fun v -> {
          abstraction = A_domain;
          signature = S_lowlevel;
          kind = K_nonrel (value_typer v);
        }
      );

    product = (fun l r -> product (List.map domain_typer l) r);

  }

and domain_typer json : config =
  visit domain_visitor json


(** {2 Stacks} *)
(** ********** *)

and stack_visitor = {
    leaf = (fun name ->
        {
          abstraction = A_stack;
          signature = S_lowlevel;
          kind = K_leaf name;
        }
      );

    seq = (fun l -> List.map stack_typer l |> chain O_seq);

    compose = (fun l -> List.map stack_typer l |> chain O_compose);

    apply = (fun s d -> assert false);

    nonrel = (fun v -> assert false);

    product = (fun l r -> assert false);

  }

and stack_typer json : config =
  visit stack_visitor json


(** {2 Values} *)
(** ********** *)

and value_visitor = {
    leaf = (fun name ->
        {
          abstraction = A_value;
          signature = S_lowlevel;
          kind = K_leaf name;
        }
      );

    seq = (fun l -> List.map value_typer l |> chain O_seq);

    compose = (fun l -> assert false);

    apply = (fun s d -> assert false);

    nonrel = (fun v -> assert false);

    product = (fun l r -> product (List.map value_typer l) r);

  }

and value_typer json : config =
  visit value_visitor json


(** {2 Entry point} *)
(** *************** *)

let typer json : config =
  domain_typer json
