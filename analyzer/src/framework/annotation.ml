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

(** Annotations are used to add extra-information to flows.
    They are implemented as polymorphic maps so that annotations can be defined
    on top of the global abstraction.
*)

type ('a, _) key = ..

type (_, _) eq = Eq : ('b, 'b) eq


(* Descriptor of a stateful annotation entry *)
type ('a, 'b) sf = {
  eq : 'c. ('a, 'c) key -> ('b, 'c) eq option;
  print : Format.formatter -> 'b -> unit;
}


(* Descriptor of a stateless annotation entry *)
type 'b sl = {
  eq : 'a 'c. ('a, 'c) key -> ('b, 'c) eq option;
  print : Format.formatter -> 'b -> unit;
}


(* Descriptors *)
type ('a, 'b) descr =
  | D_stateful of ('a, 'b) sf
  | D_stateless of 'b sl


type 'a vl =
  | []   : 'a vl
  | (::) : (('a, 'b) descr * 'b) * 'a vl -> 'a vl

type 'a sfl =
  | [] : 'a sfl
  | (::) : (('a, 'b) sf) * 'a sfl -> 'a sfl

type sll =
  | [] : sll
  | (::) : 'b sl * sll -> sll

type 'a annot = {
  values: 'a vl;
  stateful_descriptors  : 'a sfl;
}

let stateless_descriptors : sll ref = ref []

let cardinal m =
  let rec aux : type a. a vl -> int =
    function
    | [] -> 0
    | _ :: tl -> 1 + aux tl
  in
  aux m.values


let register_stateful_annot (d: ('a, 'b) sf) (m: 'a annot) : 'a annot =
  {m with stateful_descriptors = d :: m.stateful_descriptors}

let register_stateless_annot (d: 'b sl) () : unit =
  stateless_descriptors := d :: !stateless_descriptors


exception Key_not_found

let find_descriptor (k: ('a, 'b) key) (m: 'a annot) : ('a, 'b) descr =
  let rec aux1 : type b. ('a, b) key -> 'a sfl -> ('a, b) descr =
    fun k -> function
      | [] -> raise Key_not_found
      | d :: tl ->
        match d.eq k with
        | Some Eq -> D_stateful d
        | None -> aux1 k tl
  in

  let rec aux2 : type b. ('a, b) key -> sll -> ('a, b) descr =
    fun k -> function
      | [] -> raise Key_not_found
      | d :: tl ->
        match d.eq k with
        | Some Eq -> D_stateless d
        | None -> aux2 k tl
  in

  try aux1 k m.stateful_descriptors
  with Key_not_found -> aux2 k !stateless_descriptors

let empty = {
  values = [];
  stateful_descriptors = [];
}

let add : type b. ('a, b) key -> b -> 'a annot -> 'a annot =
  fun k v m ->
    let rec aux : type b. ('a, b) key -> b -> 'a vl -> 'a vl =
      fun k v -> function
        | [] -> [(find_descriptor k m, v)]
        | hd :: tl ->
          let (d, _) = hd in
          let eq = match d with D_stateful d -> d.eq | D_stateless d -> d.eq in
          match eq k with
          | Some Eq -> (d, v) :: tl
          | None -> hd :: (aux k v tl)
    in
    {m with values = aux k v m.values}

let find : type b. ('a, b) key -> 'a annot -> b =
  fun k m ->
    let rec aux : type c. ('a, c) key -> 'a vl -> c =
      fun k -> function
        | [] -> raise Not_found
        | hd :: tl ->
          let (d, v) = hd in
          let eq = match d with D_stateful d -> d.eq | D_stateless d -> d.eq in
          match eq k with
          | Some Eq -> v
          | None -> aux k tl
    in
    aux k m.values

let remove : type b. ('a, b) key -> 'a annot -> 'a annot =
  fun k m ->
    let rec aux : type b. ('a, b) key -> 'a vl -> 'a vl =
      fun k -> function
        | [] -> []
        | hd :: tl ->
          let (d, _) = hd in
          let eq = match d with D_stateful d -> d.eq | D_stateless d -> d.eq in
          match eq k with
          | Some Eq -> tl
          | None -> hd :: (aux k tl)
    in
    {m with values = aux k m.values}

let mem : type b. ('a, b) key -> 'a annot -> bool =
  fun k m ->
    let rec aux : type b. ('a, b) key -> 'a vl -> bool =
      fun k -> function
        | [] -> false
        | hd :: tl ->
          let (d, _) = hd in
          let eq = match d with D_stateful d -> d.eq | D_stateless d -> d.eq in
          match eq k with
          | Some Eq -> true
          | None -> aux k tl
    in
    aux k m.values

let print fmt m =
  let rec aux : type b. Format.formatter -> b vl -> unit =
    fun fmt vl ->
      match vl with
      | [] -> ()
      | [hd] ->
        let (d, v) = hd in
        let print = match d with D_stateful d -> d.print | D_stateless d -> d.print in
        Format.fprintf fmt "%a" print v
      | hd :: tl ->
        let (d, v) = hd in
        let print = match d with D_stateful d -> d.print | D_stateless d -> d.print in
        Format.fprintf fmt "%a@\n%a" print v aux tl
  in
  Format.fprintf fmt "(@\n  @[%a@]@\n)" aux m.values
