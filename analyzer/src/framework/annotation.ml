(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Annotations are used to add extra-information to flows.
    They are implemented as polymorphic maps so that annotations can be defined
    on top of the global abstraction.
*)

type ('a, _) key = ..

type (_, _) eq = Eq : ('b, 'b) eq


(* Stateful witness *)
type ('a, 'b) sfw = {
  eq : 'c. ('a, 'c) key -> ('b, 'c) eq option;
}


(* Stateless witness *)
type 'b slw = {
  eq : 'a 'c. ('a, 'c) key -> ('b, 'c) eq option;
}


(* Witness *)
type ('a, 'b) w =
  | W_stateful of ('a, 'b) sfw
  | W_stateless of 'b slw


type 'a vl =
  | []   : 'a vl
  | (::) : (('a, 'b) w * 'b) * 'a vl -> 'a vl

type 'a sfwl =
  | [] : 'a sfwl
  | (::) : (('a, 'b) sfw) * 'a sfwl -> 'a sfwl

type slwl =
  | [] : slwl
  | (::) : 'b slw * slwl -> slwl

type 'a annot = {
  values: 'a vl;
  stateful_witnesses  : 'a sfwl;
}

let stateless_witnesses : slwl ref = ref []

let cardinal m =
  let rec aux : type a. a vl -> int =
    function
    | [] -> 0
    | _ :: tl -> 1 + aux tl
  in
  aux m.values


let register_stateful_annot (w: ('a, 'b) sfw) (m: 'a annot) : 'a annot =
  {m with stateful_witnesses = w :: m.stateful_witnesses}

let register_stateless_annot (w: 'b slw) () : unit =
  stateless_witnesses := w :: !stateless_witnesses


exception Key_not_found

let find_witness (k: ('a, 'b) key) (m: 'a annot) : ('a, 'b) w =
  let rec aux1 : type b. ('a, b) key -> 'a sfwl -> ('a, b) w =
    fun k -> function
      | [] -> raise Key_not_found
      | w :: tl ->
        match w.eq k with
        | Some Eq -> W_stateful w
        | None -> aux1 k tl
  in

  let rec aux2 : type b. ('a, b) key -> slwl -> ('a, b) w =
    fun k -> function
      | [] -> raise Key_not_found
      | w :: tl ->
        match w.eq k with
        | Some Eq -> W_stateless w
        | None -> aux2 k tl
  in

  try aux1 k m.stateful_witnesses
  with Key_not_found -> aux2 k !stateless_witnesses

let empty = {
  values = [];
  stateful_witnesses = [];
}

let add : type b. ('a, b) key -> b -> 'a annot -> 'a annot =
  fun k v m ->
    let rec aux : type b. ('a, b) key -> b -> 'a vl -> 'a vl =
      fun k v -> function
        | [] -> [(find_witness k m, v)]
        | hd :: tl ->
          let (w, _) = hd in
          let eq = match w with W_stateful w -> w.eq | W_stateless w -> w.eq in
          match eq k with
          | Some Eq -> (w, v) :: tl
          | None -> hd :: (aux k v tl)
    in
    {m with values = aux k v m.values}

let find : type b. ('a, b) key -> 'a annot -> b =
  fun k m ->
    let rec aux : type c. ('a, c) key -> 'a vl -> c =
      fun k -> function
        | [] -> raise Not_found
        | hd :: tl ->
          let (w, v) = hd in
          let eq = match w with W_stateful w -> w.eq | W_stateless w -> w.eq in
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
          let (w, _) = hd in
          let eq = match w with W_stateful w -> w.eq | W_stateless w -> w.eq in
          match eq k with
          | Some Eq -> tl
          | None -> hd :: (aux k tl)
    in
    {m with values = aux k m.values}
