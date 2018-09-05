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

type ('a, 'b) w = {
  eq : 'c. ('a, 'c) key -> ('b, 'c) eq option;
}

type 'a vl =
  | []   : 'a vl
  | (::) : (('a, 'b) w * 'b) * 'a vl -> 'a vl

type 'a wl =
  | [] : 'a wl
  | (::) : (('a, 'b) w) * 'a wl -> 'a wl

type 'a annot = {
  values: 'a vl;
  witnesses  : 'a wl;
}

let cardinal m =
  let rec aux : type a. a vl -> int =
    function
    | [] -> 0
    | _ :: tl -> 1 + aux tl
  in
  aux m.values


let register_annot (w: ('a, 'b) w) (m: 'a annot) : 'a annot =
  {m with witnesses = w :: m.witnesses}

exception Key_not_found

let find_witness (k: ('a, 'b) key) (m: 'a annot) : ('a, 'b) w =
  let rec aux : type b. ('a, b) key -> 'a wl -> ('a, b) w =
    fun k -> function
      | [] -> raise Key_not_found
      | w :: tl ->
        match w.eq k with
        | Some Eq -> w
        | None -> aux k tl
  in
  aux k m.witnesses

let empty = {
  values = [];
  witnesses = [];
}

let add : type b. ('a, b) key -> b -> 'a annot -> 'a annot =
  fun k v m ->
    let rec aux : type b. ('a, b) key -> b -> 'a vl -> 'a vl =
      fun k v -> function
        | [] -> [(find_witness k m, v)]
        | hd :: tl ->
          let (w, _) = hd in
          match w.eq k with
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
          match w.eq k with
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
          match w.eq k with
          | Some Eq -> tl
          | None -> hd :: (aux k tl)
    in
    {m with values = aux k m.values}
