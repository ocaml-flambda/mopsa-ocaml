(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Disjunctive normal form. *)

type 'a t = 'a list list

let singleton (a: 'a) : 'a t = [[a]]

let mk_true = [[]]

let mk_false = []

let rec mk_and (a: 'a t) (b: 'a t) : 'a t =
   List.fold_left (fun acc conj1 ->
      List.fold_left (fun acc conj2 ->
          let conj = conj1 @ conj2 in
          mk_or acc [conj]
        ) acc b
    ) mk_false a

and mk_or (a: 'a t) (b: 'a t) : 'a t = a @ b

and mk_neg neg (a: 'a t) : 'a t =
  a |> List.fold_left (fun acc conj ->
      mk_and acc (
        conj |>
        List.fold_left (fun acc x ->
            mk_or acc (neg x)
          ) []
      )
    ) [[]]

let map
    (f: 'a -> 'b)
    (dnf: 'a t)
  : 'b t =
  List.map (List.map f) dnf

let substitute
    (f: 'a -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (dnf: 'a t)
  : 'b =
  let rec apply_conj = function
    | [] -> assert false
    | [e] -> f e
    | e :: tl -> meet (f e) (apply_conj tl)
  in
  let rec apply_disj = function
    | [conj] -> apply_conj conj
    | conj :: tl -> join (apply_conj conj) (apply_disj tl)
    | _ -> assert false
  in
  apply_disj dnf

let substitute2
    (f: 'a -> 'b t)
    (dnf: 'a t) : 'b t =
  substitute
    f
    mk_or
    mk_and
    dnf


let collapse
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (dnf: 'a t)
  : 'b =
  let rec apply_conj = function
    | [] -> assert false
    | [e] -> e
    | e :: tl -> meet e (apply_conj tl)
  in
  let rec apply_disj = function
    | [conj] -> apply_conj conj
    | conj :: tl -> join (apply_conj conj) (apply_disj tl)
    | _ -> assert false
  in
  apply_disj dnf



let distribute (ddnf : 'a t t) : 'a t=
  collapse mk_or mk_and ddnf

let to_list dnf = dnf


