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

(** Disjunctive normal form. *)

type 'a t = 'a list list


let singleton (a: 'a) : 'a t = [[a]]


let mk_true : 'a t = [[]]


let mk_false : 'a t = []

let is_true a = a = [[]]

let is_false a = a = []

let rec mk_and
    (a: 'a t) (b: 'a t) : 'a t
  =
  if a == b then a else
  if is_true a then b else
  if is_true b then a
  else
    List.fold_left
      (fun acc conj1 ->
         List.fold_left
           (fun acc conj2 ->
              let conj = conj1 @ conj2 in
              mk_or acc [conj]
           ) acc b
      ) mk_false a


and mk_or
    (a: 'a t) (b: 'a t) : 'a t
  =
  if a == b then a else
  if is_false a then b else
  if is_false b then a
  else a @ b


and mk_neg neg (a: 'a t) : 'a t =
  a |> List.fold_left (fun acc conj ->
      mk_and acc (
        conj |>
        List.fold_left (fun acc x ->
            mk_or acc (neg x)
          ) []
      )
    ) [[]]

let is_empty a = is_false a || is_true a

let map
    (f: 'a -> 'b)
    (dnf: 'a t)
  : 'b t =
  List.map (List.map f) dnf

let map_conjunction
    (f:'a list -> 'b list)
    (dnf: 'a t)
  : 'b t =
  List.map (fun conj -> f conj) dnf

let rec to_cnf (dnf:'a t) : 'a list list =
  match dnf with
  | [] -> [[]]
  | conj::tl ->
    let next = to_cnf tl in
    List.fold_left
      (fun acc x ->
         List.fold_left
           (fun acc nexti -> (x::nexti) :: acc)
           acc next
      )
      [] conj

let from_cnf (cnf:'a list list) : 'a t = to_cnf cnf

let map_disjunction
    (f: 'a list -> 'b list)
    (dnf: 'a t)
  : 'b t =
  let cnf = to_cnf dnf in
  let cnf' = List.map f cnf in
  from_cnf cnf'

let iter
    (f:'a -> unit)
    (dnf:'a t)
  : unit
  =
  List.iter (List.iter f) dnf


let reduce
    (f: 'a -> 'b)
    ~(join: 'b -> 'b -> 'b)
    ~(meet: 'b -> 'b -> 'b)
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

let fold_reduce
    (f:'a -> 'b -> 'a * 'c)
    ~(join:'c -> 'c -> 'c)
    ~(meet:'c -> 'c -> 'c)
    (init:'a)
    (dnf:'b t) : 'a * 'c =
  let rec apply_conj acc = function
    | [e] -> f acc e
    | e :: tl ->
      let acc',x = f acc e in
      let acc'',y = apply_conj acc' tl in
      acc'',meet x y
    | [] -> assert false
  in
  let rec apply_disj acc = function
    | [conj] -> apply_conj acc conj
    | conj :: tl ->
      let acc',x = apply_conj acc conj in
      let acc'',y = apply_disj acc' tl in
      acc'',join x y
    | _ -> assert false
  in
  apply_disj init dnf

let reduce_conjunction
    (f: 'a list -> 'b)
    ~(join: 'b -> 'b -> 'b)
    (dnf: 'a t)
  : 'b =
  let rec apply_disj = function
    | [conj] -> f conj
    | conj :: tl -> join (f conj) (apply_disj tl)
    | _ -> assert false
  in
  apply_disj dnf

let fold_reduce_conjunction
    (f: 'a -> 'b list -> 'a * 'c)
    ~(join: 'c -> 'c -> 'c)
    (init:'a)
    (dnf: 'b t)
  : 'a * 'c =
  let rec apply_disj acc = function
    | [conj] -> f acc conj
    | conj :: tl ->
      let acc',x = f acc conj in
      let acc'',y = apply_disj acc' tl in
      acc', join x y
    | _ -> assert false
  in
  apply_disj init dnf

let reduce_disjunction
    (f: 'a list -> 'b)
    ~(meet: 'b -> 'b -> 'b)
    (dnf: 'a t)
  : 'b =
  let cnf = to_cnf dnf in
  let rec apply_conj = function
    | [disj] -> f disj
    | disj::tl -> meet (f disj) (apply_conj tl)
    | _ -> assert false
  in
  apply_conj cnf

let fold_reduce_disjunction
    (f: 'a -> 'b list -> 'a * 'c)
    ~(meet: 'c -> 'c -> 'c)
    (init:'a)
    (dnf: 'b t)
  : 'a * 'c =
  let cnf = to_cnf dnf in
  let rec apply_conj acc = function
    | [disj] -> f acc disj
    | disj::tl ->
      let acc',x = f acc disj in
      let acc'',y = apply_conj acc' tl in
      acc', meet x y
    | _ -> assert false
  in
  apply_conj init cnf

let bind
    (f: 'a -> 'b t)
    (dnf: 'a t) : 'b t =
  reduce f ~join:mk_or ~meet:mk_and dnf

let fold_bind
    (f: 'a -> 'b -> 'a * 'c t)
    (init:'a)
    (dnf: 'b t) : 'a * 'c t =
  fold_reduce f ~join:mk_or ~meet:mk_and init dnf

let bind_conjunction
    (f: 'a list -> 'b t)
    (dnf: 'a t) : 'b t =
  reduce_conjunction f ~join:mk_or dnf

let fold_bind_conjunction
    (f: 'a -> 'b list -> 'a * 'c t)
    (init:'a)
    (dnf: 'b t) : 'a * 'c t =
  fold_reduce_conjunction f ~join:mk_or init dnf

let bind_disjunction
    (f: 'a list -> 'b t)
    (dnf: 'a t) : 'b t =
  reduce_disjunction f ~meet:mk_and dnf

let fold_bind_disjunction
    (f: 'a -> 'b list -> 'a * 'c t)
    (init:'a)
    (dnf: 'b t) : 'a * 'c t =
  fold_reduce_disjunction f ~meet:mk_and init dnf

let fold
    (f: 'b -> 'a -> 'b)
    (init: 'b)
    (dnf: 'a t)
  : 'b =
  List.fold_left (List.fold_left f) init dnf


let partition (f:'a -> bool) (a:'a t) =
  let r1,r2 =
    List.fold_left
      (fun (acc1,acc2) conj ->
         let conj1,conj2 = List.partition f conj in
         let acc1' = if conj1 = [] then acc1 else mk_or acc1 [conj1] in
         let acc2' = if conj2 = [] then acc2 else mk_or acc2 [conj2] in
         acc1',acc2'
      ) (mk_false, mk_false) a in
  if is_false r1 then None,Some a else
  if is_false r2 then Some a, None
  else Some r1, Some r2

let choose (dnf: 'a t) : 'a option =
  match dnf with
  | [] | [[]] -> None
  | (hd :: _) :: _ -> Some hd
  | _ -> assert false


let to_list (dnf: 'a t) : 'a list list = dnf


let from_list (l: 'a list list) : 'a t = l


let print pp fmt (dnf:'a t) =
  let open Format in
  let l = to_list dnf in
  fprintf fmt "@[<hv 2>%a@]"
  (pp_print_list
     ~pp_sep:(fun fmt () -> Format.fprintf fmt " ∨@;")
     (fun fmt conj ->
        match conj with
        | [] -> ()
        | [e] -> pp fmt e
        | _ ->
          fprintf fmt "(@[<hv 2>@,%a@;@])"
            (pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt " ∧@;")
               pp
            )
            conj
     )
  )
  l


let cardinal (dnf:'a t) : int =
  List.fold_left (fun acc conj ->
      List.length conj + acc
    ) 0 dnf
