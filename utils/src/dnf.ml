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


let rec mk_and
    ?(fand=(@))
    ?(compare=Pervasives.compare)
    (a: 'a t) (b: 'a t) : 'a t
  =
   List.fold_left (fun acc conj1 ->
      List.fold_left (fun acc conj2 ->
          let conj = fand conj1 conj2 in
          let conj' = List.sort_uniq compare conj in
          mk_or acc [conj']
        ) acc b
    ) mk_false a


and mk_or
    ?(compare=Pervasives.compare)
    (a: 'a t) (b: 'a t) : 'a t
  =
  a @ b |>
  List.sort_uniq (Compare.list compare)


and mk_neg neg ?(compare=Pervasives.compare) (a: 'a t) : 'a t =
  a |> List.fold_left (fun acc conj ->
      mk_and ~compare acc (
        conj |>
        List.fold_left (fun acc x ->
            mk_or ~compare acc (neg x)
          ) []
      )
    ) [[]]


let map
    (f: 'a -> 'b)
    (dnf: 'a t)
  : 'b t =
  List.map (List.map f) dnf


let iter
    (f:'a -> unit)
    (dnf:'a t)
  : unit
  =
  List.iter (List.iter f) dnf


let apply
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


let apply_list
    (f: 'a -> 'b)
    (join: 'c list -> 'd)
    (meet: 'b list -> 'c)
    (dnf: 'a t)
  : 'd =
  join (
    List.map (fun c ->
        meet (List.map f c)
      ) dnf
  )



let bind
    (f: 'a -> 'b t)
    (dnf: 'a t) : 'b t =
  apply f mk_or mk_and dnf


let fold
    (f: 'b -> 'a -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (init: 'b)
    (dnf: 'a t)
  : 'b =
  let rec apply_conj acc = function
    | [] -> acc
    | [e] -> f acc e
    | e :: tl ->
      let acc1 = f acc e in
      let acc2 = apply_conj acc1 tl in
      meet acc1 acc2
  in
  let rec apply_disj acc = function
    | [] -> acc
    | [conj] -> apply_conj acc conj
    | conj :: tl ->
      let acc1 = apply_conj acc conj in
      let acc2 = apply_disj acc1 tl in
      join acc1 acc2
  in
  apply_disj init dnf


let fold_apply
    (f: 'b -> 'a -> 'b * 'c)
    (join: 'c -> 'c -> 'c)
    (meet: 'c -> 'c -> 'c)
    (init: 'b)
    (dnf: 'a t)
  : 'b * 'c =
  let rec apply_conj acc = function
    | [] -> assert false
    | [e] -> f acc e
    | e :: tl ->
      let (acc1, r1) = f acc e in
      let (acc2, r2) = apply_conj acc1 tl in
      acc2, meet r1 r2
  in
  let rec apply_disj acc = function
    | [] -> assert false
    | [conj] -> apply_conj acc conj
    | conj :: tl ->
      let (acc1,r1) = apply_conj acc conj in
      let (acc2,r2) = apply_disj acc1 tl in
      acc2, join r1 r2
  in
  apply_disj init dnf


let choose (dnf: 'a t) : 'a option =
  match dnf with
  | [] | [[]] -> None
  | (hd :: _) :: _ -> Some hd
  | _ -> assert false


let to_list (dnf: 'a t) : 'a list list = dnf


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


let merge
    (f: 'a -> 'b -> 'a t option * 'b t option)
    (dnf1:'a t) (dnf2:'b t)
  : 'a t option * 'b t option =
  List.fold_left (fun (acc1,acc2) conj1 ->
      List.fold_left (fun (acc1,acc2) conj2 ->
          let conj1', conj2' =
            List.fold_left (fun (acc1,acc2) a ->
                List.fold_left (fun (acc1,acc2) b ->
                    let a', b' = f a b in
                    Option.neutral2 mk_and a' acc1,
                    Option.neutral2 mk_and b' acc2
                  ) (acc1,acc2) conj2
              ) (None, None) conj1
          in
          Option.neutral2 mk_or conj1' acc1,
          Option.neutral2 mk_or conj2' acc2
        ) (acc1,acc2) dnf2
    ) (None, None) dnf1


let to_cnf (dnf:'a t) : 'a list list =
  let rec aux : 'a t -> 'a list list = function
    | [] -> [[]]
    | conj :: tl ->
      aux tl |>
      List.fold_left (fun acc tl ->
          List.fold_left (fun acc e ->
              (e :: tl) :: acc
            ) acc conj
        ) []
  in
  aux dnf
