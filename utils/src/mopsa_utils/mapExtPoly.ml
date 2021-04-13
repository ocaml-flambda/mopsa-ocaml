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

(** Maps with polymorphic keys and values *)

type ('k,'a) map =
    Empty
  | Node of ('k,'a) map * 'k * 'a * ('k,'a) map * int

type 'k compare = 'k -> 'k -> int

type ('k,'a) t = {
  map: ('k,'a) map;
  compare: 'k compare;
}


(** {2 Internal functions with compare parameter} *)
(** ********************************************* *)

let height_ = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create_ l x d r =
  let hl = height_ l and hr = height_ r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let singleton_ x d = Node(Empty, x, d, Empty, 1)

let bal_ l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Mapext.bal"
    | Node(ll, lv, ld, lr, _) ->
      if height_ ll >= height_ lr then
        create_ ll lv ld (create_ lr x d r)
      else begin
        match lr with
          Empty -> invalid_arg "Mapext.bal"
        | Node(lrl, lrv, lrd, lrr, _)->
          create_ (create_ ll lv ld lrl) lrv lrd (create_ lrr x d r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Mapext.bal"
    | Node(rl, rv, rd, rr, _) ->
      if height_ rr >= height_ rl then
        create_ (create_ l x d rl) rv rd rr
      else begin
        match rl with
          Empty -> invalid_arg "Mapext.bal"
        | Node(rll, rlv, rld, rlr, _) ->
          create_ (create_ l x d rll) rlv rld (create_ rlr rv rd rr)
      end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let empty_ = Empty

let is_empty_ = function Empty -> true | _ -> false

let rec add_ compare x data m = match m with
    Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare x v in
    if c = 0 then
      if d == data then m else Node(l, x, data, r, h)
    else if c < 0 then
      let ll = add_ compare x data l in
      if l == ll then m else bal_ ll v d r
    else
      let rr = add_ compare x data r in
      if r == rr then m else bal_ l v d rr

let rec find_ compare x = function
    Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = compare x v in
    if c = 0 then d
    else find_ compare x (if c < 0 then l else r)

let rec find_opt_ compare x = function
    Empty ->
    None
  | Node(l, v, d, r, _) ->
    let c = compare x v in
    if c = 0 then Some d
    else find_opt_ compare x (if c < 0 then l else r)


let rec mem_ compare x = function
    Empty ->
    false
  | Node(l, v, d, r, _) ->
    let c = compare x v in
    c = 0 || mem_ compare x (if c < 0 then l else r)

let rec min_binding_ = function
    Empty -> raise Not_found
  | Node(Empty, x, d, r, _) -> (x, d)
  | Node(l, x, d, r, _) -> min_binding_ l

let rec max_binding_ = function
    Empty -> raise Not_found
  | Node(l, x, d, Empty, _) -> (x, d)
  | Node(l, x, d, r, _) -> max_binding_ r

let rec remove_min_binding_ = function
    Empty -> invalid_arg "Mapext.remove_min_elt"
  | Node(Empty, x, d, r, _) -> r
  | Node(l, x, d, r, _) -> bal_ (remove_min_binding_ l) x d r

let merge_ t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_ t2 in
    bal_ t1 x d (remove_min_binding_ t2)

let rec remove_ compare x m = match m with
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let c = compare x v in
    if c = 0 then
      merge_ l r
    else if c < 0 then
      let ll = remove_ compare x l in if l == ll then m else bal_ ll v d r
    else
      let rr = remove_ compare x r in if r == rr then m else bal_ l v d rr

let rec iter_ f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
    iter_ f l; f v d; iter_ f r

let rec map_ f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = map_ f l in
    let d' = f d in
    let r' = map_ f r in
    Node(l', v, d', r', h)

let rec mapi_ f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = mapi_ f l in
    let d' = f v d in
    let r' = mapi_ f r in
    Node(l', v, d', r', h)

let rec fold_ f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
    fold_ f r (f v d (fold_ f l accu))

let rec for_all_ p = function
    Empty -> true
  | Node(l, v, d, r, _) -> for_all_ p l && p v d && for_all_ p r

let rec exists_ p = function
    Empty -> false
  | Node(l, v, d, r, _) -> exists_ p l || p v d || exists_ p r

let filter_ compare p s =
  fold_ (fun k d a -> if p k d then add_ compare k d a else a) s Empty

let partition_ compare p s =
  let rec part (t, f as accu) = function
    | Empty -> accu
    | Node(l, v, d, r, _) ->
      part (part (if p v d then (add_ compare v d t, f) else (t, add_ compare v d f)) l) r in
  part (Empty, Empty) s

let rec join_ compare l v d r =
  match (l, r) with
    (Empty, _) -> add_ compare v d r
  | (_, Empty) -> add_ compare v d l
  | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
    if lh > rh + 2 then bal_ ll lv ld (join_ compare lr v d r) else
    if rh > lh + 2 then bal_ (join_ compare l v d rl) rv rd rr else
      create_ l v d r
let concat_ compare t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_ t2 in
    join_ compare t1 x d (remove_min_binding_ t2)

let concat_or_join_ compare t1 v d t2 =
  match d with
  | Some d -> join_ compare t1 v d t2
  | None -> concat_ compare t1 t2


let of_list_ compare l =
  List.fold_left (fun acc (k,x) -> add_ compare k x acc) empty_ l

let rec cut_ compare k = function
    Empty -> Empty,None,Empty
  | Node (l1,k1,d1,r1,h1) ->
    let c = compare k k1 in
    if c < 0 then
      let l2,d2,r2 = cut_ compare k l1 in (l2,d2,Node (r2,k1,d1,r1,h1))
    else if c > 0 then
      let l2,d2,r2 = cut_ compare k r1 in (Node (l1,k1,d1,l2,h1),d2,r2)
    else (l1,Some d1,r1)

let rec for_all2zo_ compare f1 f2 f m1 m2 =
  (m1 == m2) ||
  (match m1 with
   | Empty -> for_all_ f2 m2
   | Node (l1,k,d1,r1,h1) ->
     let l2, d2, r2 = cut_ compare k m2 in
     (for_all2zo_ compare f1 f2 f l1 l2) &&
     (match d2 with None -> f1 k d1 | Some d2 -> d1 == d2 || f k d1 d2) &&
     (for_all2zo_ compare f1 f2 f r1 r2)
  )

let rec map2zo_ compare f1 f2 f m1 m2 =
  if m1 == m2 then m1 else
    match m1 with
    | Empty -> mapi_ f2 m2
    | Node (l1,k,d1,r1,h1) ->
      let l2, d2, r2 = cut_ compare k m2 in
      let l = map2zo_ compare f1 f2 f l1 l2 in
      let d = match d2 with
        | None -> f1 k d1
        | Some d2 -> if d1 == d2 then d1 else f k d1 d2
      in
      let r = map2zo_ compare f1 f2 f r1 r2 in
      join_ compare l k d r

let rec split_ compare x = function
    Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = compare x v in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split_ compare x l in (ll, pres, join_ compare rl v d r)
    else
      let (lr, pres, rr) = split_ compare x r in (join_ compare l v d lr, pres, rr)

let rec merge_ compare f s1 s2 =
  match (s1, s2) with
    (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height_ s2 ->
    let (l2, d2, r2) = split_ compare v1 s2 in
    concat_or_join_ compare (merge_ compare f l1 l2) v1 (f v1 (Some d1) d2) (merge_ compare f r1 r2)
  | (_, Node (l2, v2, d2, r2, h2)) ->
    let (l1, d1, r1) = split_ compare v2 s1 in
    concat_or_join_ compare (merge_ compare f l1 l2) v2 (f v2 d1 (Some d2)) (merge_ compare f r1 r2)
  | _ ->
    assert false

let rec bindings_aux_ accu = function
    Empty -> accu
  | Node(l, v, d, r, _) -> bindings_aux_ ((v, d) :: bindings_aux_ accu r) l

let bindings_ s =
  bindings_aux_ [] s

let rec cardinal_ = function
    Empty -> 0
  | Node(l, _, _, r, _) -> cardinal_ l + 1 + cardinal_ r

let rec fold2zo_ compare f1 f2 f m1 m2 acc =
  if m1 == m2 then acc else
    match m1 with
    | Empty -> fold_ f2 m2 acc
    | Node (l1,k,d1,r1,h1) ->
      let l2, d2, r2 = cut_ compare k m2 in
      let acc = fold2zo_ compare f1 f2 f l1 l2 acc in
      let acc = match d2 with
        | None -> f1 k d1 acc
        | Some d2 -> if d1 == d2 then acc else f k d1 d2 acc
      in
      fold2zo_ compare f1 f2 f r1 r2 acc

let rec fold2o_ compare f1 f2 f m1 m2 acc =
  match m1 with
  | Empty -> fold_ f2 m2 acc
  | Node (l1,k,d1,r1,h1) ->
    let l2, d2, r2 = cut_ compare k m2 in
    let acc = fold2o_ compare f1 f2 f l1 l2 acc in
    let acc = match d2 with
      | None -> f1 k d1 acc | Some d2 -> f k d1 d2 acc
    in
    fold2o_ compare f1 f2 f r1 r2 acc

let rec iter_ f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
    iter_ f l; f v d; iter_ f r

let rec iter2zo_ compare f1 f2 f m1 m2 =
  if m1 == m2 then () else
    match m1 with
    | Empty -> iter_ f2 m2
    | Node (l1,k,d1,r1,h1) ->
      let l2, d2, r2 = cut_ compare k m2 in
      iter2zo_ compare f1 f2 f l1 l2;
      (match d2 with
       | None -> f1 k d1
       | Some d2 -> if d1 != d2 then f k d1 d2);
      iter2zo_ compare f1 f2 f r1 r2

let compare_ compare cmp m1 m2 =
  let r = ref 0 in
  try
    iter2zo_ compare
      (fun _ _ -> r :=  1; raise Exit)
      (fun _ _ -> r := -1; raise Exit)
      (fun _ x y -> r := cmp x y; if !r <> 0 then raise Exit)
      m1 m2;
    !r
  with Exit -> !r

(** {2 Exported functions} *)
(** ********************** *)

let singleton ~compare x d = { map = singleton_ x d; compare }

let empty ~compare = { map = empty_; compare }

let is_empty m = is_empty_ m.map

let add x data m =
  let map = add_ m.compare x data m.map in
  if m.map == map then m else { m with map }

let find x m = find_ m.compare x m.map

let find_opt x m = find_opt_ m.compare x m.map

let mem x m = mem_ m.compare x m.map

let min_binding m = min_binding_ m.map

let remove_min_binding m = { m with map = remove_min_binding_ m.map }

let remove x m =
  let map = remove_ m.compare x m.map in
  if map == m.map then m else { m with map }

let max_binding m = max_binding_ m.map

let iter f m = iter_ f m.map

let map f m = { m with map = map_ f m.map }

let mapi f m = { m with map = mapi_ f m.map }

let fold f m accu = fold_ f m.map accu

let for_all p m = for_all_ p m.map

let exists p m = exists_ p m.map

let filter p s =
  let map = filter_ s.compare p s.map in
  if map == s.map then s else { s with map }

let partition p s =
  let map1,map2 = partition_ s.compare p s.map in
  (if map1 == s.map then s else { s with map = map1 }),
  (if map2 == s.map then s else { s with map = map2 })

let for_all2zo f1 f2 f m1 m2 =
  for_all2zo_ m1.compare f1 f2 f m1.map m2.map

let map2zo f1 f2 f m1 m2 =
  let map = map2zo_ m1.compare f1 f2 f m1.map m2.map in
  if map == m1.map then m1
  else if map == m2.map then m2
  else { map; compare = m1.compare }

let merge f m1 m2 =
  let map = merge_ m1.compare f m1.map m2.map in
  { map; compare = m1.compare }

let bindings s = bindings_ s.map

let cardinal s = cardinal_ s.map

let fold2zo f1 f2 f m1 m2 acc =
  fold2zo_ m1.compare f1 f2 f m1.map m2.map acc

let fold2o f1 f2 f m1 m2 acc =
  fold2o_ m1.compare f1 f2 f m1.map m2.map acc

let compare cmp m1 m2 = compare_ m1.compare cmp m1.map m2.map

let of_list compare l = {map = of_list_ compare l; compare}
