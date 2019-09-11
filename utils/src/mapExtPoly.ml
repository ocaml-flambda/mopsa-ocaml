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

type ('k,+'a) map =
    Empty
  | Node of ('k,'a) map * 'k * 'a * ('k,'a) map * int

type 'k compare = 'k -> 'k -> int

type ('k,+'a) t = {
  map: ('k,'a) map;
  compare: 'k compare;
}

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

(* [MOPSA] changed to call p in the key order *)
let rec for_all_ p = function
    Empty -> true
  | Node(l, v, d, r, _) -> for_all_ p l && p v d && for_all_ p r

(* [MOPSA] changed to call p in the key order *)
let rec exists_ p = function
    Empty -> false
  | Node(l, v, d, r, _) -> exists_ p l || p v d || exists_ p r

(* [MOPSA] changed to call p in the key order *)
let filter_ compare p s =
  fold_ (fun k d a -> if p k d then add_ compare k d a else a) s Empty

let partition_ compare p s =
  let rec part (t, f as accu) = function
    | Empty -> accu
    | Node(l, v, d, r, _) ->
      part (part (if p v d then (add_ compare v d t, f) else (t, add_ compare v d f)) l) r in
  part (Empty, Empty) s











let singleton ~compare x d = { map = singleton_ x d; compare }

let empty ~compare = { map = empty_; compare }

let is_empty m = is_empty_ m.map

let add x data m =
  let map = add_ m.compare x data m.map in
  if m.map == map then m else { m with map }

let find x m = find_ m.compare x m.map

let mem x m = mem_ m.compare x m.map

let min_binding m = min_binding_ m.map

let remove_min_binding m = { m with map = remove_min_binding_ m.map }

let merge t1 t2 : ('a,'k) t =
  { map = merge_ t1.map t2.map; compare = t1.compare }

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
  { s with map = map1 }, { s with map = map2 }
