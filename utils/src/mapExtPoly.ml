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

type (+'a,'k) map =
    Empty
  | Node of ('a,'k) map * 'k * 'a * ('a,'k) map * int

type 'k compare = 'k -> 'k -> int

type (+'a,'k) t = {
  map: ('a,'k) map;
  compare: 'k compare;
}


let height_ = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h



let create_ l x d r =
  let hl = height_ l and hr = height_ r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))


let singleton_ x d = Node(Empty, x, d, Empty, 1)

let singleton ~compare x d = { map = singleton_ x d; compare }


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

let empty ~compare = { map = empty_; compare }


let is_empty_ = function Empty -> true | _ -> false

let is_empty m = is_empty_ m.map


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

let add x data m =
  let map = add_ m.compare x data m.map in
  if m.map == map then m else { m with map }


let rec find_ compare x = function
    Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = compare x v in
    if c = 0 then d
    else find_ compare x (if c < 0 then l else r)

let find x m = find_ m.compare x m.map
