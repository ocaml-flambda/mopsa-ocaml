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

(** Relation with printing builder *)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
    val print : Format.formatter -> t -> unit
  end


module Make(L : OrderedType)(R : OrderedType) =
struct
  exception Already_Paired
  module LR = MapExt.Make(L)
  module RL = MapExt.Make(R)
  type t =
    {
      lr : R.t LR.t ;
      rl : L.t RL.t ;
    }
  let compare (e : t) (e' : t) : int =
    try
      let r = LR.fold (fun k v acc ->
          try
            let v' = LR.find k acc in
            if R.compare v' v = 0 then
              LR.remove k acc
            else raise Exit
          with
          | Not_found -> raise Exit
        ) e.lr e'.lr
      in
      if LR.cardinal r = 0 then
        0
      else -1
    with
    | Exit -> 1

  let empty =
    {lr = LR.empty ; rl = RL.empty}

  let fold (f : L.t * R.t -> 'a -> 'a) (e : t) (a : 'a) =
    LR.fold (fun k v acc -> f (k,v) acc) e.lr a

  let iter (f : L.t * R.t -> unit) (e: t) : unit =
    fold (fun p acc -> f p) e ()

  let remove_l (l : L.t) (e : t) =
    try
      let b = LR.find l e.lr in
      {lr = LR.remove l e.lr ; rl = RL.remove b e.rl}
    with
    | Not_found -> e

  let remove_r (r : R.t) (e : t) =
    try
      let l = RL.find r e.rl in
      {lr = LR.remove l e.lr ; rl = RL.remove r e.rl}
    with
    | Not_found -> e

  let add ((l,r) : L.t * R.t) (e : t) =
    try
      let r' = LR.find l e.lr in
      if R.compare r r' = 0 then
        e
      else
        raise Already_Paired
    with
    | Not_found ->
      {lr = LR.add l r e.lr ; rl = RL.add r l e.rl}

  let mem ((l,r) : L.t * R.t) (e : t) =
    try
      let r' = LR.find l e.lr in
      R.compare r r' = 0
    with
    | Not_found -> false

  let concat (e1:t) (e2:t) =
    let patch kp vp compare k v1 v2 =
      match v1, v2 with
      | None, None -> None
      | Some vv, None | None, Some vv -> Some vv
      | Some vv1, Some vv2 ->
        if compare vv1 vv2 = 0
        then Some vv1
        else Exceptions.panic "Equiva.concat: key %a points to different values %a and %a"
            kp k vp vv1 vp vv2
    in
    {
      lr = LR.merge (patch L.print R.print R.compare) e1.lr e2.lr;
      rl = RL.merge (patch R.print L.print L.compare) e1.rl e2.rl;
    }

  let mem_l (l : L.t) (e: t) =
    LR.mem l e.lr

  let mem_r (r : R.t) (e: t) =
    RL.mem r e.rl

  let find_l l e =
    LR.find l e.lr

  let find_r l e =
    RL.find l e.rl

  let find_l_opt (l : L.t) (e : t) : R.t option =
    try
      Some (LR.find l e.lr)
    with
    | Not_found -> None

  let find_r_opt (r : R.t) (e : t) : L.t option =
    try
      Some (RL.find r e.rl)
    with
    | Not_found -> None

  let map (f : (L.t * R.t) -> (L.t * R.t)) (e : t) : t =
    fold (fun p acc -> add (f p) acc) e empty

  let filter (f: (L.t * R.t) -> bool) (e: t) : t =
    fold (fun p acc -> if f p then add p acc else acc) e empty

  let exists (f: (L.t * R.t) -> bool) (e: t) : bool =
    let exception FoundOne in
    try
      iter (fun p -> if f p then raise FoundOne) e;
      false
    with
    | FoundOne -> true

  let forall (f: (L.t * R.t) -> bool) (e: t) : bool =
    let exception FoundNot in
    try
      iter (fun p -> if not (f p) then raise FoundNot) e;
      true
    with
    | FoundNot -> false


end
