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


(** Lattice of pairs *)

open Core.All


(** Signature of ordered types with printers *)
module type ORDER =
sig
  type t
  val compare: t -> t -> int
  val print : Print.printer -> t -> unit
end


module Make(First:LATTICE)(Second:LATTICE) =
struct

  type t = First.t * Second.t

  let bottom : t = First.bottom, Second.bottom

  let top : t = First.top, Second.top

  let singleton (x:t) : t = x

  let map_fst (f:(First.t -> First.t)) ((a,b) as x:t) : t =
    let a' = f a in
    if a' == a then x else (a',b)

  let map_snd (f:(Second.t -> Second.t)) ((a,b) as x:t) : t =
    let b' = f b in
    if b == b' then x else (a,b')

  let is_bottom ((a,b):t) : bool =
    First.is_bottom a || Second.is_bottom b

  let subset ((a1,b1):t) ((a2,b2):t) : bool =
    First.subset a1 a2 && Second.subset b1 b2

  let apply f1 f2 ((v1,v2) as v) =
    let r1 = f1 v1 in
    let r2 = f2 v2 in
    if r1 == v1 && r2 == v2 then v
    else (r1,r2)

  let apply2 f1 f2 ((v1,v2) as v) ((w1,w2) as w) =
    let r1 = f1 v1 w1 in
    let r2 = f2 v2 w2 in
    if r1 == v1 && r2 == v2 then v else
    if r1 == w1 && r2 == w2 then w
    else (r1,r2)

  let join ((v1,v2) as v:t) ((w1,w2) as w:t) : t =
    if v1 == w1 && v2 == w2 then v else
    apply2 First.join Second.join v w

  let meet ((v1,v2) as v:t) ((w1,w2) as w:t) : t =
    if v1 == w1 && v2 == w2 then v else
    apply2 First.meet Second.meet v w

  let widen ctx ((v1,v2) as v:t) ((w1,w2) as w:t) : t =
    if v1 == w1 && v2 == w2 then v else
    apply2 (First.widen ctx) (Second.widen ctx) v w

  let print printer ((a,b):t) : unit =
    pp_obj_tuple
      printer
      [ boxed First.print a;
        boxed Second.print b ]

end
