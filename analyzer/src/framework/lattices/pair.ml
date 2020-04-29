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

open Core.Lattice


(** Signature of ordered types with printers *)
module type ORDER =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end


module Make(First:LATTICE)(Second:LATTICE) =
struct

  type t = First.t * Second.t

  let bottom : t = First.bottom, Second.bottom

  let top : t = First.top, Second.top

  let singleton (x:t) : t = x

  let fst ((a,_):t) : First.t = a

  let snd ((_,b):t) : Second.t = b

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

  let join ((a1,b1) as x:t) ((a2,b2):t) : t =
    if a1 == a2 && b1 == b2 then x else
    (First.join a1 a2, Second.join b1 b2)

  let meet ((a1,b1) as x:t) ((a2,b2):t) : t =
    if a1 == a2 && b1 == b2 then x else
    (First.meet a1 a2, Second.meet b1 b2)

  let widen ctx ((a1,b1) as x:t) ((a2,b2):t) : t =
    if a1 == a2 && b1 == b2 then x else
    (First.widen ctx a1 a2, Second.widen ctx b1 b2)

  let print fmt ((a,b):t) : unit =
    Format.fprintf fmt "(%a, %a)" First.print a Second.print b

end
