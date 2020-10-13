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

(** Powerset lattice with finite cardinality elements or ⊺. *)

open Mopsa_utils
open Core.All
open Top

module type ELT =
sig
  type t
  val compare: t -> t -> int
  val print : printer -> t -> unit
end


module Make(Elt: ELT) =
struct
  module Set = SetExt.Make(Elt)

  type v = Set.t

  type t = v with_top

  let bottom : t = Nt Set.empty

  let top : t = TOP

  let is_top (abs: t) = abs = TOP

  let subset (abs1:t) (abs2:t) : bool =
    top_included Set.subset abs1 abs2

  let equal (abs1:t) (abs2:t) : bool =
    top_equal Set.equal abs1 abs2

  let compare (abs1:t) (abs2:t) : int =
    match abs1, abs2 with
    | TOP, TOP -> 0
    | TOP, _ -> -1
    | _, TOP -> 1
    | Nt x1, Nt x2 -> Set.compare x1 x2

  let join (abs1:t) (abs2:t) : t =
    if abs1 == abs2 then abs1 else
    top_lift2 Set.union abs1 abs2

  let meet (abs1:t) (abs2:t) : t =
    if abs1 == abs2 then abs1 else
    top_neutral2 Set.inter abs1 abs2

  let union = join

  let inter = meet

  let diff (abs1:t) (abs2:t) : t =
    if is_top abs2
    then bottom
    else top_lift2 Set.diff abs1 abs2

  let widen ctx (abs1:t) (abs2:t) : t =
    top_absorb2
      (fun s1 s2 ->
         if Set.subset s2 s1 then
           abs2
         else
           TOP
      )
      abs1 abs2

  let print printer (abs:t) =
    match abs with
    | Top.TOP -> pp_string printer "⊤"
    | Top.Nt s ->
      if Set.is_empty s then pp_string printer "∅"
      else
        pp_list Elt.print printer (Set.elements s) ~lopen:"{" ~lsep:"," ~lclose:"}"

  let add v (abs:t) : t =
    top_lift1 (Set.add v) abs

  let fold (f:Elt.t->'a->'a) (abs:t) (init:'a) : 'a =
    top_to_exn abs |> (fun s -> Set.fold f s init)

  let remove (v:Elt.t) (abs:t) : t =
    top_lift1 (Set.remove v) abs

  let mem (v:Elt.t) (abs:t) : bool =
    top_dfl1 true (Set.mem v) abs

  let filter f (abs:t) : t =
    top_lift1 (Set.filter f) abs

  let partition f (abs:t) : t * t =
    match abs with
    | TOP -> TOP, TOP
    | Nt a ->
      let r1, r2 = Set.partition f a in
      Nt r1, Nt r2

  let exists f (abs:t) : bool =
    top_to_exn abs |> (fun s -> Set.exists f s)

  let for_all f (abs:t) : bool =
    top_to_exn abs |> (fun s -> Set.for_all f s)

  let cardinal (abs:t) : int =
    top_to_exn abs |> (fun s -> Set.cardinal s)

  let find f (abs:t) : Elt.t =
    top_to_exn abs |> (fun s -> Set.find f s)

  let choose (abs:t) : Elt.t =
    top_to_exn abs |> (fun s -> Set.choose s)

  let singleton (x:Elt.t) : t = Nt (Set.singleton x)

  let of_list (l:Elt.t list) =
    Nt (Set.of_list l)

  let is_empty (abs:t) =
    top_dfl1 false Set.is_empty abs

  let is_singleton a =
    top_apply (fun s ->
        Set.cardinal s = 1 (* FIXME: computing the cardinal for this check is inefficient *)
      ) false a

  let empty = bottom

  let is_bottom = is_empty

  let elements (abs:t) =
    top_to_exn abs |> Set.elements

  let map (f:Elt.t -> Elt.t)  (abs:t) : t =
    top_lift1 (Set.map f) abs

  let iter (f:Elt.t -> unit) (abs:t) : unit =
    top_to_exn abs |> Set.iter f

  let apply (f:Set.t -> 'a) (dfl:'a) (abs:t) : 'a = Top.top_apply f dfl abs

end
