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

(** Lattice of partial maps.

    Sets of partial maps M ∈ ℘(𝕂 ⇀ 𝕍) from concrete keys set 𝕂 to
    concrete values set 𝕍 are abstracted as a partial map ℳ ∈ (𝕂⇀𝒱)
    that binds concrete keys to abstract values.
*)

open Bot_top
open Core.Lattice


let debug fmt = Debug.debug ~channel:"framework.lattices.partial_map" fmt

module PMap = MapExtPoly


type ('k,'v) map = ('k,'v) PMap.t with_bot_top


module type KEY =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end


module Make
    (Key   : KEY)
    (Value : LATTICE)
=
struct


  (** Abstraction of a set of partial maps from [Key.t] to ['a].*)
  type t = (Key.t, Value.t) map

  let bottom : t = BOT

  let top : t = TOP

  let is_bottom (a:t) : bool =
    match a with
    | BOT -> true
    | TOP -> false
    | Nbt m -> false

  let empty : t = Nbt (PMap.empty ~compare:Key.compare)

  let subset (a1:t) (a2:t) : bool =
    if a1 == a2 then true else
    match a1, a2 with
    | BOT, _ -> true
    | _, BOT -> false
    | _, TOP -> true
    | TOP, _ -> false
    | Nbt m1, Nbt m2 ->
      PMap.for_all2zo
         (fun _ v1 -> false)
         (fun _ v2 -> true)
         (fun _ v1 v2 -> Value.subset v1 v2)
         m1 m2
  (** Inclusion test. *)

  let join (a1:t) (a2:t) : t =
    if a1 == a2 then a1 else
    match a1, a2 with
    | BOT, x | x, BOT -> x
    | TOP, _ | _, TOP -> TOP
    | Nbt m1, Nbt m2 ->
      Nbt (
        PMap.map2zo
          (fun _ v1 -> v1)
          (fun _ v2 -> v2)
          (fun _ v1 v2 -> Value.join v1 v2)
          m1 m2
      )
  (** Join two sets of partial maps. *)

  let widen ctx (a1:t) (a2:t) : t =
    if a1 == a2 then a1 else
    match a1, a2 with
    | BOT, x | x, BOT -> x
    | TOP, x | x, TOP -> TOP
    | Nbt m1, Nbt m2 ->
      Nbt (
        PMap.map2zo
          (fun _ v1 -> v1)
          (fun _ v2 -> v2)
          (fun _ v1 v2 -> Value.widen ctx v1 v2)
          m1 m2
      )
  (** Widening (naive). *)

  let meet (a1:t) (a2:t) : t =
    if a1 == a2 then a1 else
    match a1, a2 with
    | BOT, x | x, BOT -> BOT
    | TOP, x | x, TOP -> x
    | Nbt m1, Nbt m2 ->
      try
        Nbt (
          PMap.merge (fun _ v1 v2 ->
              match v1, v2 with
              | None, _ | _, None -> None
              | Some vv1, Some vv2 ->
                let vv = Value.meet vv1 vv2 in
                if Value.is_bottom vv then raise Bot.Found_BOT
                else Some vv
            ) m1 m2
        )
      with Bot.Found_BOT -> bottom
  (** Meet. *)


  let print fmt (a:t) : unit =
    match a with
    | BOT -> Format.pp_print_string fmt "⊥"
    | TOP -> Format.pp_print_string fmt "⊤"
    | Nbt m when PMap.is_empty m -> Format.fprintf fmt "∅"
    | Nbt m ->
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
           (fun fmt (k, v) ->
              Format.fprintf fmt "%a ⇀ @[%a@]" Key.print k Value.print v
           )
        ) (PMap.bindings m)
  (** Printing. *)

  let find (k: Key.t) (a:t) : 'a =
    match a with
    | BOT -> Value.bottom
    | TOP -> Value.top
    | Nbt m -> PMap.find k m


  let find_opt (k: Key.t) (a:t) : 'a option =
    match a with
    | BOT -> Some Value.bottom
    | TOP -> Some Value.top
    | Nbt m -> PMap.find_opt k m

  let remove (k: Key.t) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m -> Nbt (PMap.remove k m)

  let add (k: Key.t) (v:'a) (a:t) : t =
    if Value.is_bottom v then BOT
    else
      match a with
      | BOT -> BOT
      | TOP -> TOP
      | Nbt m -> Nbt (PMap.add k v m)

  let rename (k: Key.t) (k': Key.t) (a:t) : t =
    let v = find k a in
    let a = remove k a in
    add k' v a

  let singleton (k:Key.t) (v:'a) : t =
    add k v empty

  let filter (f : Key.t -> 'a -> bool) (a :t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m -> Nbt (PMap.filter f m)

  let iter (f:Key.t -> 'a -> unit) (a:t) : unit =
    match a with
    | BOT -> ()
    | TOP -> raise Top.Found_TOP
    | Nbt m -> PMap.iter f m

  let fold (f:Key.t -> 'a -> 'b -> 'b) (a:t) (x:'b) : 'b =
    match a with
    | BOT -> x
    | TOP -> raise Top.Found_TOP
    | Nbt m -> PMap.fold f m x

  let fold2zo f1 f2 f a b acc =
    match a, b with
    | BOT, _ | _, BOT -> acc
    | TOP, _ | _, TOP -> raise Top.Found_TOP
    | Nbt m1, Nbt m2 -> PMap.fold2zo f1 f2 f m1 m2 acc

  let mem (x:Key.t) (a:t) : bool =
    match a with
    | BOT -> false
    | TOP -> true
    | Nbt m -> PMap.mem x m

  let canonize (a:t) : t =
    if is_bottom a then BOT else a

  let map (f:Value.t -> Value.t) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      Nbt (PMap.map f m) |>
      canonize

  let mapi (f:Key.t -> Value.t -> Value.t) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      Nbt (PMap.mapi f m) |>
      canonize

  let map_p (f:Key.t * Value.t -> Key.t * 'a) (a:t) : (Key.t, 'a) map =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      Nbt (PMap.fold (fun k v acc ->
          let k',v' = f (k,v) in
          PMap.add k' v' acc
        ) m (PMap.empty ~compare:Key.compare))
      |>
      canonize


  let bindings (a:t) : (Key.t * 'a) list =
    match a with
    | BOT -> []
    | TOP -> raise Top.Found_TOP
    | Nbt m -> PMap.bindings m

  let for_all (f:Key.t -> 'a -> bool) (a:t) : bool =
    match a with
    | BOT -> true
    | TOP -> raise Top.Found_TOP
    | Nbt m -> PMap.for_all f m

  let exists (f:Key.t -> 'a -> bool) (a:t) : bool =
    match a with
    | BOT -> false
    | TOP -> raise Top.Found_TOP
    | Nbt m -> PMap.exists f m

  let max_binding (a:t) : (Key.t * 'a) option =
    match a with
    | BOT -> None
    | TOP -> None
    | Nbt m -> Some (PMap.max_binding m)

  let cardinal (a:t) : int =
    match a with
    | BOT -> 0
    | TOP -> raise Top.Found_TOP
    | Nbt m -> PMap.cardinal m

end
