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

(** Abstraction of sets of partial maps. *)

open Top
open Core.Lattice

let debug fmt = Debug.debug ~channel:"framework.lattices.partial_map" fmt

module type KEY =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end


module Make
    (Key : KEY)
    (Value: LATTICE)
=
struct

  module Map = MapExt.Make(Key)

  (** [a:t] is an abstraction of a set of partial maps from [Key.t]
      to [Value.t].*)
  type t =
    | Bot
    (** empty set *)

    | Finite of Value.t Map.t
    (** [Finite m] abstracts partial maps having support included in
       the support of [m] *)

    | Top
    (** all possible partial maps *)

  let bottom : t = Bot

  let top : t = Top

  let is_bottom (a:t) : bool =
    match a with
    | Bot -> true
    | Top -> false
    | Finite m ->
      Map.cardinal m > 0 &&
      Map.exists (fun k v -> Value.is_bottom v) m

  let empty : t = Finite Map.empty (* Note: an empty map is different than an empty set of maps *)

  let subset  (a1:t) (a2:t) : bool =
    match a1, a2 with
    | Bot, _ -> true
    | _, Bot -> false
    | _, Top -> true
    | Top, _ -> false
    | Finite m1, Finite m2 ->
      Map.for_all2zo
         (fun _ v1 -> false)
         (fun _ v2 -> true)
         (fun _ v1 v2 -> Value.subset v1 v2)
         m1 m2
  (** Inclusion test. *)

  let join (a1:t) (a2:t) : t =
    match a1, a2 with
    | Bot, x | x, Bot -> x
    | Top, _ | _, Top -> Top
    | Finite m1, Finite m2 ->
      Finite (
        Map.map2zo
          (fun _ v1 -> v1)
          (fun _ v2 -> v2)
          (fun _ v1 v2 -> Value.join v1 v2)
          m1 m2
      )
  (** Join two sets of partial maps. *)

  let widen annot (a1:t) (a2:t) : t =
    match a1, a2 with
    | Bot, x | x, Bot -> x
    | Top, x | x, Top -> Top
    | Finite m1, Finite m2 ->
      Finite (
        Map.map2zo
          (fun _ v1 -> v1)
          (fun _ v2 -> v2)
          (fun _ v1 v2 -> Value.widen annot v1 v2)
          m1 m2
      )
  (** Widening (naive). *)

  let meet (a1:t) (a2:t) : t =
    match a1, a2 with
    | Bot, x | x, Bot -> Bot
    | Top, x | x, Top -> x
    | Finite m1, Finite m2 ->
      Finite (
        Map.merge (fun _ v1 v2 ->
            match v1, v2 with
            | None, _ | _, None -> None
            | Some vv1, Some vv2 -> Some (Value.meet vv1 vv2)
          ) m1 m2
      )
  (** Meet. *)


  let print fmt (a:t) =
    match a with
    | Bot -> Format.pp_print_string fmt "⊥"
    | Top -> Format.pp_print_string fmt "⊤"
    | Finite m when Map.is_empty m -> Format.fprintf fmt "∅"
    | Finite m ->
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
           (fun fmt (k, v) ->
              Format.fprintf fmt "%a ⇀ @[%a@]" Key.print k Value.print v
           )
        ) (Map.bindings m)
  (** Printing. *)

  let find (k: Key.t) (a: t) : Value.t =
    match a with
    | Bot -> Value.bottom
    | Top -> Value.top
    | Finite m ->
      try Map.find k m
      with Not_found -> Exceptions.panic ~loc:__LOC__ "key %a not found" Key.print k

  let remove (k: Key.t) (a: t) : t =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m -> Finite (Map.remove k m)

  let add (k: Key.t) (v: Value.t) (a: t) : t =
    if Value.is_bottom v then Bot
    else
      match a with
      | Bot -> Bot
      | Top -> Top
      | Finite m -> Finite (Map.add k v m)

  let rename (k: Key.t) (k': Key.t) (a: t) : t =
    let v = find k a in
    let a = remove k a in
    add k' v a

  let singleton (k:Key.t) (v:Value.t) : t =
    add k v empty

  let filter (f : Key.t -> Value.t -> bool) (a : t) : t =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m -> Finite (Map.filter f m)

  let iter (f:Key.t -> Value.t -> unit) (a: t) : unit =
    match a with
    | Bot -> ()
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.iter f m

  let fold (f:Key.t -> Value.t -> 'a -> 'a) (a:t) (x:'a) : 'a =
    match a with
    | Bot -> x
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.fold f m x

  let fold_d (f:Key.t -> Value.t -> 'a -> 'a) (a:t) (d :'a) (x :'a) : 'a =
    match a with
    | Bot -> x
    | Top -> d
    | Finite m -> Map.fold f m x

  let mem (x:Key.t) (a:t) : bool =
    match a with
    | Bot -> false
    | Top -> true
    | Finite m -> Map.mem x m

  let canonize (a:t) : t =
    if is_bottom a then Bot else a

  let map (f:Value.t -> Value.t) (a:t) : t =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m ->
      Finite (Map.map f m) |>
      canonize

  let map_p (f:Key.t * Value.t -> Key.t * Value.t) (a:t) : t  =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m ->
      Finite (Map.fold (fun k v acc ->
          let k',v' = f (k,v) in
          Map.add k' v' acc
        ) m Map.empty)
      |>
      canonize

  let bindings (a:t) : (Key.t * Value.t) list =
    match a with
    | Bot -> []
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.bindings m

  let for_all (f:Key.t -> Value.t -> bool) (a:t) : bool =
    match a with
    | Bot -> true
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.for_all f m

  let exists (f:Key.t -> Value.t -> bool) (a:t) : bool =
    match a with
    | Bot -> false
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.exists f m

  let max_binding (a:t) : (Key.t * Value.t) option =
    match a with
    | Bot -> None
    | Top -> None
    | Finite m -> Some (Map.max_binding m)

  let cardinal (a: t) : int =
    match a with
    | Bot -> 0
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.cardinal m
end
