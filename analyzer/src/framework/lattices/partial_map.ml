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


module MakePolymorph
    (Key : KEY)
=
struct

  module Map = MapExt.Make(Key)

  (** Abstraction of a set of partial maps from [Key.t] to ['a].*)
  type +'a t =
    | Bot
    (** empty set *)

    | Finite of 'a Map.t
    (** [Finite m] abstracts partial maps having support included in
       the support of [m] *)

    | Top
    (** all possible partial maps *)

  let bottom : 'a t = Bot

  let top : 'a t = Top

  let is_bottom ~is_bottomv (a:'a t) : bool =
    match a with
    | Bot -> true
    | Top -> false
    | Finite m ->
      Map.cardinal m > 0 &&
      Map.exists (fun k v -> is_bottomv v) m

  let empty : 'a t = Finite Map.empty (* Note: an empty map is different than an empty set of maps *)

  let subset ~subsetv (a1:'a t) (a2:'a t) : bool =
    match a1, a2 with
    | Bot, _ -> true
    | _, Bot -> false
    | _, Top -> true
    | Top, _ -> false
    | Finite m1, Finite m2 ->
      Map.for_all2zo
         (fun _ v1 -> false)
         (fun _ v2 -> true)
         (fun _ v1 v2 -> subsetv v1 v2)
         m1 m2
  (** Inclusion test. *)

  let join ~joinv (a1:'a t) (a2:'a t) : 'a t =
    match a1, a2 with
    | Bot, x | x, Bot -> x
    | Top, _ | _, Top -> Top
    | Finite m1, Finite m2 ->
      Finite (
        Map.map2zo
          (fun _ v1 -> v1)
          (fun _ v2 -> v2)
          (fun _ v1 v2 -> joinv v1 v2)
          m1 m2
      )
  (** Join two sets of partial maps. *)

  let widen ctx ~widenv (a1:'a t) (a2:'a t) : 'a t =
    match a1, a2 with
    | Bot, x | x, Bot -> x
    | Top, x | x, Top -> Top
    | Finite m1, Finite m2 ->
      Finite (
        Map.map2zo
          (fun _ v1 -> v1)
          (fun _ v2 -> v2)
          (fun _ v1 v2 -> widenv ctx v1 v2)
          m1 m2
      )
  (** Widening (naive). *)

  let meet ~meetv (a1:'a t) (a2:'a t) : 'a t =
    match a1, a2 with
    | Bot, x | x, Bot -> Bot
    | Top, x | x, Top -> x
    | Finite m1, Finite m2 ->
      Finite (
        Map.merge (fun _ v1 v2 ->
            match v1, v2 with
            | None, _ | _, None -> None
            | Some vv1, Some vv2 -> Some (meetv vv1 vv2)
          ) m1 m2
      )
  (** Meet. *)


  let print fmt ~printv (a:'a t) =
    match a with
    | Bot -> Format.pp_print_string fmt "⊥"
    | Top -> Format.pp_print_string fmt "⊤"
    | Finite m when Map.is_empty m -> Format.fprintf fmt "∅"
    | Finite m ->
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
           (fun fmt (k, v) ->
              Format.fprintf fmt "%a ⇀ @[%a@]" Key.print k printv v
           )
        ) (Map.bindings m)
  (** Printing. *)

  let find ~bottomv ~topv (k: Key.t) (a:'a t) : 'a =
    match a with
    | Bot -> bottomv
    | Top -> topv
    | Finite m ->
      try Map.find k m
      with Not_found -> Exceptions.panic ~loc:__LOC__ "key %a not found" Key.print k

  let remove (k: Key.t) (a:'a t) : 'a t =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m -> Finite (Map.remove k m)

  let add (k: Key.t) ~is_bottomv (v:'a) (a:'a t) : 'a t =
    if is_bottomv v then Bot
    else
      match a with
      | Bot -> Bot
      | Top -> Top
      | Finite m -> Finite (Map.add k v m)

  let rename ~bottomv ~topv ~is_bottomv (k: Key.t) (k': Key.t) (a:'a t) : 'a t =
    let v = find ~bottomv ~topv k a in
    let a = remove k a in
    add ~is_bottomv k' v a

  let singleton ~is_bottomv (k:Key.t) (v:'a) : 'a t =
    add ~is_bottomv k v empty

  let filter (f : Key.t -> 'a -> bool) (a :'a t) : 'a t =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m -> Finite (Map.filter f m)

  let iter (f:Key.t -> 'a -> unit) (a:'a t) : unit =
    match a with
    | Bot -> ()
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.iter f m

  let fold (f:Key.t -> 'a -> 'b -> 'b) (a:'a t) (x:'b) : 'b =
    match a with
    | Bot -> x
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.fold f m x

  let fold_d (f:Key.t -> 'a -> 'b -> 'b) (a:'a t) (d :'b) (x :'b) : 'b =
    match a with
    | Bot -> x
    | Top -> d
    | Finite m -> Map.fold f m x

  let mem (x:Key.t) (a:'a t) : bool =
    match a with
    | Bot -> false
    | Top -> true
    | Finite m -> Map.mem x m

  let canonize ~is_bottomv (a:'a t) : 'a t =
    if is_bottom ~is_bottomv a then Bot else a

  let map ~is_bottomv (f:'a -> 'b) (a:'a t) : 'b t =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m ->
      Finite (Map.map f m) |>
      canonize ~is_bottomv

  let map_p ~is_bottomv (f:Key.t * 'a -> Key.t * 'b) (a:'a t) : 'b t  =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m ->
      Finite (Map.fold (fun k v acc ->
          let k',v' = f (k,v) in
          Map.add k' v' acc
        ) m Map.empty)
      |>
      canonize ~is_bottomv

  let bindings (a:'a t) : (Key.t * 'a) list =
    match a with
    | Bot -> []
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.bindings m

  let for_all (f:Key.t -> 'a -> bool) (a:'a t) : bool =
    match a with
    | Bot -> true
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.for_all f m

  let exists (f:Key.t -> 'a -> bool) (a:'a t) : bool =
    match a with
    | Bot -> false
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.exists f m

  let max_binding (a:'a t) : (Key.t * 'a) option =
    match a with
    | Bot -> None
    | Top -> None
    | Finite m -> Some (Map.max_binding m)

  let cardinal (a:'a t) : int =
    match a with
    | Bot -> 0
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.cardinal m
end





module Make
    (Key : KEY)
    (Value: LATTICE)
=
struct

  module Map = MakePolymorph(Key)

  type t = Value.t Map.t

  let bottom : t = Map.bottom

  let top : t = Map.top

  let is_bottom (a:t) : bool = Map.is_bottom a ~is_bottomv:Value.is_bottom

  let empty : t = Map.empty

  let subset (a1:t) (a2:t) : bool = Map.subset a1 a2 ~subsetv:Value.subset

  let join (a1:t) (a2:t) : t = Map.join a1 a2 ~joinv:Value.join

  let widen ctx (a1:t) (a2:t) : t = Map.widen ctx a1 a2 ~widenv:Value.widen

  let meet (a1:t) (a2:t) : t = Map.meet a1 a2 ~meetv:Value.meet

  let print fmt (a:t) = Map.print fmt a ~printv:Value.print

  let find (k: Key.t) (a: t) : Value.t = Map.find k a ~bottomv:Value.bottom ~topv:Value.top

  let remove (k: Key.t) (a: t) : t = Map.remove k a

  let add (k: Key.t) (v: Value.t) (a: t) : t = Map.add k v a ~is_bottomv:Value.is_bottom

  let rename (k: Key.t) (k': Key.t) (a: t) : t = Map.rename k k' a ~bottomv:Value.bottom ~topv:Value.top ~is_bottomv:Value.is_bottom

  let singleton (k:Key.t) (v:Value.t) : t = Map.singleton k v ~is_bottomv:Value.is_bottom

  let filter (f : Key.t -> Value.t -> bool) (a : t) : t = Map.filter f a

  let iter (f:Key.t -> Value.t -> unit) (a: t) : unit = Map.iter f a

  let fold (f:Key.t -> Value.t -> 'a -> 'a) (a:t) (x:'a) : 'a = Map.fold f a x

  let fold_d (f:Key.t -> Value.t -> 'a -> 'a) (a:t) (d :'a) (x :'a) : 'a = Map.fold_d f a d x

  let mem (x:Key.t) (a:t) : bool = Map.mem x a

  let canonize (a:t) : t = Map.canonize a ~is_bottomv:Value.is_bottom

  let map (f:Value.t -> Value.t) (a:t) : t = Map.map f a ~is_bottomv:Value.is_bottom

  let map_p (f:Key.t * Value.t -> Key.t * Value.t) (a:t) : t  = Map.map_p f a ~is_bottomv:Value.is_bottom

  let bindings (a:t) : (Key.t * Value.t) list = Map.bindings a

  let for_all (f:Key.t -> Value.t -> bool) (a:t) : bool = Map.for_all f a

  let exists (f:Key.t -> Value.t -> bool) (a:t) : bool = Map.exists f a

  let max_binding (a:t) : (Key.t * Value.t) option = Map.max_binding a

  let cardinal (a: t) : int = Map.cardinal a
end
