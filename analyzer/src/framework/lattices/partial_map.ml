(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Map abstraction assuming ⊥ values for non-existing keys. *)

open Top
open Lattice

let debug fmt = Debug.debug ~channel:"framework.lattices.partial_map" fmt

module type KEY =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end


module Make
    (Key : KEY)
    (Value: Lattice.LATTICE)
=
struct
  module Map = MapExt.Make(Key)

  (** Abstraction of a set of partial maps. [Bot] represents the
       empty set, [Finite m] represents a (possibly infinite) set of
       partial maps in Key -> Value with finite support, and [Top] is
       the set of all partial maps in Key -> Value.  *)
  type t =
    | Bot
    | Finite of Value.t Map.t
    | Top

  let bottom = Bot

  let top = Top

  let is_bottom a = (a = Bot)

  let empty = Finite Map.empty

  let subset  (a1:t) (a2:t) : bool =
    match a1, a2 with
    | Bot, _ -> true
    | _, Bot -> false
    | _, Top -> true
    | Top, _ -> false
    | Finite m1, Finite m2 ->
      Map.for_all2zo
         (fun _ v1 -> Value.is_bottom v1) (* non-⊥ ⊈ ⊥ *)
         (fun _ v2 -> true)  (* ⊥ ⊆ non-⊥ *)
         (fun _ v1 v2 -> Value.subset v1 v2)
         m1 m2
  (** Inclusion testing. Missing variables in one map are assimilated to ⊥. *)

  let join annot (a1:t) (a2:t) : t =
    match a1, a2 with
    | Bot, x | x, Bot -> x
    | Top, x | x, Top -> Top
    | Finite m1, Finite m2 ->
      Finite (
        Map.map2zo
          (fun _ v1 -> v1)
          (fun _ v2 -> v2)
          (fun _ v1 v2 -> Value.join annot v1 v2)
          m1 m2
      )
  (** Join. Missing variables in one map are assimilated to ⊥. *)

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

  let meet annot (a1:t) (a2:t) : t =
    match a1, a2 with
    | Bot, x | x, Bot -> Bot
    | Top, x | x, Top -> x
    | Finite m1, Finite m2 ->
      Finite
        (Map.map2zo
           (fun _ v1 -> Value.bottom)
           (fun _ v2 -> Value.bottom)
           (fun _ v1 v2 -> Value.meet annot v1 v2)
           m1 m2
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
              Format.fprintf fmt "%a ⇀ @[<h2> %a@]" Key.print k Value.print v
           )
        ) (Map.bindings m)
  (** Printing. *)

  let find (k: Key.t) (a: t) =
    match a with
    | Bot -> Value.bottom
    | Top -> Value.top
    | Finite m ->
      try Map.find k m
      with Not_found -> Value.bottom

  let remove (k: Key.t) (a: t) : t =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m -> Finite (Map.remove k m)

  let add (k: Key.t) (v: Value.t) (a: t) =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m -> Finite (Map.add k v m)

  let singleton k v =
    add k v empty

  let filter (f : Key.t -> Value.t -> bool) (a : t) =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m -> Finite (Map.filter f m)
  
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

  let mem (x:Key.t) (a:t) =
    match a with
    | Bot -> false
    | Top -> true
    | Finite m -> Map.mem x m

  let map (f:Value.t -> Value.t) (a:t) : t =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m -> Finite (Map.map f m)

  let map_p (f:Key.t * Value.t -> Key.t * Value.t) (a:t) : t  =
    match a with
    | Bot -> Bot
    | Top -> Top
    | Finite m ->
      Finite (Map.fold (fun k v acc ->
          let k',v' = f (k,v) in
          Map.add k' v' acc
        ) m Map.empty)

  let bindings a =
    match a with
    | Bot -> []
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.bindings m

  let for_all f a =
    match a with
    | Bot -> true
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.for_all f m

  let exists f a =
    match a with
    | Bot -> false
    | Top -> raise Top.Found_TOP
    | Finite m -> Map.exists f m

end
