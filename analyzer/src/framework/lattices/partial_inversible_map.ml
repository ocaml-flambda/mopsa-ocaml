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

(** Lattice of partial inversible maps with heterogeneous support sets. *)


open Bot_top
open Top
open Core.Lattice



(** Signature of ordered types with printers *)
module type ORDER =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end


module Make
    (Key   : ORDER)
    (Value : ORDER)
=
struct

  (** Inversible relations between keys and values *)
  module Relation = InvRelation.Make(Key)(Value)


  (** Set of keys *)
  module KeySet = Relation.DomSet


  (** Set of values *)
  module ValueSet = Relation.CoDomSet


  (** Inversible maps are represented as a set of relations between keys and
     values and a set of keys mapped to ⊤
  *)
  type map = {
    relations: Relation.t;
    top_keys: Relation.DomSet.t
  }

  (** Type of inversible maps with ⊤ and ⊥ *)
  type t = map with_bot_top

  (** ⊥ element *)
  let bottom : t = BOT

  (** ⊤ element *)
  let top : t = TOP

  (** Check whether [a] is ⊥ *)
  let is_bottom (a:t) : bool =
    match a with
    | BOT -> true
    | TOP -> false
    | Nbt m -> false

  (** Singleton of empty map *)
  let empty : t = Nbt { relations = Relation.empty; top_keys = KeySet.empty }


  (** Remove a set of keys from a relation *)
  let remove_relation_keys (keys:KeySet.t) (rel:Relation.t) : Relation.t =
    KeySet.fold Relation.remove_image keys rel


  (** Inclusion test. *)
  let subset (a1:t) (a2:t) : bool =
    if a1 == a2 then true else
    match a1, a2 with
    | BOT, _ -> true
    | _, BOT -> false
    | _, TOP -> true
    | TOP, _ -> false
    | Nbt m1, Nbt m2 ->
      (* Remove keys of m1 that valuate to ⊤ in m2 *)
      let m1' = { m1 with relations = remove_relation_keys m2.top_keys m1.relations } in
      Relation.subset m1'.relations m2.relations &&
      KeySet.subset m1'.top_keys m2.top_keys


  (** Join two sets of partial maps. *)
  let join (a1:t) (a2:t) : t =
    if a1 == a2 then a1 else
      match a1, a2 with
      | BOT, x | x, BOT -> x
      | TOP, _ | _, TOP -> TOP
      | Nbt m1, Nbt m2 ->
        (* Remove keys that valuate to ⊤ in m1 or m2 *)
        let top_keys = KeySet.union m1.top_keys m2.top_keys in
        let m1' = { m1 with relations = remove_relation_keys top_keys m1.relations } in
        let m2' = { m2 with relations = remove_relation_keys top_keys m2.relations } in
        Nbt {
          relations = Relation.union m1'.relations m2'.relations;
          top_keys = top_keys;
        }

  (** Meet. *)
  let meet (a1:t) (a2:t) : t =
    if a1 == a2 then a1 else
      match a1, a2 with
      | BOT, x | x, BOT -> BOT
      | TOP, x | x, TOP -> x
      | Nbt m1, Nbt m2 ->
        (* If the key of singular binding does not belong to m1.top_keys or
           m2.top_keys, then the map becomes ⊥ *)
        try
          Nbt (
            let relations = Relation.fold2
                (fun k1 _ acc ->
                   if KeySet.mem k1 m2.top_keys then acc else raise Bot.Found_BOT
                )
                (fun k2 _ acc ->
                   if KeySet.mem k2 m1.top_keys then acc else raise Bot.Found_BOT
                )
                (fun k v acc -> Relation.add k v acc)
                m1.relations m2.relations Relation.empty
            in
            let top_keys = KeySet.inter m1.top_keys m2.top_keys in
            { relations; top_keys }
          )
        with Bot.Found_BOT -> bottom

  (** Widening operator *)
  let widen ctx (a1:t) (a2:t) : t =
    let a2 = join a1 a2 in
    if a1 == a2 then a1 else
      match a1, a2 with
      | BOT, x | x, BOT -> x
      | TOP, x | x, TOP -> TOP
      | Nbt m1, Nbt m2 ->
        (* Add to m2.top_keys the keys of the new relations *)
        let new_top_keys = Relation.fold2_diff
            (fun _ _ acc -> acc)
            (fun k _ acc -> KeySet.add k acc)
            m1.relations m2.relations KeySet.empty
        in
        (* Remove top_keys from m2 *)
        Nbt {
          relations = remove_relation_keys new_top_keys m2.relations;
          top_keys = KeySet.union m2.top_keys new_top_keys
        }


  (** Printing. *)
  let print fmt (a:t) : unit =
    match a with
    | BOT ->
      Format.pp_print_string fmt "⊥"

    | TOP ->
      Format.pp_print_string fmt "⊤"

    | Nbt m when Relation.is_empty m.relations
              && KeySet.is_empty m.top_keys ->
      Format.fprintf fmt "∅"

    | Nbt m ->
      Format.fprintf fmt "@[<v>%a@,%a@]"
        (fun fmt rel ->
           if Relation.is_empty rel then ()
           else
             Relation.iter_domain (fun k vs ->
                 Format.fprintf fmt "%a ⇀ @[%a@],@,"
                   Key.print k
                   (ValueSet.fprint SetExt.printer_default Value.print) vs
               ) rel
        ) m.relations
        (fun fmt top_keys ->
           if KeySet.is_empty top_keys then ()
           else
             KeySet.iter (fun k ->
                 Format.fprintf fmt "%a ⇀ ⊤,@," Key.print k
               ) top_keys
        ) m.top_keys


  (** Find the set of values attached to a key *)
  let find (k: Key.t) (a:t) : ValueSet.t with_top =
    match a with
    | BOT -> Nt ValueSet.empty
    | TOP -> TOP
    | Nbt m ->
      if KeySet.mem k m.top_keys then TOP else Nt (Relation.image k m.relations)


  let find_opt (k: Key.t) (a:t) : ValueSet.t with_top option =
    try Some (find k a) with Not_found -> None

  let remove (k: Key.t) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      Nbt { relations = Relation.remove_image k m.relations; top_keys = KeySet.remove k m.top_keys }

  let add_singleton (k: Key.t) (v:Value.t) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      if KeySet.mem k m.top_keys then a else Nbt { m with relations = Relation.add k v m.relations }

  let add (k: Key.t) (vs:ValueSet.t with_top) (a:t) : t =
    match vs with
    | Nt s when ValueSet.is_empty s -> BOT
    | _ ->
      match a with
      | BOT -> BOT
      | TOP -> TOP
      | Nbt m ->
        if KeySet.mem k m.top_keys then a else
        match vs with
        | TOP -> Nbt { top_keys = KeySet.add k m.top_keys; relations = Relation.remove_image k m.relations }
        | Nt vs -> Nbt { m with relations = Relation.add_image_set k vs m.relations }


  let rename (k: Key.t) (k': Key.t) (a:t) : t =
    let v = find k a in
    let a = remove k a in
    add k' v a


  let singleton (k:Key.t) (v:Value.t) : t =
    add_singleton k v empty


  let mem (k:Key.t) (a:t) : bool =
    match a with
    | BOT -> false
    | TOP -> true
    | Nbt m -> Relation.mem_domain k m.relations ||
               KeySet.mem k m.top_keys

  let fold (f:Key.t -> ValueSet.t with_top -> 'a -> 'a) (a:t) (init:'a) : 'a =
    match a with
    | BOT -> init
    | TOP -> raise Found_TOP
    | Nbt m ->
      KeySet.fold (fun k acc -> f k TOP acc) m.top_keys init |>
      Relation.fold_domain (fun k vs acc -> f k (Nt vs) acc) m.relations

  let map (f:ValueSet.t with_top -> ValueSet.t with_top) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      fold (fun k vs acc ->
          add k (f vs) acc
        ) a empty


end
