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


(** Lattice of partial inversible maps.

    Sets of partial maps M ∈ ℘(𝕂 ⇀ 𝕍) from concrete keys set 𝕂 to
    concrete values set 𝕍 are abstracted as a set of partial maps ℳ ∈ 
    𝕂 ⇀ (℘(𝕍) ∪ {⊤}).
*)



open Bot_top
open Top
open Core.All



(** Signature of ordered types with printers *)
module type ORDER =
sig
  type t
  val compare: t -> t -> int
  val print : Print.printer -> t -> unit
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
      values, in addition to a set of keys mapped to ⊤
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


  (** Printing. *)
  let print printer (a:t) : unit =
    match a with
    | BOT ->
      pp_string printer "⊥"

    | TOP ->
      pp_string printer "⊤"

    | Nbt m when Relation.is_empty m.relations
              && KeySet.is_empty m.top_keys ->
      pp_string printer "∅"

    | Nbt m ->
      Relation.iter_domain (fun k vs ->
          pprint printer ~path:[pkey Key.print k]
            (pbox (pp_list Value.print ~lopen:"{" ~lsep:"," ~lclose:"}") (ValueSet.elements vs))
        ) m.relations;
      KeySet.iter (fun k ->
          pp_string printer "⊤"
            ~path:[pkey Key.print k]
        ) m.top_keys


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
        let m1' = { m1 with relations = remove_relation_keys m2.top_keys m1.relations } in
        let m2' = { m2 with relations = remove_relation_keys m1.top_keys m2.relations } in
        Nbt {
          relations = Relation.union m1'.relations m2'.relations;
          top_keys = KeySet.union m1.top_keys m2.top_keys;
        }


  (** Meet. *)
  let meet (a1:t) (a2:t) : t =
    if a1 == a2 then a1 else
    match a1, a2 with
      | BOT, x | x, BOT -> BOT
      | TOP, x | x, TOP -> x
      | Nbt m1, Nbt m2 ->
        (* In addition to bindings that are part of the two relations,
           keep the bindings that are in one relation only if the key
           belongs to ⊤ keys of the other abstract element.
        *)
        try
          Nbt (
            let relations = Relation.map2zo_domain
                (fun k1 vs1 ->
                   (* Check if k1 is mapped to ⊤ in m2 *)
                   if KeySet.mem k1 m2.top_keys
                   then vs1
                   else raise Bot.Found_BOT
                )
                (fun k2 vs2 ->
                   (* Check if k2 is mapped to ⊤ in m1 *)
                   if KeySet.mem k2 m1.top_keys
                   then vs2
                   else raise Bot.Found_BOT
                )
                (fun k vs1 vs2 ->
                   let vs = ValueSet.inter vs1 vs2 in
                   if ValueSet.is_empty vs
                   then raise Bot.Found_BOT
                   else vs
                )
                m1.relations m2.relations
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
        (* Find the keys that belong only to m2, i.e. new relations. These keys will be mapped to ⊤. *)
        let instable_keys = Relation.fold2_diff
            (fun _ _ acc -> acc)
            (fun k _ acc -> KeySet.add k acc)
            m1.relations m2.relations KeySet.empty
        in
        (* Remove instable_keys from m2 relations and add them to top_keys *)
        Nbt {
          relations = remove_relation_keys instable_keys m2.relations;
          top_keys = KeySet.union m2.top_keys instable_keys
        }


  (** Find the set of values attached to a key. Raise [Not_found] of the key is not found. *)
  let find (k: Key.t) (a:t) : ValueSet.t with_top =
    match a with
    | BOT -> Nt ValueSet.empty
    | TOP -> TOP
    | Nbt m ->
      if KeySet.mem k m.top_keys then TOP else Nt (Relation.image k m.relations)


  (** Find keys attached to value [v] in [a] *)
  let find_inverse (v:Value.t) (a:t) : KeySet.t with_top =
    match a with
    | BOT -> Nt (KeySet.empty)
    | TOP -> TOP
    | Nbt m ->
      let s1 = Relation.inverse v m.relations in
      let s2 = m.top_keys in
      Nt (KeySet.union s1 s2)


  (** Remove all bindings [(k,-)] in [a] *)
  let remove (k: Key.t) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      Nbt { relations = Relation.remove_image k m.relations; top_keys = KeySet.remove k m.top_keys }


  (** Remove all bindings [(-,v)] in [a] *)
  let remove_inverse (v:Value.t) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      Nbt { m with relations = Relation.remove_inverse v m.relations }


  (** [filter f a] keeps all bindings [(k,vs)] in [a] such that [f k vs] is true *)
  let filter (f:Key.t -> ValueSet.t with_top -> bool) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      Nbt { relations = Relation.filter_domain (fun k vs -> f k (Top.Nt vs)) m.relations;
            top_keys  = KeySet.filter (fun k -> f k Top.TOP) m.top_keys }

  (** [filter_inverse f a] keeps all inverse bindings [(v,ks)] in [a] such that [f v ks] is true *)
  let filter_inverse (f:Value.t -> KeySet.t -> bool) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m -> Nbt { m with relations = Relation.filter_codomain f m.relations }

  (* val filter_inverse : (value -> KeySet.t -> bool) -> t
   * (\** [filter_inverse f a] keeps all inverse bindings [(v,ks)] in [a] such that [f v ks] is true *\) *)

  (** Add bindings [(k,vs)] to [a]. Previous bindings are overwritten. *)
  let set (k: Key.t) (vs:ValueSet.t with_top) (a:t) : t =
    match vs with
    | Nt s when ValueSet.is_empty s -> BOT
    | _ ->
      match a with
      | BOT -> BOT
      | TOP -> TOP
      | Nbt m ->
        match vs with
        | TOP -> Nbt { top_keys = KeySet.add k m.top_keys; relations = Relation.remove_image k m.relations }
        | Nt vs -> Nbt { top_keys = KeySet.remove k m.top_keys; relations = Relation.set_image k vs m.relations }


  (** [add_inverse v ks a] adds the binding [(k,{v} ∪ find k a)] to [a], where [k] ∈ [ks]. *)
  let add_inverse (v:Value.t) (ks:KeySet.t) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      (* Do not add bindings for keys k ∈ m.top_keys *)
      let ks' = KeySet.diff ks m.top_keys in
      Nbt { m with relations = Relation.add_inverse_set v ks' m.relations }


  (** Rename key [k] to [k'] *)
  let rename (k: Key.t) (k': Key.t) (a:t) : t =
    let v = find k a in
    let a = remove k a in
    set k' v a


  (** Create a map with singleton binding [(k,{v})] *)
  let singleton (k:Key.t) (v:Value.t) : t =
    Nbt {
      top_keys = KeySet.empty;
      relations = Relation.singleton k v;
    }


  (** Check whether a binding [(k,-)] exists in [a] *)
  let mem (k:Key.t) (a:t) : bool =
    match a with
    | BOT -> false
    | TOP -> true
    | Nbt m -> Relation.mem_domain k m.relations ||
               KeySet.mem k m.top_keys

  (** [fold f a init] folds function [f] over elements [(k,vs)] *)
  let fold (f:Key.t -> ValueSet.t with_top -> 'a -> 'a) (a:t) (init:'a) : 'a =
    match a with
    | BOT -> init
    | TOP -> raise Found_TOP
    | Nbt m ->
      KeySet.fold (fun k acc -> f k TOP acc) m.top_keys init |>
      Relation.fold_domain (fun k vs acc -> f k (Nt vs) acc) m.relations

  (** Replace bindings [(k,vs)] in [a] with [(k,f vs)] *)
  let map (f:ValueSet.t with_top -> ValueSet.t with_top) (a:t) : t =
    match a with
    | BOT -> BOT
    | TOP -> TOP
    | Nbt m ->
      fold (fun k vs acc ->
          set k (f vs) acc
        ) a empty


end
