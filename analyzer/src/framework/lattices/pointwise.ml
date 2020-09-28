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

(** Pointwise lattice construction.

    Lattice of partial maps 𝕂 ⇀ 𝕍 where 𝕂 is a key set 
    and 𝕍 is a value lattice.
    Unlink Partial_map, ⊥ values is not coalescent.

    Bindings that map keys to ⊥ values are not represented.    

    We use Maps, so 𝕂 needs to be totally ordered.

    ⊥ map is represented as the empty map.
    ⊤ map is represented as a special TOP element.
*)

open Top
open Core.All


let debug fmt = Debug.debug ~channel:"framework.lattices.pointwise" fmt

module type KEY =
sig
  type t
  val compare: t -> t -> int
  val print : printer -> t -> unit
end


module Make
    (Key   : KEY)
    (Value : LATTICE)
=
struct

  module M = MapExt.Make(Key)

  type nt_t = Value.t M.t

  type t = nt_t with_top

  let bottom : t = Nt M.empty

  let empty : t = bottom
  
  let top : t = TOP

  let is_bottom (a:t) : bool = 
    top_dfl1 false M.is_empty a

  let is_empty (a:t) : bool =
    is_bottom a

  let is_top (a:t) : bool =
    a = TOP
  
  let subset (a:t) (b:t) : bool =
    if a == b then true else
    top_included
      (M.for_all2zo
         (fun _ _ -> false)
         (fun _ _ -> true)
         (fun _ x y -> Value.subset x y))
      a b

  let join (a:t) (b:t) : t =
    if a == b then a else
    top_lift2
      (M.map2zo
         (fun _ x -> x)
         (fun _ x -> x)
         (fun _ x y -> Value.join x y)
      )
      a b
  
  let widen ctx (a:t) (b:t) : t =
    if a == b then a else
    top_lift2
      (M.map2zo
         (fun _ x -> x)
         (fun _ x -> x)
         (fun _ x y -> Value.widen ctx x y)
      )
      a b

  let meet (a:t) (b:t) : t =
    if a == b then a else
    top_neutral2
      (M.map2zo
         (fun _ x -> x)
         (fun _ x -> x)
         (fun _ x y -> Value.meet x y)
      )
      a b

  let print printer (a:t) : unit =
    match a with
    | TOP -> pp_string printer "⊤"
    | Nt m when M.is_empty m -> pp_string printer "⊥"
    | Nt m ->
      pp_map
        Key.print Value.print
        printer (M.bindings m)

  (** Returns ⊥ value if either k is not found, or a is the ⊤ map. *)
  let find (k: Key.t) (a:t) : Value.t =
    top_dfl1
      Value.top
      (fun m -> try M.find k m with Not_found -> Value.bottom) a

  let remove (k: Key.t) (a:t) : t =
    top_lift1 (fun m -> M.remove k m) a

  (* Internal function .
     Call this instead of M.add to ensure that no binding to the ⊥ value
     is ever added to the map.
  *)
  let m_add (k: Key.t) (v:Value.t) (a:nt_t) : nt_t =
    if Value.is_bottom v then M.remove k a else M.add k v a
    
  let add (k: Key.t) (v:Value.t) (a:t) : t =
    top_lift1 (fun m -> m_add k v m) a

  (** Returns false of a is the ⊤ map, or k is mapped to the ⊥ value. *)
  let mem (k: Key.t) (a:t) : bool =
    top_dfl1 false (M.mem k) a
  
  let rename (k: Key.t) (k': Key.t) (a:t) : t =
    let v = find k a in
    let a = remove k a in
    add k' v a

  let apply (k: Key.t) (f:Value.t -> Value.t) (a:t) : t =
    add k (f (find k a)) a
  
  let singleton (k:Key.t) (v:Value.t) : t =
    add k v bottom

  let filter (f : Key.t -> Value.t -> bool) (a :t) : t =
    top_lift1 (M.filter f) a

  
  (** Raises a Top_encountered exception for the ⊤ map. *)
  let bindings (a:t) : (Key.t * Value.t) list =
    M.bindings (detop a)

  
  (** Returns None for a ⊤ or ⊥ map. *)
  let max_binding (a:t) : (Key.t * Value.t) option =
    top_dfl1
      None
      (fun m -> try Some (M.max_binding m) with Not_found -> None)
      a

  (** Raises a Top_encountered exception for the ⊤ map. *)
  let cardinal (a:t) : int =
    M.cardinal (detop a)

  let of_list (l : (Key.t * Value.t) list) : t =
    Nt (M.of_list l)



  (** Iterator functions.
  
      These functions do nothing for the ⊤ map, which is 
      equivalent here to the empty map.
  *)
      
  let iter (f:Key.t -> Value.t -> unit) (a:t) : unit =
    top_apply (M.iter f) () a

  let fold (f:Key.t -> Value.t -> 'a -> 'a) (a:t) (x:'a) : 'a =
    top_dfl1 x (fun a -> M.fold f a x) a

  let map (f:Value.t -> Value.t) (a:t) : t =
    top_lift1 (fun m -> M.fold (fun k v n -> m_add k (f v) n) m M.empty) a

  let mapi (f:Key.t -> Value.t -> Value.t) (a:t) : t =
    top_lift1 (fun m -> M.fold (fun k v n -> m_add k (f k v) n) m M.empty) a

  let for_all (f:Key.t -> Value.t -> bool) (a:t) : bool =
    top_dfl1 false (M.for_all f) a

  let exists (f:Key.t -> Value.t -> bool) (a:t) : bool =
    top_dfl1 false (M.exists f) a



  (**
     Binary iterators.

     If a key is bound in only one map, the function is called
     with the ⊥ value as missing argument.
     These functions do nothing if either map is the ⊤ map.     
  *)
  
  let map2 (f:Key.t -> Value.t -> Value.t -> Value.t) (m1:t) (m2:t) : t =
    top_lift2
      (fun m1 m2 ->
         M.fold2o
           (fun k v1 n -> m_add k (f k v1 Value.bottom) n)
           (fun k v2 n -> m_add k (f k Value.bottom v2) n)
           (fun k v1 v2 n -> m_add k (f k v1 v2) n)
           m1 m2 M.empty
      )
      m1 m2

  let iter2 (f:Key.t -> Value.t -> Value.t -> unit) (m1:t) (m2:t) : unit =
    top_apply2
      () ()
      (M.iter2o
         (fun k v1 -> f k v1 Value.bottom)
         (fun k v2 -> f k Value.bottom v2)
         f)
      m1 m2

  let fold2 (f:Key.t -> Value.t -> Value.t -> 'a -> 'a) (m1:t) (m2:t) (acc:'a) : 'a =
    top_apply2
      acc acc
      (fun m1 m2 ->
         M.fold2o
           (fun k v1 acc -> f k v1 Value.bottom acc)
           (fun k v2 acc -> f k Value.bottom v2 acc)
           f
           m1 m2 acc
      )
      m1 m2

  let for_all2 (f:Key.t -> Value.t -> Value.t -> bool) (m1:t) (m2:t) : bool =
    top_dfl2
      false
      (M.for_all2o
         (fun k v1 -> f k v1 Value.bottom)
         (fun k v2 -> f k Value.bottom v2)
         f
      )
      m1 m2

  let exists2 (f:Key.t -> Value.t -> Value.t -> bool) (m1:t) (m2:t) : bool =
    top_dfl2
      false
      (M.exists2o
         (fun k v1 -> f k v1 Value.bottom)
         (fun k v2 -> f k Value.bottom v2)
         f
      )
      m1 m2


  let map2z (f:Key.t -> Value.t -> Value.t -> Value.t) (m1:t) (m2:t) : t =
    top_lift2
      (fun m1 m2 ->
         M.fold2zo
           (fun k v1 n -> m_add k (f k v1 Value.bottom) n)
           (fun k v2 n -> m_add k (f k Value.bottom v2) n)
           (fun k v1 v2 n -> m_add k (f k v1 v2) n)
           m1 m2 M.empty
      )
      m1 m2

  let iter2z (f:Key.t -> Value.t -> Value.t -> unit) (m1:t) (m2:t) : unit =
    top_apply2
      () ()
      (M.iter2zo
         (fun k v1 -> f k v1 Value.bottom)
         (fun k v2 -> f k Value.bottom v2)
         f)
      m1 m2

  let fold2z (f:Key.t -> Value.t -> Value.t -> 'a -> 'a) (m1:t) (m2:t) (acc:'a) : 'a =
    top_apply2
      acc acc
      (fun m1 m2 ->
         M.fold2z
           f
           m1 m2 acc
      )
      m1 m2

  let fold2zo
      (f1:Key.t -> Value.t -> 'a -> 'a)
      (f2:Key.t -> Value.t -> 'a -> 'a)
      (f:Key.t -> Value.t -> Value.t -> 'a -> 'a) (m1:t) (m2:t) (acc:'a) : 'a =
    top_apply2
      acc acc
      (fun m1 m2 ->
         M.fold2zo f1 f2 f m1 m2 acc
      )
      m1 m2

  let for_all2z (f:Key.t -> Value.t -> Value.t -> bool) (m1:t) (m2:t) : bool =
    top_dfl2
      false
      (M.for_all2zo
         (fun k v1 -> f k v1 Value.bottom)
         (fun k v2 -> f k Value.bottom v2)
         f
      )
      m1 m2

  let exists2z (f:Key.t -> Value.t -> Value.t -> bool) (m1:t) (m2:t) : bool =
    top_dfl2
      false
      (M.exists2zo
         (fun k v1 -> f k v1 Value.bottom)
         (fun k v2 -> f k Value.bottom v2)
         f
      )
      m1 m2


  

  (** Slice iterations. *)
  

  let map_slice (f:Key.t -> Value.t -> Value.t) (m:t) (lo:Key.t) (hi:Key.t) : t =
    top_lift1 (fun m -> M.fold_slice (fun k v n -> m_add k (f k v) n) m lo hi M.empty) m

  let iter_slice (f:Key.t -> Value.t -> unit) (m:t) (lo:Key.t) (hi:Key.t) : unit =
    top_apply (fun m -> M.iter_slice f m lo hi) () m
      
  let fold_slice (f:Key.t -> Value.t -> 'a -> 'a) (m:t) (lo:Key.t) (hi:Key.t) (acc:'a) : 'a =
    top_apply (fun m -> M.fold_slice f m lo hi acc) acc m

  let for_all_slice (f:Key.t -> Value.t -> bool) (m:t) (lo:Key.t) (hi:Key.t) : bool =
    top_dfl1 false (fun m -> M.for_all_slice f m lo hi) m

  let exists_slice (f:Key.t -> Value.t -> bool) (m:t) (lo:Key.t) (hi:Key.t) : bool =
    top_dfl1 false (fun m -> M.exists_slice f m lo hi) m
  
end
