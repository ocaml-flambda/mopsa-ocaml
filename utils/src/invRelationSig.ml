(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2018-2019 The MOPSA Project.                               *)
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

(**
  InvRelationSig - Signature of relations with access to inverse images.
 *)

module type OrderedType = MapExtSig.OrderedType

module type S = sig


  (** {2 Types} *)

  type t
  (** Represents a relation between a domain and a co-domain.
      An element of [t] can be see as a set of bindings, i.e.,
      a subset of [dom] * [codom], or as a function from [dom] to
      subsets of [codom].
      Relations are imutable, purely functional data-structures.

      This data-type maintains both the relation and its inverse,
      so that we can efficiently compute both the image of a domain
      element and the inverse image of a codomain element, at the cost
      of slower insersion and removal (to keep the maps consistent).
  *)


  type dom
  (** An element of the domain. *)

  type codom
  (** An element of the codomain. *)

  module DomSet : SetExtSig.S
  (** Data-type for sets of domain elements. *)

  module CoDomSet : SetExtSig.S
  (** Data-type for sets of codomain elements. *)

  type dom_set = DomSet.t
  (** A set of elements of the domain. *)

  type codom_set = CoDomSet.t
  (** A set of elements of the codomain. *)

  type binding = dom * codom
  (** A binding. *)



  (** {2 Construction and update} *)


  val empty: t
  (** The empty relation. *)

  val image: dom -> t -> codom_set
  (** [image x r] is the set of codomain elements associated to [x] in [r]
      (possibly the empty set).
   *)

  val inverse: codom -> t -> dom_set
  (** [inverse y r] is the set of domain elements associated to [y] in [r]
      (possibly the empty set).
  *)

  val set_image: dom -> codom_set -> t -> t
  (** [set_image x ys r] is a new relation where the image of [x]
      has been updated to be [ys].
   *)

  val set_inverse: codom -> dom_set -> t -> t
  (** [set_inverse y xs r] is a new relation where the inverse of [y]
      has been updated to be [xs].
   *)

  val is_image_empty: dom -> t -> bool
  (** [is_image_empty x r] returns true if there is a codomain element associated
      to [x] in [r]
   *)

  val is_inverse_empty: codom -> t -> bool
  (** [is_inverse_empty y r] returns true if there is a domain element associated
      to [y] in [r].
   *)

  val is_empty: t -> bool
  (** Whether the relation is empty. *)

  val singleton: dom -> codom -> t
  (** [singleton x y] is the relation with a unique binding ([x],[y]).
   *)

  val add: dom -> codom -> t -> t
  (** [add x y r] returns [r] with a new binding ([x],[y]) added.
      Previous bindings to [x] are preserved.
   *)

  val add_image_set: dom -> codom_set -> t -> t
  (** [add_image_set x ys r] returns [r] with all the bindings ([x],[y]) for
      [y] in the set [ys] added.
      Others bindings are preserved.
   *)

  val add_inverse_set: codom -> dom_set -> t -> t
  (** [add_inverse_set xs y r] returns [r] with all the bindings ([x],[y]) for
      [x] in the set [xs] added.
      Other bindings are preserved.
   *)

  val remove: dom -> codom -> t -> t
  (** [remove x y r] returns [r] with the binding ([x],[y]) removed.
      Other bindings are preserved.
   *)

  val remove_image_set: dom -> codom_set -> t -> t
  (** [remove_image_set x ys r] returns [r] with a all bindings ([x],[y]) for
      [y] in the set [ys] removed.
      Other bindings are preserved.
   *)

  val remove_inverse_set: codom -> dom_set -> t -> t
  (** [remove_inverse_set xs y r] returns [r] with a all bindings ([x],[y]) for
      [x] in the set [xs] removed.
      Other bindings are preserved.
   *)

  val remove_image: dom -> t -> t
  (** [remove_image x r] returns [r] with all bindings ([x],[y]) removed.
   *)

  val remove_inverse: codom -> t -> t
  (** [remove_inverse y r] returns [r] with all bindings ([x],[y]) removed.
   *)

  val mem: dom -> codom -> t -> bool
  (** [mem x y r] returns [true] if the binding ([x],[y]) is present in [r].
   *)

  val of_list: binding list -> t
  (** [of_list l] returns a relation constructed from the list of bindings. *)

  val bindings: t -> binding list
  (** [bindings r] lists the bindings in the relation. *)

  val min_binding: t -> binding
  (** [min_binding r] returns the smallest binding in [r], for the
      lexicographic order of domain and codomain.
      Raises [Not_found] if the relation is empty.
   *)

  val max_binding: t -> binding
  (** [max_binding r] returns the largest binding in [r], for the
      lexicographic order of domain and codomain.
      Raises [Not_found] if the relation is empty.
   *)

  val choose: t -> binding
  (** [choose r] returns an arbitrary binding in [r].
      Raises [Not_found] if the relation is empty.
   *)

  val cardinal: t -> int
  (** [cardinal r] returns the number of bindings in [r]. *)



  (** {2 Global operations} *)


  val iter: (dom -> codom -> unit) -> t -> unit
  (** [iter f r] applies [f x y] to every binding ([x],[y]) of [r]. *)

  val fold: (dom -> codom -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f r acc] folds [acc] through every binding ([x],[y]) in [r] through [f]. *)

  val map: (dom -> codom -> binding) -> t -> t
  (** [map f r] returns a relation where every binding ([x],[y]) is replaced
      with ([x'],[y'])=[f x y].
      The bindings are considered in increasing order for the lexicographic
      order sorting through dom first, and then codom.
   *)

  val domain_map: (dom -> dom) -> t -> t
  (** [domain_map f r] returns a relation where every binding ([x],[y]) is
      replaced with ([f x],[y]).
      The bindings are considered in increasing order for the lexicographic
      order sorting through dom first, and then codom.
   *)

  val codomain_map: (codom -> codom) -> t -> t
  (** [codomain_map f r] returns a relation where every binding ([x],[y])
      is replaced with ([x],[f y]).
      The bindings are considered in increasing order for the dual lexicographic
      order sorting through codom first, and then dom.
   *)

  val for_all: (dom -> codom -> bool) -> t -> bool
  (** [for_all f r] returns true if [f x y] is true for every binding
      ([x],[y]) in [r].
      The bindings are considered in increasing order for the lexicographic
      order sorting through dom first, and then codom.
   *)

  val exists: (dom -> codom -> bool) -> t -> bool
  (** [exists f r] returns true if [f x y] is true for at leat one
      binding ([x],[y]) in [r].
      The bindings are considered in increasing order for the lexicographic
      order sorting through dom first, and then codom.
   *)

  val filter: (dom -> codom -> bool) -> t -> t
  (** [filter f r] returns a relation with only the bindings ([x],[y])
      from [r] where [f x y] is true.
   *)



  (** {2 Set operations} *)


  val compare: t -> t -> int
  (** Total ordering function that returns -1, 0 or 1. *)

  val equal: t -> t -> bool
  (** Whether the two relations have the exact same set of bindings. *)

  val subset: t -> t -> bool
  (** [subset r1 r2] returns whether the set of bindings in [s1] is
      included in the set of bindings in [s2].
   *)

  val union: t -> t -> t
  (** [union r s] is the union of the bindings of [r] and [s]. *)

  val inter: t -> t -> t
  (** [inter r s] is the intersection of the bindings of [r] and [s]. *)

  val diff: t -> t -> t
  (** [diff r s] is the set difference of the bindings of [r] and [s]. *)



  (** {2 Binary operations} *)


  val iter2: (dom -> codom -> unit) -> (dom -> codom -> unit) -> (dom -> codom -> unit) -> t -> t -> unit
  (** [iter2 f1 f2 f r1 r2] applies [f1] to the bindings only in [r1],
      [f2] to the bindings only in [r2], and [f] to the bindings in both
      [r1] and [r2].
      The bindings are considered in increasing lexicographic order.
   *)

  val fold2: (dom -> codom -> 'a -> 'a) -> (dom -> codom -> 'a -> 'a) -> (dom -> codom -> 'a -> 'a) -> t -> t -> 'a -> 'a
  (** [fold2 f1 f2 f r1 r2] applies [f1] to the bindings only in [r1],
      [f2] to the bindings only in [r2], and [f] to the bindings in both
      [r1] and [r2].
      The bindings are considered in increasing lexicographic order.
   *)

  val for_all2: (dom -> codom -> bool) -> (dom -> codom -> bool) -> (dom -> codom -> bool) -> t -> t -> bool
  (** [for_all2 f1 f2 f r1 r2] is true if [f1] is true on all the bindings
      only in [r1], [f2] is true on all the bindings only in [r2], and [f]
      is true on all the bindings both in [r1] and [r2].
      The bindings are considered in increasing lexicographic order.
   *)

  val exists2: (dom -> codom -> bool) -> (dom -> codom -> bool) -> (dom -> codom -> bool) -> t -> t -> bool
  (** [exists2 f1 f2 f r1 r2] is true if [f1] is true on one binding
      only in [r1] or [f2] is true on one binding only in [r2], or [f]
      is true on one binding both in [r1] and [r2].
      The bindings are considered in increasing lexicographic order.
   *)



  val iter2_diff: (dom -> codom -> unit) -> (dom -> codom -> unit) -> t -> t -> unit
  (** [iter2_diff f1 f2 r1 r2] applies [f1] to the bindings only in [r1]
      and [f2] to the bindings only in [r2].
      The bindings both in [r1] and [r2] are ignored.
      The bindings are considered in increasing lexicographic order.
      It is equivalent to calling [iter2] with [f = fun x y -> ()],
      but more efficient.
   *)


  val fold2_diff: (dom -> codom -> 'a -> 'a) -> (dom -> codom -> 'a -> 'a) -> t -> t -> 'a -> 'a
  (** [fold2_diff f1 f2 r1 r2] applies [f1] to the bindings only in [r1]
      and [f2] to the bindings only in [r2].
      The bindings both in [r1] and [r2] are ignored.
      The bindings are considered in increasing lexicographic order.
      It is equivalent to calling [fold2] with [f = fun x y acc -> acc],
      but more efficient.
   *)

  val for_all2_diff: (dom -> codom -> bool) -> (dom -> codom -> bool) -> t -> t -> bool
  (** [for_all2_diff f1 f2 f r1 r2] is true if [f1] is true on all the bindings
      only in [r1] and [f2] is true on all the bindings only in [r2].
      The bindings both in [r1] and [r2] are ignored.
      The bindings are considered in increasing lexicographic order.
      It is equivalent to calling [for_all2] with [f = fun x y -> true], but more efficient.
     *)

  val exists2_diff: (dom -> codom -> bool) -> (dom -> codom -> bool) -> t -> t -> bool
  (** [exists2_diff f1 f2 f r1 r2] is true if [f1] is true on one binding
      only in [r1] or [f2] is true on one binding only in [r2].
      The bindings both in [r1] and [r2] are ignored.
      The bindings are considered in increasing lexicographic order.
      It is equivalent to calling [exists2] with [f = fun x y -> false], but more efficient.
     *)



  (** {2 Multi-map domain operations} *)


  (** These functions consider the relation as a map from domain elements
      to codomain sets.
   *)

  val iter_domain: (dom -> codom_set -> unit) -> t -> unit
  (** [iter_domain f r] applies [f x ys] for every domain element [x] and
      its image set [ys] in [r].
      The domain elements are considered in increasing order.
   *)

  val fold_domain: (dom -> codom_set -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_domain f r acc] applies [f x ys acc] for every domain element
      [x] and its image set [ys] in [r].
      The domain elements are considered in increasing order.
   *)

  val map_domain: (dom -> codom_set -> codom_set) -> t -> t
  (** [map_domain f r] returns a new relation where the image set [ys] of
      [x] in [r] is replaced with [f x ys].
      The domain elements are considered in increasing order.
   *)

  val for_all_domain: (dom -> codom_set -> bool) -> t -> bool
  (** [for_all_domain f r] returns true if [f x ys] is true for every
      domain element [x] and its image set [ys] in [r].
      The domain elements are considered in increasing order.
   *)

  val exists_domain: (dom -> codom_set -> bool) -> t -> bool
  (** [exists_domain f r] returns true if [f x ys] is true for at least
      one domain element [x] and its image set [ys] in [r].
      The domain elements are considered in increasing order.
   *)

  val filter_domain: (dom -> codom_set -> bool) -> t -> t
  (** [filter_domain f r] returns a new relation restricted to the
      domain elements [x] with their image [ys] from [r] such that
      [f x ys] is true.
   *)

  val min_domain: t -> dom
  (** [min_domain r] returns the smallest domain element in [r].
      Raises [Not_found] if the relation is empty.
   *)

  val max_domain: t -> dom
  (** [max_domain r] returns the greatest domain element in [r].
      Raises [Not_found] if the relation is empty.
   *)

  val choose_domain: t -> dom
  (** [choose_domain r] returns any domain element in [r].
      Raises [Not_found] if the relation is empty.
   *)

  val cardinal_domain: t -> int
  (** [cardinal r] returns the number of distinct domain elements used in [r]. *)

  val elements_domain: t -> dom list
  (** [elemets_domain r] returns the list of domain elements used in [r].
      The elements are returned in increasing order.
   *)



  (** {2 Multi-map codomain operations} *)


  (** These functions consider the relation as a map from codomain elements
      to domain sets.
   *)

  val iter_codomain: (codom -> dom_set -> unit) -> t -> unit
  (** [iter_codomain f r] applies [f y xs] for every codomain element [y] and
      its inverse image set [xs] in [r].
      The codomain elements are considered in increasing order.
   *)

  val fold_codomain: (codom -> dom_set -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_codomain f r acc] applies [f y xs acc] for every codomain element
      [y] and its inverse image set [xs] in [r].
      The codomain elements are considered in increasing order.
   *)

  val map_codomain: (codom -> dom_set -> dom_set) -> t -> t
  (** [map_codomain f r] returns a new relation where the inverse image set [xs] of
      [y] in [r] is replaced with [f y xs].
      The codomain elements are considered in increasing order.
   *)

  val for_all_codomain: (codom -> dom_set -> bool) -> t -> bool
  (** [for_all_codomain f r] returns true if [f y xs] is true for every
      codomain element [y] and its inverse image set [xs] in [r].
      The codomain elements are considered in increasing order.
   *)

  val exists_codomain: (codom -> dom_set -> bool) -> t -> bool
  (** [exists_codomain f r] returns true if [f y xs] is true for at least
      one codomain element [y] and its inverse image set [xs] in [r].
      The codomain elements are considered in increasing order.
   *)

  val filter_codomain: (codom -> dom_set -> bool) -> t -> t
  (** [filter_codomain f r] returns a new relation restricted to the
      codomain elements [y] with their inverse image [xs] from [r] such that
      [f y xs] is true.
   *)

  val min_codomain: t -> codom
  (** [min_codomain r] returns the smallest codomain element in [r].
      Raises [Not_found] if the relation is empty.
   *)

  val max_codomain: t -> codom
  (** [max_codomain r] returns the greatest codomain element in [r].
      Raises [Not_found] if the relation is empty.
   *)

  val choose_codomain: t -> codom
  (** [choose_codomain r] returns any codomain element in [r].
      Raises [Not_found] if the relation is empty.
   *)

  val cardinal_codomain: t -> int
  (** [cardinal r] returns the number of distinct codomain elements used in [r]. *)

  val elements_codomain: t -> codom list
  (** [elemets_codomain r] returns the list of codomain elements used in [r].
      The elements are returned in increasing order.
   *)



  (** {2 Printing} *)


  type relation_printer = {
      print_empty: string; (** Special text for empty relations *)
      print_begin: string; (** Text before the first binding *)
      print_open: string;  (** Text before a domain element *)
      print_comma: string; (** Text between a domain and a codomain element *)
      print_close: string; (** Text after a codomain element *)
      print_sep: string;   (** Text between two bindings *)
      print_end: string;   (** Text after the last binding *)
    }
  (** Tells how to print a relation. *)

  val printer_default: relation_printer
  (** Print as set: {(dom1,codom1),...,(domN,codomN)} *)

  val to_string: relation_printer -> (dom -> string) -> (codom -> string) -> t -> string
  (** String representation. *)

  val print: relation_printer -> (out_channel -> dom -> unit) -> (out_channel -> codom -> unit) -> out_channel -> t -> unit
  (** Prints to an output_channel (for Printf.(f)printf). *)

  val fprint: relation_printer -> (Format.formatter -> dom -> unit) -> (Format.formatter -> codom -> unit) -> Format.formatter -> t -> unit
  (** Prints to a formatter (for Format.(f)printf). *)

  val bprint: relation_printer -> (Buffer.t -> dom -> unit) -> (Buffer.t -> codom -> unit) -> Buffer.t -> t -> unit
  (** Prints to a string buffer (for Printf.bprintf). *)


end
