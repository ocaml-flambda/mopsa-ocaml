(*
  This file is derived from the set.mli file from the OCaml distribution.
  Changes are marked with the [MOPSA] symbol.

  Modifications are Copyright (C) 2017-2019 The MOPSA Project.

  Original copyright follows.
*)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Sets over ordered types.

   This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance.

   The {!Make} functor constructs implementations for any type, given a
   [compare] function.
   For instance:
   {[
     module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Stdlib.compare x0 x1 with
               0 -> Stdlib.compare y0 y1
             | c -> c
       end

     module PairsSet = Set.Make(IntPairs)

     let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (11,13))
   ]}

   This creates a new module [PairsSet], with a new type [PairsSet.t]
   of sets of [int * int].
*)

module type OrderedType = MapExtSig.OrderedType

                        
type set_printer = {
    print_empty: string; (** Special text for empty sets *)
    print_begin: string; (** Text before the first element *)
    print_sep: string;   (** Text between two elements *)
    print_end: string;   (** Text after the last element *)
  }
(** [MOPSA] Tells how to print a set. *)


                        
module type S =
  sig
    type elt
    (** The type of the set elements. *)

    type t
    (** The type of sets. *)

    val empty: t
    (** The empty set. *)

    val is_empty: t -> bool
    (** Test whether a set is empty or not. *)

    val mem: elt -> t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

    val add: elt -> t -> t
    (** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged
       (the result of the function is then physically equal to [s]).
       @before 4.03 Physical equality was not ensured. *)

    val singleton: elt -> t
    (** [singleton x] returns the one-element set containing only [x]. *)

    val remove: elt -> t -> t
    (** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged
       (the result of the function is then physically equal to [s]).
       @before 4.03 Physical equality was not ensured. *)

    val union: t -> t -> t
    (** Set union. *)

    val inter: t -> t -> t
    (** Set intersection. *)

    val diff: t -> t -> t
    (** Set difference. *)

    val compare: t -> t -> int
    (** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

    val equal: t -> t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)

    val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

    val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
       The elements of [s] are presented to [f] in increasing order
       with respect to the ordering over the type of the elements. *)

    val map: (elt -> elt) -> t -> t
    (** [map f s] is the set whose elements are [f a0],[f a1]... [f
        aN], where [a0],[a1]...[aN] are the elements of [s].

       The elements are passed to [f] in increasing order
       with respect to the ordering over the type of the elements.

       If no element of [s] is changed by [f], [s] is returned
       unchanged. (If each output of [f] is physically equal to its
       input, the returned set is physically equal to [s].)
       @since 4.04.0 *)

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s], in increasing order. *)

    val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

    val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. If [p] satisfies every element in [s],
       [s] is returned unchanged (the result of the function is then
       physically equal to [s]).
       @before 4.03 Physical equality was not ensured.*)

    val partition: (elt -> bool) -> t -> t * t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

    val cardinal: t -> int
    (** Return the number of elements of a set. *)

    val elements: t -> elt list
    (** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

    val min_elt: t -> elt
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the set is empty. *)

    val min_elt_opt: t -> elt option
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or [None]
       if the set is empty.
        @since 4.05
    *)

    val max_elt: t -> elt
    (** Same as {!Set.S.min_elt}, but returns the largest element of the
       given set. *)

    val max_elt_opt: t -> elt option
    (** Same as {!Set.S.min_elt_opt}, but returns the largest element of the
        given set.
        @since 4.05
    *)

    val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. *)

    val choose_opt: t -> elt option
    (** Return one element of the given set, or [None] if
        the set is empty. Which element is chosen is unspecified,
        but equal elements will be chosen for equal sets.
        @since 4.05
    *)

    val split: elt -> t -> t * bool * t
    (** [split x s] returns a triple [(l, present, r)], where
          [l] is the set of elements of [s] that are
          strictly less than [x];
          [r] is the set of elements of [s] that are
          strictly greater than [x];
          [present] is [false] if [s] contains no element equal to [x],
          or [true] if [s] contains an element equal to [x]. *)

    val find: elt -> t -> elt
    (** [find x s] returns the element of [s] equal to [x] (according
        to [Ord.compare]), or raise [Not_found] if no such element
        exists.
        @since 4.01.0 *)

    val find_opt: elt -> t -> elt option
    (** [find_opt x s] returns the element of [s] equal to [x] (according
        to [Ord.compare]), or [None] if no such element
        exists.
        @since 4.05 *)

    val find_first: (elt -> bool) -> t -> elt
    (** [find_first f s], where [f] is a monotonically increasing function,
       returns the lowest element [e] of [s] such that [f e],
       or raises [Not_found] if no such element exists.

       For example, [find_first (fun e -> Ord.compare e x >= 0) s] will return
       the first element [e] of [s] where [Ord.compare e x >= 0] (intuitively:
       [e >= x]), or raise [Not_found] if [x] is greater than any element of
       [s].

        @since 4.05
       *)

    val find_first_opt: (elt -> bool) -> t -> elt option
    (** [find_first_opt f s], where [f] is a monotonically increasing function,
       returns an option containing the lowest element [e] of [s] such that
       [f e], or [None] if no such element exists.
        @since 4.05
       *)

    val find_last: (elt -> bool) -> t -> elt
    (** [find_last f s], where [f] is a monotonically decreasing function,
       returns the highest element [e] of [s] such that [f e],
       or raises [Not_found] if no such element exists.
        @since 4.05
       *)

    val find_last_opt: (elt -> bool) -> t -> elt option
    (** [find_last_opt f s], where [f] is a monotonically decreasing function,
       returns an option containing the highest element [e] of [s] such that
       [f e], or [None] if no such element exists.
        @since 4.05
       *)

    val of_list: elt list -> t
    (** [of_list l] creates a set from a list of elements.
        This is usually more efficient than folding [add] over the list,
        except perhaps for lists with many duplicated elements.
        @since 4.02.0 *)




    (* [MOPSA] additions *)
   
    (** {2 Additional functions} *)

    val iter2: (elt -> unit) -> (elt -> unit) -> (elt -> unit) -> t -> t -> unit
    (** [iter2 f1 f2 f s1 s2] applies [f1] to the elements only in [s1],
        [f2] to the elements only in [s2], and [f] to the elements in both
        [s1] and [s2].
        The elements are considered in increasing order.
     *)

    val fold2: (elt -> 'a -> 'a) -> (elt -> 'a -> 'a) -> (elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
    (** [fold2 f1 f2 f s1 s2 acc] applies [f1] to the elements only in [s1],
        [f2] to the elements only in [s2], and [f] to the elements both
        in [s1] and [s2].
        The elements are considered in increasing order.
     *)
      
    val for_all2: (elt -> bool) -> (elt -> bool) -> (elt -> bool) -> t -> t -> bool
    (** [for_all2 f1 f2 f s1 s2] is true if [f1] is true on all the elements
        only in [s1], [f2] is true on all the elements only in [s2], and [f] 
        is true on all the elements both in [s1] and [s2].
        The elements are considered in increasing order.
     *)

    val exists2: (elt -> bool) -> (elt -> bool) -> (elt -> bool) -> t -> t -> bool
    (** [exists2 f1 f2 f s1 s2] is true if [f1] is true on one element
        only in [s1], or [f2] is true on one element only in [s2], or [f] 
        is true on one element both in [s1] and [s2].
        The elements are considered in increasing order.
     *)

    val iter2_diff: (elt -> unit) -> (elt -> unit) -> t -> t -> unit
    (** [iter2_diff f1 f2 s1 s2] applies [f1] to the elements only in [s1]
        and [f2] to the elements only in [s2].
        The elements both in [s1] and [s2] are ignored.
        The elements are considered in increasing order.
        It is equivalent to calling [iter2] with [f = ignore], but more efficient. 
     *)


    val fold2_diff: (elt -> 'a -> 'a) -> (elt -> 'a -> 'a) -> t -> t -> 'a -> 'a
    (** [fold2_diff f1 f2 s1 s2] applies [f1] to the elements only in [s1] and
        [f2] to the elements only in [s2].
        The elements both in [s1] and [s2] are ignored.
        The elements are considered in increasing order.
        It is equivalent to calling [fold2] with [f = fun v acc -> acc], but more efficient. 
     *)

    val for_all2_diff: (elt -> bool) -> (elt -> bool) -> t -> t -> bool
    (** [for_all2_diff f1 f2 f s1 s2] is true if [f1] is true on all the elements
        only in [s1] and [f2] is true on all the elements only in [s2].
        The elements both in [s1] and [s2] are ignored.
        The elements are considered in increasing order.
        It is equivalent to calling [for_all2] with [f = fun x -> true], but more efficient. 
     *)

    val exists2_diff: (elt -> bool) -> (elt -> bool) -> t -> t -> bool
    (** [exists2_diff f1 f2 f s1 s2] is true if [f1] is true on one element
        only in [s1] or if [f2] is true on one element only in [s2].
        The elements both in [s1] and [s2] are ignored.
        The elements are considered in increasing order.
        It is equivalent to calling [exists2] with [f = fun x -> false], but more efficient. 
     *)

    val diff_list: t -> t -> elt list
    (** [diff_list s1 s2] returns the list of elements in [s1] and not [s2]. *)

    val sym_diff_list: t -> t -> elt list * elt list
    (** [symb_diff_list s1 s2] returns the list of elements in [s1] and 
        not [s2], and the list of elements in [s2] and not in [s1]. 
     *)

    val add_sym_diff: t -> elt list * elt list -> t
    (** [add_sym_diff s2 d] returns [s1] assuming that 
        [d = sym_diff_list s1 s2], i.e., it reconstructs a set given its 
        symmetric difference with another set. 
     *)

      
    val iter_slice: (elt -> unit) -> t -> elt -> elt -> unit
    (** [iter_slice f m k1 k2] is similar to [iter f m], but only calls
        [f] on elements greater or equal to [k1] and smaller
        or equal to [k2].
        It is as if, outside this range, [f k] has no effect.
        The elements are considered in increasing order.
     *)

    val fold_slice: (elt -> 'a -> 'a) -> t -> elt -> elt -> 'a -> 'a
    (** [fold_slice f m k1 k2 a] is similar to [fold f m], but only calls
        [f] on elements greater or equal to [k1] and smaller
        or equal to [k2].
        It is as if, outside this range, [f k x = x] and has no effect.
        The elements are considered in increasing order.
     *)

    val for_all_slice: (elt -> bool) -> t -> elt -> elt -> bool
    (** [for_all_slice f m k1 k2 a] is similar to [for_all f m], but only calls
        [f] on elements greater or equal to [k1] and smaller
        or equal to [k2].
        It is as if, outside this range, [f k = true] and has no effect.
        The elements are considered in increasing order.
     *)

    val exists_slice: (elt -> bool) -> t -> elt -> elt -> bool
    (** [exists_slice f m k1 k2 a] is similar to [exists f m], but only calls
        [f] on elements greater or equal to [k1] and smaller
        or equal to [k2].
        It is as if, outside this range, [f k = false] and has no effect.
        The elements are considered in increasing order.
     *)

      
    (** {2 Printing} *)

    val to_string: set_printer -> (elt -> string) -> t -> string
    (** String representation. *)

    val print: set_printer -> (out_channel -> elt -> unit) -> out_channel -> t -> unit
    (** Prints to an output_channel (for Printf.(f)printf). *)

    val fprint: set_printer -> (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
    (** Prints to a formatter (for Format.(f)printf). *)

    val bprint: set_printer -> (Buffer.t -> elt -> unit) -> Buffer.t -> t -> unit
    (** Prints to a string buffer (for Printf.bprintf). *)                     


    (** {2 Translation to polymorphic sets} *)

    val to_poly_set : t -> elt SetExtPoly.t
  end
(** Output signature of the functor {!SetExt.Make}. *)
