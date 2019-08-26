(*
  This file is derived from the map.mli file from the OCaml distribution.
  Changes are marked with the [MOPSA] symbol.

  Modifications are Copyright (C) 2017-2019 The MOPSA Project.

  Original copyright follows.
*)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: mapext.mli,v 1.1 2014-02-24 16:25:06 mine Exp $ *)



module type OrderedType =
  sig
    type t
      (** The type of the map keys. *)
    val compare : t -> t -> int
      (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. *)
  end
(** Input signature of the functor {!Map.Make}. *)


type map_printer = {
    print_empty: string; (** Special text for empty maps *)
    print_begin: string; (** Text before the first binding *)
    print_arrow: string; (** Text between a key and its value *)
    print_sep: string;   (** Text between two bindings *)
    print_end: string;   (** Text after the last binding *)
  }
(** [MOPSA] Tells how to print a map. *)



module type S =
  sig
    type key
    (** The type of the map keys. *)

    type (+'a) t
    (** The type of maps from type [key] to type ['a]. *)

    val empty: 'a t
    (** The empty map. *)

    val is_empty: 'a t -> bool
    (** Test whether a map is empty or not. *)

    val mem: key -> 'a t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val add: key -> 'a -> 'a t -> 'a t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)

    val singleton: key -> 'a -> 'a t
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x].
        @since 3.12.0
     *)

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val remove_min_binding : 'a t -> 'a t

    val merge:
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        @since 3.12.0
     *)

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps.
       We assume implicitly that [cmp x x] always returns 0.
     *)

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys.
       We assume implicitly that [cmp x x] always returns [true].
     *)

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    (* [MOPSA] now guarantees the evaluation order *)
    (** [for_all p m] checks if all the bindings of the map
        satisfy the predicate [p].
        The predicate [p] is tested on bindings according to the key order.
        @since 3.12.0
     *)

    val exists: (key -> 'a -> bool) -> 'a t -> bool
    (* [MOPSA] now guarantees the evaluation order *)
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p].
        The predicate [p] is tested on bindings according to the key order.
        @since 3.12.0
     *)

    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    (* [MOPSA] now guarantees the evaluation order *)
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p].
        The predicate [p] is tested on bindings according to the key order.
        @since 3.12.0
     *)

    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
        @since 3.12.0
     *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map.
        @since 3.12.0
     *)

    val bindings: 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Map.Make}.
        @since 3.12.0
     *)

    val min_binding: 'a t -> (key * 'a)
    (** Return the smallest binding of the given map
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the map is empty.
        @since 3.12.0
     *)

    val max_binding: 'a t -> (key * 'a)
    (** Same as {!Map.S.min_binding}, but returns the largest binding
        of the given map.
        @since 3.12.0
     *)

    val choose: 'a t -> (key * 'a)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
        @since 3.12.0
     *)

    val split: key -> 'a t -> 'a t * 'a option * 'a t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find: key -> 'a t -> 'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)

    val find_opt: key -> 'a t -> 'a option
    (** [find_opt x m] returns [Some v] if the current binding of [x] in [m]
        is [v], or [None] if no such binding exists.
        @since 4.05
    *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)


    (* [MOPSA] additions *)

    (** {2 Additional functions} *)

    val of_list: (key * 'a) list -> 'a t
    (** [of_list l] converts an association list to a map. *)

    val map2: (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** [map2 f m1 m2] is similar to [map] but applies [f] to pairs
       of bindings [a1] from [m1] and [a2] from [m2] corresponding to
       the same key to construct a new map with the same key set.
       [m1] and [m2] must have the same key sets.
       The bindings are passed to [f] in increasing order of key. *)

    val iter2: (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
    (** [iter2 f m1 m2] is similar to [map] but applies [f] to pairs
       of bindings [a1] from [m1] and [a2] from [m2] corresponding to
       the same key.
       [m1] and [m2] must have the same key sets.
       The bindings are passed to [f] in increasing order of key. *)

    val fold2: (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
    (** [fold2 f m1 m2 x] is similar to [fold] but applies [f] to pairs
       of bindings [a1] from [m1] and [a2] from [m2] corresponding to
       the same key.
       [m1] and [m2] must have the same key sets.
       The bindings are passed to [f] in increasing order of keys. *)

    val for_all2: (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** [for_all2 f m1 m2] is similar to [for_all] but applies [f] to pairs
       of bindings [a1] from [m1] and [a2] from [m2] corresponding to
       the same key.
       [m1] and [m2] must have the same key sets.
       The bindings are passed to [f] in increasing order of keys. *)

    val exists2: (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** [exists2 f m1 m2] is similar to [exists] but applies [f] to pairs
       of bindings [a1] from [m1] and [a2] from [m2] corresponding to
       the same key.
       [m1] and [m2] must have the same key sets.
       The bindings are passed to [f] in increasing order of keys. *)




    val map2z: (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    (** [map2z f m1 m2] is similar to [map2 f m1 m2], but physically
        equal subtrees are put unchanged into the result instead of
        being traversed.
        This is more efficient than [map2], and equivalent if [f] is
        side-effect free and idem-potent ([f k a a = a]).
        [m1] and [m2] must have the same key sets.
        The bindings are passed to [f] in increasing order of keys. *)

    val iter2z: (key -> 'a -> 'a -> unit) -> 'a t -> 'a t -> unit
     (** [iter2z f m1 m2] is similar to [iter2 f m1 m2], but physically
        equal subtrees are ignored.
        This is more efficient than [iter2], and equivalent if
        [f k a a] has no effect.
        [m1] and [m2] must have the same key sets.
        The bindings are passed to [f] in increasing order of keys. *)

    val fold2z: (key -> 'a -> 'a -> 'b -> 'b) -> 'a t -> 'a t -> 'b -> 'b
    (** [fold2z f m1 m2 a] is similar to [fold2 f m1 m2 a], but physically
        equal subtrees are ignored.
        This is more efficient than [fold2], and equivalent if
        [f k a a x = x] and has no effect.
        [m1] and [m2] must have the same key sets.
        The bindings are passed to [f] in increasing order of keys. *)

    val for_all2z: (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [for_all2z f m1 m2] is similar to [for_all2 f m1 m2], but returns
        [true] for physically equal subtrees without traversing them.
        This is more efficient than [for_all2z], and equivalent if
        [f k a a = true] and has no effect.
        [m1] and [m2] must have the same key sets.
        The bindings are passed to [f] in increasing order of keys. *)

    val exists2z: (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [exists2z f m1 m2] is similar to [exists2 f m1 m2], but returns
        [false] for physically equal subtrees without traversing them.
        This is more efficient than [exists2z], and equivalent if
        [f k a a = false] and has no effect.
        [m1] and [m2] must have the same key sets.
        The bindings are passed to [f] in increasing order of keys. *)




    val map2o: (key -> 'a -> 'c) -> (key -> 'b -> 'c) -> (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    (** [map2o f1 f2 f m1 m2] is similar to [map2 f m1 m2], but
        accepts maps defined over different sets of keys.
        To get a new binding, [f1] is used for keys appearing only
        in [m1], [f2] for keys appearing only in [m2], and [f] for
        keys appearing in both maps.
        The returned map has bindings for all keys appearing in either
        [m1] or [m2].
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val iter2o: (key -> 'a -> unit) -> (key -> 'b -> unit) -> (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
    (** [iter2o f1 f2 f m1 m2] is similar to [iter2 f m1 m2], but
        accepts maps defined over different sets of keys.
        [f1] is called for keys appearing only in [m1],
        [f2] for keys appearing only in [m2],
        and [f] for keys appearing in both maps.
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val fold2o: (key -> 'a -> 'c -> 'c) -> (key -> 'b -> 'c -> 'c) -> (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
    (** [fold2o f1 f2 f m1 m2 a] is similar to [fold2 f m1 m2 a], but
        accepts maps defined over different sets of keys.
        [f1] is called for keys appearing only in [m1],
        [f2] for keys appearing only in [m2],
        and [f] for keys appearing in both maps.
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val for_all2o: (key -> 'a -> bool) -> (key -> 'b -> bool) -> (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** [for_all2o f1 f2 f m1 m2] is similar to [for_all2 f m1 m2], but
        accepts maps defined over different sets of keys.
        [f1] is called for keys appearing only in [m1],
        [f2] for keys appearing only in [m2],
        and [f] for keys appearing in both maps.
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val exists2o: (key -> 'a -> bool) -> (key -> 'b -> bool) -> (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    (** [fexists2o f1 f2 f m1 m2] is similar to [fexists2 f m1 m2], but
        accepts maps defined over different sets of keys.
        [f1] is called for keys appearing only in [m1],
        [f2] for keys appearing only in [m2],
        and [f] for keys appearing in both maps.
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)



    val map2zo: (key -> 'a -> 'a) -> (key -> 'a -> 'a) -> (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    (** [map2zo f1 f2 f m1 m2] is similar to [map2o f1 f2 f m1 m2] but,
        similary to [map2z], [f] is not called on physically equal
        subtrees.
        This is more efficient than [map2o], and equivalent if [f] is
        side-effect free and idem-potent ([f k a a = a]).
        The returned map has bindings for all keys appearing in either
        [m1] or [m2].
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val iter2zo: (key -> 'a -> unit) -> (key -> 'a -> unit) -> (key -> 'a -> 'a -> unit) -> 'a t -> 'a t -> unit
    (** [iter2zo f1 f2 f m1 m2] is similar to [iter2o f1 f2 f m1 m2] but,
        similary to [iter2z], [f] is not called on physically equal
        subtrees.
        This is more efficient than [iter2o], and equivalent if [f] is
        side-effect free.
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val fold2zo: (key -> 'a -> 'b -> 'b) -> (key -> 'a -> 'b -> 'b) -> (key -> 'a -> 'a -> 'b -> 'b) -> 'a t -> 'a t -> 'b -> 'b
    (** [fold2zo f1 f2 f m1 m2 a] is similar to [fold2o f1 f2 f m1 m2 a] but,
        similary to [fold2z], [f] is not called on physically equal
        subtrees.
        This is more efficient than [fold2o], and equivalent if
        [f k a a x = x] and has no side-effect.
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val for_all2zo: (key -> 'a -> bool) -> (key -> 'a -> bool) -> (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [for_all2zo f1 f2 f m1 m2] is similar to [for_all2o f1 f2 f m1 m2] but,
        similary to [for_all2z], [f] is not called on physically equal
        subtrees.
        This is more efficient than [for_all2o], and equivalent if
        [f k a a = true] and has no side-effect.
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val exists2zo: (key -> 'a -> bool) -> (key -> 'a -> bool) -> (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [exists2zo f1 f2 f m1 m2] is similar to [exists2o f1 f2 f m1 m2] but,
        similary to [exists2z], [f] is not called on physically equal
        subtrees.
        This is more efficient than [exists2o], and equivalent if
        [f k a a = false] and has no side-effect.
        The bindings are passed to [f], [f1], [f2] in increasing order of keys. *)

    val map_slice: (key -> 'a -> 'a) -> 'a t -> key -> key -> 'a t
    (** [map_slice f m k1 k2] is similar to [map f m], but only applies
        [f] to bindings with key greater or equal to [k1] and smaller
        or equal to [k2] to construct the returned map. Bindings with
        keys outside this range in [m] are put unchanged in the result.
        It is as if, outside this range, [f k a = a] and has no effect.
        The result has the same key set as [m].
        The bindings are passed to [f] in increasing order of keys,
        between [k1] and [k2]. *)

    val iter_slice: (key -> 'a -> unit) -> 'a t -> key -> key -> unit
    (** [iter_slice f m k1 k2] is similar to [iter f m], but only calls
        [f] on bindings with key greater or equal to [k1] and smaller
        or equal to [k2].
        It is as if, outside this range, [f k a] has no effect.
        The bindings are passed to [f] in increasing order of keys,
        between [k1] and [k2]. *)

    val fold_slice: (key -> 'a -> 'b -> 'b) -> 'a t -> key -> key -> 'b -> 'b
    (** [fold_slice f m k1 k2 a] is similar to [fold f m], but only calls
        [f] on bindings with key greater or equal to [k1] and smaller
        or equal to [k2].
        It is as if, outside this range, [f k a x = x] and has no effect.
        The bindings are passed to [f] in increasing order of keys,
        between [k1] and [k2]. *)

    val for_all_slice: (key -> 'a -> bool) -> 'a t -> key -> key -> bool
    (** [for_all_slice f m k1 k2 a] is similar to [for_all f m], but only calls
        [f] on bindings with key greater or equal to [k1] and smaller
        or equal to [k2].
        It is as if, outside this range, [f k a = true] and has no effect.
        The bindings are passed to [f] in increasing order of keys,
        between [k1] and [k2]. *)

    val exists_slice: (key -> 'a -> bool) -> 'a t -> key -> key -> bool
    (** [exists_slice f m k1 k2 a] is similar to [exists f m], but only calls
        [f] on bindings with key greater or equal to [k1] and smaller
        or equal to [k2].
        It is as if, outside this range, [f k a = false] and has no effect.
        The bindings are passed to [f] in increasing order of keys,
        between [k1] and [k2]. *)

    val key_equal: 'a t -> 'a t -> bool
    (** [key_equal m1 m2] returns true if [m1] and [m2] are defined
        over exactly the same set of keys (but with possibly different
        values).
     *)

    val key_subset: 'a t -> 'a t -> bool
    (** [key_equal m1 m2] returns true if [m1] is defined on a subset of
        the keys of [m2] (but with possibly different values).
     *)

    val find_greater: key -> 'a t -> key * 'a
    (** [find_greater k m] returns the binding (key and value) in [m]
        with key strictly greater than [k] and as small as possible.
        Raises [Not_found] if [m] has no binding for a key strictly greater
        than [k].
     *)

    val find_less: key -> 'a t -> key * 'a
    (** [find_less k m] returns the binding (key and value) in [m]
        with key strictly less than [k] and as large as possible.
        Raises [Not_found] if [m] has no binding for a key strictly less
        than [k].
     *)

    val find_greater_equal: key -> 'a t -> key * 'a
    (** [find_greater_euql k m] returns the binding (key and value) in [m]
        with key greater or equal to [k] and as small as possible.
        Raises [Not_found] if [m] has no binding for a key greater or equal
        to [k].
     *)

    val find_less_equal: key -> 'a t -> key * 'a
    (** [find_less_equal k m] returns the binding (key and value) in [m]
        with key less or equal to [k] and as large as possible.
        Raises [Not_found] if [m] has no binding for a key less or equal
        to [k].
     *)


    (** {2 Printing} *)

    val to_string: map_printer -> (key -> string) -> ('a -> string) -> 'a t -> string
    (** String representation. *)

    val print: map_printer -> (out_channel -> key -> unit) -> (out_channel -> 'a -> unit) -> out_channel -> 'a t -> unit
    (** Prints to an output_channel (for Printf.(f)printf). *)

    val fprint: map_printer -> (Format.formatter -> key -> unit) -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    (** Prints to a formatter (for Format.(f)printf). *)

    val bprint: map_printer -> (Buffer.t -> key -> unit) -> (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a t -> unit
    (** Prints to a string buffer (for Printf.bprintf). *)

  end
(** Output signature of the functor {!MapExt.Make}. *)
