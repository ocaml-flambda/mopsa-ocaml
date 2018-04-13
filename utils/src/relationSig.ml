(**
  RelationSig - Signature of relations (or multimaps) between ordered sets.

  Copyright (C) 2018 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Mine'
 *)


module type OrderedType = MapExtSig.OrderedType

module type S = sig

  
  (** {2 Types} *)

  type t
  (** Represents a relation between a domain and a codomain.
      An element of [t] can be seen as a set of bindings, i.e.,
      a subset of [dom] * [codom], or as a function from [dom] to 
      subsets of [codom] (i.e., a multi-map).
      Relations are imutable, purely functional data-structures.

      Internally, the relation is represented as a map to sets.
      As a consequence, it is easy to get the post-image of an element of 
      the domain, but hard to get the pre-image of an element of the 
      codomain (use [InvRelation] if this is needed).
      We use Maps and Sets, so that it is required to provide
      [OrderedType] structures for the domain and codomain.
   *)

     
     
  type dom
  (** An element of the domain. *)

  type codom
  (** An element of the codomain. *)
             
  module CoDomSet : SetExtSig.S
  (** Data-type for sets of codomain elements. *)

  type codom_set = CoDomSet.t
  (** A set of elements of the codomain. *)
                 
  type binding = dom * codom
  (** A binding. *)
            

  (** {2 Construction and update} *)

  val empty: t
  (** The empty relation. *)
    
  val image: dom -> t -> codom_set
  (** [image x r] is the set of elements associated to [x] in [r]
      (possibly the empty set).
   *)

  val set_image: dom -> codom_set -> t -> t
  (** [set_image x ys r] is a new relation where the image of [x]
      has been updated to be [ys].
   *)

  val is_image_empty: dom -> t -> bool
  (** [is_image_empty x r] returns true if there is no element associated 
      to [x] in [r].
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

  val add_set: dom -> codom_set -> t -> t
  (** [add_set x ys r] returns [r] with all the bindings ([x],[y]) for
      [y] in the set [ys] added. 
      Previous bindings to [x] are preserved.
   *)

  val remove: dom -> codom -> t -> t
  (** [remove x y r] returns [r] with the binding ([x],[y]) removed,
      if it exists.
      Other bindings to [x] are preserved.
   *)

  val remove_set: dom -> codom_set -> t -> t
  (** [remove_set x ys r] returns [r] with a all bindings ([x],[y]) for
      [y] in the set [ys] removed.
      Other bindings to [x] are preserved.
   *)

  val remove_image: dom -> t -> t
  (** [remove_image x r] returns [r] with all bindings to [x] removed.
   *)

  val mem: dom -> codom -> t -> bool
  (** [mem x y r] returns [true] if the binding ([x],[y]) is present in [r].
   *)

  val of_list: binding list -> t
  (** [of_list l] returns a relation constructed from the list of bindings. *)

  val bindings: t -> binding list
  (** [bindings r] lists the bindings in the relation. 
   *)

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
  (** [iter f r] applies [f x y] to every binding ([x],[y]) of [r]. 
      The bindings are considered in increasing order for the lexicographic
      order sorting through dom first, and then codom.
   *)
    
  val fold: (dom -> codom -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f r acc] folds [acc] through every binding ([x],[y]) in [r] through [f]. 
      The bindings are considered in increasing order for the lexicographic
      order sorting through dom first, and then codom.
   *)

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
      The bindings are considered in increasing order for the lexicographic
      order sorting through dom first, and then codom.
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
    
    
    
  (** {2 Multi-map operations} *)

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
                                                  
  
