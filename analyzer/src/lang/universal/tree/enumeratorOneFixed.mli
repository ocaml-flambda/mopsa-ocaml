(** Enumerator module. Given l a list of size (n-1) of list and an
   element e, the iterator will produce every list of size n where at
   least one element is e and all other elements live in l *)

type 'a t =
  {
    length     : int;
    mustbe     : 'a;
    cur_pos    : int;
    enumerator : 'a Enumerator.t
  }

val init : 'a -> 'a list -> int -> 'a t

val next : 'a t -> ('a list * 'a t) option

val fold : ('a list -> 'b -> 'b) -> 'a list -> 'a -> int -> 'b -> 'b
