(** Enumerator module. Given l a list of size n of list, the iterator
   will produce every list of size n where the first element lives in
   l[0], the first in l[1] ... . init_id and fold_id are particular
   cases where all lists are identical *)

type 'a t

val init : 'a list list -> 'a t

val init_id : 'a list -> int -> 'a t

val next : 'a t -> ('a list * 'a t) option

val reset : 'a t -> 'a t

val fold : ('a list -> 'b -> 'b) -> 'a list list -> 'b -> 'b

val fold_id : ('a list -> 'b -> 'b) -> 'a list -> int -> 'b -> 'b
