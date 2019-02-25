type 'a compare_chain = ('a -> 'a -> int) ref

type 'a print_chain = (Format.formatter -> 'a -> unit) ref

val mk_compare_chain : ('a -> 'a -> int) -> 'a compare_chain

val mk_print_chain : (Format.formatter -> 'a -> int) -> 'a print_chain

type 'a info = {
  compare : ('a -> 'a -> int) -> 'a -> 'a -> int;
  print   : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit;
}

val register : 'a info -> 'a compare_chain -> 'a print_chain -> unit

val compare : 'a compare_chain -> 'a -> 'a -> int

val print : 'a print_chain -> Format.formatter -> 'a -> int
