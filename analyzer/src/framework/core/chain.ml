type 'a info = {
  compare : ('a -> 'a -> int) -> 'a -> 'a -> int;
  print   : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit;
}

