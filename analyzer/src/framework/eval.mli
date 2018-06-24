(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


type ('e, 'a) case = {
  result : 'e option;
  flow: 'a Flow.flow;
  cleaners: Ast.stmt list;
}

type ('e, 'a) eval

val return : ('e, 'a) eval -> ('e, 'a) eval option

val case : 'e option -> ?cleaners:Ast.stmt list -> 'a Flow.flow -> ('e, 'a) eval

val singleton : 'e option -> ?cleaners:Ast.stmt list -> 'a Flow.flow -> ('e, 'a) eval option
(** Singleton evaluation *)

val join : ('e, 'a) eval option -> ('e, 'a) eval option -> ('e, 'a) eval option
(** Compute the union of two evaluations *)

val add_cleaners : Ast.stmt list -> ('e, 'a) eval option -> ('e, 'a) eval option
(** Add cleaners to an evaluation *)


val map_: ('e -> 'a Flow.flow -> ('f, 'a) case) -> ('e, 'a) eval -> ('f, 'a) eval
val map: ('e -> 'a Flow.flow -> ('f, 'a) case) -> ('e, 'a) eval option -> ('f, 'a) eval option

val iter_: ('e -> 'a Flow.flow -> unit) -> ('e, 'a) eval -> unit
val iter: ('e -> 'a Flow.flow -> unit) -> ('e, 'a) eval option -> unit

val fold_: ('b -> ('e, 'a) case -> 'b) -> 'b -> ('e, 'a) eval  -> 'b
val fold: ('b -> ('e, 'a) case -> 'b) -> 'b -> ('e, 'a) eval option -> 'b option

val bind_ : ('e -> 'a Flow.flow -> ('f, 'a) eval) -> ('e, 'a) eval -> ('f, 'a) eval
val bind : ('e -> 'a Flow.flow -> ('f, 'a) eval option) -> ('e, 'a) eval -> ('f, 'a) eval option

val print: pp:(Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, 'a) eval -> unit
