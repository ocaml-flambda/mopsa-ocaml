(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Common exceptions raised by domains and processed by the
   analyzer. *)

exception Panic of string

exception PanicAt of Location.range * string

val panic : ('a, Format.formatter, unit, 'b) format4 -> 'a

val panic_at : Location.range -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val fail : ('a, Format.formatter, unit, 'b) format4 -> 'a
