(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

type (_, _) eq = Eq : ('a, 'a) eq
(** Witness of type equality *)

module type KEY =
sig
  type _ t
end
