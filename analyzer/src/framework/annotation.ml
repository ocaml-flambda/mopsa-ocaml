(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

type ('a, _) key = ..

module M = MapPolyHet.Make(struct type ('a, 'b) t = ('a, 'b) key end)

include M
