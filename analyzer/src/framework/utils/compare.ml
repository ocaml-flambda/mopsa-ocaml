(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   [compose cl] applies a list of comparison functions [cl] in sequence
   and stops when encountering the first non-zero result.
*)
let rec compose = function
  | [] -> 0
  | cmp :: tl ->
    let r = cmp () in
    if r <> 0 then r else compose tl
