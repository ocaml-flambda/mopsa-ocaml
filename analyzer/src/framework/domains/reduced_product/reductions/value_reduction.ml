(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operators of non-relational abstract values *)

open Manager
open Pool

module type VALUE_REDUCTION =
sig
  val reduce : 'v value_man -> 'v -> 'v
end

(** Registration *)
let reductions : (string * (module VALUE_REDUCTION)) list ref = ref []
let register_reduction name rule = reductions := (name, rule) :: !reductions
let find_reduction name = List.assoc name !reductions
