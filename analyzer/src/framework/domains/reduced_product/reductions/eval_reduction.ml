(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operators of evaluations *)

open Manager
open Pool

module type REDUCTION =
sig
  val reduce : Ast.expr -> ('a, 'd) domain_man -> ('a, 'v) nonrel_man -> ('a, 'b) man -> 'a evl_conj -> 'a evl_conj
end

(** Registration *)
let reductions : (string * (module REDUCTION)) list ref = ref []
let registerreduction name rule = reductions := (name, rule) :: !reductions
let find_reduction name = List.assoc name !reductions
