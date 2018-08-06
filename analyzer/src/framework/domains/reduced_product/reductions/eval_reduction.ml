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

module type EVAL_REDUCTION =
sig
  val reduce : Ast.expr -> ('a, 'd, 'v) pool_man -> ('a, 'b) man -> 'a pool_evl -> 'a pool_evl
end

(** Registration *)
let reductions : (string * (module EVAL_REDUCTION)) list ref = ref []
let registerreduction name rule = reductions := (name, rule) :: !reductions
let find_reduction name = List.assoc name !reductions
