(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction rules.*)

open Ast
open Manager
open Flow
open Domain

type selector =
  | BOTH
  | HEAD
  | TAIL

module type REDUCTION =
sig

  val refine_eval:
    ('a, 't) manager -> Context.context ->
    expr -> 'a flow ->
    expr * 'a flow -> expr * 'a flow ->
    selector

end

let reductions : (string * (module REDUCTION)) list ref = ref []
let register_reduction name r = reductions := (name, r) :: !reductions
let find_reduction name = List.assoc name !reductions

module Make(Head: REDUCTION)(Tail: REDUCTION) : REDUCTION =
struct

  let refine_eval man ctx exp flow evl1 evl2 =
    match Head.refine_eval man ctx exp flow evl1 evl2 with
    | BOTH -> Tail.refine_eval man ctx exp flow evl1 evl2
    | x -> x

end

module EmptyReduction : REDUCTION =
struct
  let refine_eval man ctx exp flow evl1 evl2 = BOTH
end
