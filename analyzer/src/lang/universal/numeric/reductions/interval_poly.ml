(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduced product of intervals and polyhedra domains. *)

open Framework.Essentials
open Framework.Domains.Reductions.Domain_reduction
open Ast

let name = "universal.numeric.reductions.interval_poly"
let debug fmt = Debug.debug ~channel:name fmt

module I = Framework.Domains.Nonrel.Make(Values.Int.Value)
module P = Relational.Poly

type _ key +=
  | Itv : I.t key
  | Poly : P.t key

module Reduction : REDUCTION =
struct

  let exec stmt pman ctx flow = None
  let eval exp pman ax ctx peval = None
end


let () =
  register_reduction name {
    pool = Pool.[Domain (Itv, (module I)); Domain (Poly, (module P))];
    eq = (
      let f : type a b. a key -> b key -> (a, b) eq option = fun k1 k2 ->
        match k1, k2 with
        | Itv, Itv -> Some Eq
        | Poly, Poly -> Some Eq
        | _ -> None
      in
      f
    )
  } (module Reduction)
