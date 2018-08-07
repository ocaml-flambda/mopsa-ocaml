(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction operator for integer intervals and octagon. *)

open Framework.Essentials
open Framework.Domains.Reduced_product.Reductions.State_reduction
open Framework.Domains.Reduced_product.Pool
open Ast

let name = "universal.numeric.reductions.integer_interval_octagon"
let debug fmt = Debug.debug ~channel:name fmt

module Reduction : REDUCTION =
struct

  let reduce stmt dman nman man flow : 'a flow =
    flow

end


let () =
  register_reduction name (module Reduction)
