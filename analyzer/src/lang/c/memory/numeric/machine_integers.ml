(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational numeric abstraction of machine integers. *)

open Framework.Domains.Global
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.memory.numeric.machine_integers"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                         {2 Value Abstraction}                           *)
(*==========================================================================*)

(** Interval abstraction of machine integers *)
module Value =
struct
  include Universal.Numeric.Integers

  let fwd_unop op v =
    assert false

  let bwd_unop op v r =
    assert false

  let fwd_binop op v1 v2 =
    assert false

  let bwd_binop op v1 v2 r =
    assert false

  let fwd_filter op v1 v2 =
    assert false

  let bwd_filter op v1 v2 =
    assert false

  let assume_true v =
    assert false

  let can_be_true v =
    assert false

  let assume_false v =
    assert false

  let can_be_false v =
    assert false
end


(*==========================================================================*)
(**                    {2 Environment Abstraction}                          *)
(*==========================================================================*)


(** Abstract domain. *)
module Domain =
struct


  (*==========================================================================*)
  (**                           {2 Lattice}                                   *)
  (*==========================================================================*)

  module VMap = Universal.Nonrel.Domain.Make(Value)

  include VMap

  let print fmt a =
    Format.fprintf fmt "int: @[%a@]@\n"
      VMap.print a

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init prog man flow = flow

  let exec stmt man ctx flow = None

  let eval exp man ctx flow = None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
