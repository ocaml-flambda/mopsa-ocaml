(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational abstract domain of real variables. *)

open Framework.Query
open Framework.Ast
open Framework.Manager
open Framework.Domains.Global
open Ast
open Bot

let name = "universal.numeric.floats"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct
  include Nonrel.Domain.Make(Value)

  let print fmt a =
    Format.fprintf fmt "int: @[%a@]@\n" print a
  
  let init prog man ctx flow =
    ctx, set_domain_cur top man flow
    
end

let setup () =
  register_domain name (module Domain)
