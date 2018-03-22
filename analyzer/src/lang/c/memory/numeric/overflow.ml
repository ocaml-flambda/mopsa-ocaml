(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handler of numeric overflows. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.memory.numeric.overflow"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let exec stmt manager ctx flow = None
  let init _ _ flow = flow
  let eval exp man ctx flow = None
        
  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
