(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Complex numbers *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.objects.complexes"
let debug fmt = Debug.debug ~channel:name fmt

module Domain= struct

  let init man ctx prog flow = ctx, flow
  let eval man ctx exp flow = None
  let exec man ctx exp flow = None
  let ask man ctx query flow = None

end

let setup () =
  register_domain name (module Domain)
