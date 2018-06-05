(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python tuples. *)


open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Framework.Domains.Stateless
open Framework.Flow
open Framework.Exceptions
open Universal.Ast
open Universal.Ast
open Utils
open Ast
open Addr

let name = "python.objects.containers.tuples"
let debug fmt = Debug.debug ~channel:name fmt

module Domain = struct

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with

    | _ -> None

  let init man ctx prog flow = ctx, flow

  let exec man ctx stmt flow = None

  let ask man ctx query flow = None

end

let setup () =
  register_domain name (module Domain)
