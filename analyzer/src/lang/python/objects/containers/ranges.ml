(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python range objects. *)

open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Domains.Stateless
open Framework.Flow
open Framework.Exceptions
open Universal.Ast
open Universal.Ast
open Utils
open Ast
open Addr

let name = "python.objects.containers.ranges"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct


  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with

    | _ -> None

  let init _ ctx _ flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
