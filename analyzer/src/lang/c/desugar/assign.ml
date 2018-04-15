(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interpreter of assignment expressions. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Utils
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.desugar.assign"
let debug fmt = Debug.debug ~channel:name fmt


(** Abstract domain. *)
module Domain =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)
  
  let init prog man ctx flow = ctx, flow

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_assign(lval, rval) ->
      man.eval rval ctx flow |>
      eval_compose
        (fun rval flow ->
           let flow = man.exec (Universal.Ast.mk_assign lval rval exp.erange) ctx flow in
           oeval_singleton (Some rval, flow, [])
        )

    | _ -> None

  let exec stmt man ctx flow = None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
