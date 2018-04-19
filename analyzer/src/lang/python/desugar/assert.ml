(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of assert statements. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.assert"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let init _ ctx _ flow = ctx, flow

  let exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_assert (e, msg)->
      man.eval ctx e flow |>
      eval_to_exec
        (fun e flow ->
          let ok_case = man.exec ctx (mk_assume e (tag_range range "safe case assume")) flow in

          let fail_case =
            debug "checking fail";
            let flow = man.exec ctx (mk_assume (mk_not e e.erange) (tag_range range "fail case assume")) flow in
            if man.flow.is_bottom flow then
              let _ = debug "no fail" in
              man.flow.bottom
            else
              man.exec ctx (
                Builtins.mk_builtin_raise "AssertionError" (tag_range range "fail case raise")
              ) flow
          in

          man.flow.join ok_case fail_case

        )
        (man.exec ctx) man.flow  |>
      return

    | _ -> None

  let eval _ _ _ _  = None


  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
