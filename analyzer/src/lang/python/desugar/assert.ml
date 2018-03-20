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
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.assert"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let init prog manger flow = flow

  let exec stmt manager ctx flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_assert (e, msg)->
      Eval.compose_exec
        e
        (fun e flow ->
          let ok_case = manager.exec (mk_assume e (tag_range range "safe case assume")) ctx flow in

          let fail_case =
            debug "checking fail";
            let flow = manager.exec (mk_assume (mk_not e e.erange) (tag_range range "fail case assume")) ctx flow in
            if manager.flow.is_bottom flow then
              let _ = debug "no fail" in
              manager.flow.bottom
            else
              manager.exec (
                Builtins.mk_builtin_raise "AssertionError" (tag_range range "fail case raise")
              ) ctx flow
          in

          manager.flow.join ok_case fail_case |>
          Exec.return

        )
        (fun flow -> Some flow)
        manager ctx flow

    | _ -> None

  let eval _ _ _ _  = None


  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
