(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inliner of imported packages. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.loops"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_while (test, body, orelse) ->
      man.exec ctx
        (mk_while
           (mk_one range)
           (mk_block [
               mk_if
                 (mk_not test range)
                 (mk_block [
                     orelse;
                     mk_stmt S_break range
                   ] range)
                 (mk_block [] range)
                 range
               ;
               body
             ] range)
           range
        ) flow |>
      return

    | S_py_for(target, iterable, body, orelse) ->
      man.eval ctx (Utils.mk_builtin_call "iter" [iterable] range) flow |>
      eval_to_exec (fun iter flow ->
          let stmt =
            mk_while
              (mk_one range)
              (mk_block [
                  (Utils.mk_try_stopiteration
                    (mk_assign
                       target
                       (Utils.mk_builtin_call "next" [iter] range)
                       range
                    )
                    (mk_block [
                        orelse;
                        mk_stmt S_break range
                      ] range)
                    range)
                  ;
                  body
                ] range)
              range
          in
          man.exec ctx stmt flow
        ) (man.exec ctx) man.flow |>
      return

    | _ -> None

  let init _ ctx _ flow = ctx, flow
  let eval _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
