(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Assignments from iterables. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.iterable_assign"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct


  let rec exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_assign({ekind = E_py_tuple(el)}, exp, kind)
    | S_assign({ekind = E_py_list(el)}, exp, kind) ->
      man.eval ctx (Utils.mk_builtin_call "iter" [exp] range) flow |>
      eval_to_exec
        (fun iter flow ->
           assign_iter man ctx el iter kind range flow
        )
        (man.exec ctx) man.flow

      |>
      return

    | _ -> None

  and assign_iter man ctx el iter mode range flow =
    let stmtl =
      List.fold_left (fun acc e ->
          mk_assign
            e ~mode
            (Utils.mk_builtin_call "next" [iter] range)
            (tag_range range "next assign")
          :: acc
        ) [] el |>
      List.rev
    in
    let block = mk_block stmtl (tag_range range "next assign block") in
    let stmt =
      mk_try
        block
        [mk_except
           (Some (mk_py_object (Addr.find_builtin "StopIteration") (tag_range range "stop iter")))
           None
           (Utils.mk_builtin_raise "ValueError" (tag_range range "error raise"))
        ]
        (mk_nop (tag_range range "empty try else"))
        (mk_nop (tag_range range "empty try finally"))
        (tag_range range "try next")
    in

    man.exec ctx stmt flow


  let init _ ctx _ flow = ctx, flow
  let eval _ _ _ _ = None
  let ask _ _ _ _ = None


end

let setup () =
  Stateless.register_domain name (module Domain)
