(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Intra-procedural control flow abstraction *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Stateless
open Framework.Ast
open Ast

let name = "universal.flows.intraproc"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prg man fa = fa

  let exec stmt man ctx flow =
    match skind stmt with
    | S_expression(e) ->
      Eval.compose_exec
        e
        (fun e flow -> Exec.return flow)
        (fun flow -> Exec.return flow)
        man ctx flow
    | S_block(block) ->
      List.fold_left (fun acc stmt -> man.exec stmt ctx acc) flow block |>
      Exec.return

    | S_if(cond, s1, s2) ->
      let range = srange stmt in
      let flow1 =
        man.exec
          (mk_assume cond (tag_range range "if cond"))
          ctx flow |>
        man.exec s1 ctx
      in
      let flow2 =
        man.exec
          (mk_assume (mk_not cond (tag_range range "neg")) (tag_range range "if not cond"))
          ctx flow |>
        man.exec s2 ctx
      in
      man.flow.join flow1 flow2 |>
      Exec.return

    | _ -> None


  let eval _ _ _ _   = None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain)
