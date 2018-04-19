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
open Framework.Eval
open Framework.Exec
open Ast

let name = "universal.flows.intraproc"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init man ctx prg fa = ctx, fa

  let exec man ctx stmt flow =
    match skind stmt with
    | S_expression(e) ->
      man.eval ctx e flow |>
      eval_to_exec (fun e flow -> flow) (man.exec ctx) man.flow |>
      return

    | S_block(block) ->
      List.fold_left (fun acc stmt -> man.exec ctx stmt acc) flow block |>
      return

    | S_if(cond, s1, s2) ->
      let range = srange stmt in
      let flow1 =
        man.exec ctx
          (mk_assume cond (tag_range range "if cond"))
          flow |>
        man.exec ctx s1
      in
      let flow2 =
        man.exec ctx
          (mk_assume (mk_not cond (tag_range range "neg")) (tag_range range "if not cond"))
          flow |>
        man.exec ctx s2
      in
      man.flow.join flow1 flow2 |>
      return

    | _ -> None


  let eval _ _ _ _   = None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain)
