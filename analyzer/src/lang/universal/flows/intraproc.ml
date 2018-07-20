(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Intra-procedural control flow abstraction *)

open Framework.Essentials
open Ast

let name = "universal.flows.intraproc"
let debug fmt = Debug.debug ~channel:name fmt

module Domain : Framework.Domains.Stateless.DOMAIN =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prog man ctx flow = None

  let exec_interface = Framework.Domain.{
    import = [];
    export = [Framework.Zone.top];
  }

  let exec zone stmt man ctx flow =
    match skind stmt with
    | S_expression(e) ->
      man.eval e ctx flow |>
      Post.bind man ctx @@ fun e flow ->
      Post.return flow

    | S_block(block) ->
      List.fold_left (fun acc stmt -> man.exec stmt ctx acc) flow block |>
      Post.return

    | S_if(cond, s1, s2) ->
      let range = srange stmt in
      let flow1 = man.exec (mk_assume cond range) ctx flow |>
                  man.exec s1 ctx
      in
      let flow2 = man.exec (mk_assume (mk_not cond range) range) ctx flow |>
                  man.exec s2 ctx
      in
      man.flow.join flow1 flow2 |>
      Post.return

    | _ -> None

  let eval_interface = Framework.Domain.{
    import = [];
    export = [];
  }
      
  let eval _ _ _ _ _   = None

  let ask _ _ _ _  = None

  end

let setup () =
  Framework.Domains.Stateless.register_domain name (module Domain)
