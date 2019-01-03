(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Intra-procedural iterator for blocks, assignments and tests *)

open Mopsa
open Ast


module Domain : Framework.Domains.Stateless.S =
struct

  type _ domain += D_universal_intraproc : unit domain

  let id = D_universal_intraproc
  let name = "universal.iterators.intraproc"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_universal_intraproc -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = [Z_any]; import = []}
  let eval_interface = {export = []; import = []}

  let init prog man flow = None

  let exec zone stmt man flow =
    match skind stmt with
    | S_expression(e) ->
      Some (
        man.eval e flow |>
        Post.bind man @@ fun e flow ->
        Post.of_flow flow
      )

    | S_block(block) ->
      Some (
        List.fold_left (fun acc stmt -> man.exec ~zone stmt acc) flow block |>
        Post.of_flow
      )

    | S_if(cond, s1, s2) ->
      let range = srange stmt in
      let block1 = mk_block [mk_assume cond range; s1] range in
      let block2 = mk_block [mk_assume (mk_not cond range) range; s2] range in
      let flows = Flow.map_list man.exec flow [block1; block2] in
      let flow' = Flow.join_list man flows in
      Some (Post.of_flow flow')

    | S_print ->
      Debug.debug ~channel:"print" "%a@\n  @[%a@]"
        pp_position (srange stmt |> get_range_start)
        (Flow.print man) flow
      ;
      Some (Post.of_flow flow)

    | _ -> None

  let eval zone exp man flow = None

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
