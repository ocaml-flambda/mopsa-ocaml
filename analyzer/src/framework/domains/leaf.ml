(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Leaf domains have a simplified interface that gives access to
   their local abstractions only; the global manager and its flow
   abstraction are not accessible. *)

open Essentials

module type S =
sig

  include Lattice.LATTICE
  
  val init : Ast.program -> t

  val zone : Zone.t

  val exec : Ast.stmt -> t -> t

  val ask  : 'r Query.query -> t -> 'r option

end

(** Create a full domain from a leaf one. *)
module Make(D: S) : Domain.DOMAIN =
struct

  include D

  let init prog man flow =
    Some (
      set_local T_cur (D.init prog) man flow
    )

  let exec_interface = {
    export = [D.zone];
    import = [];
  }

  let eval_interface = {
    export = [Zone.top, D.zone];
    import = [Zone.top, D.zone];
  }
  
  let exec zone stmt man flow =
    match skind stmt with
    | S_assign(v, e, mode) ->
      Some (
        man.eval ~zone:(Zone.top, D.zone) e flow |>
        Post.bind man @@ fun e' flow ->
        let stmt' = {stmt with skind = S_assign(v, e', mode)} in
        let flow' = map_local T_cur (D.exec stmt') man flow in
        Post.singleton flow'
      )

    | S_assume(e) ->
      Some (
        man.eval ~zone:(Zone.top, D.zone) e flow |>
        Post.bind man @@ fun e' flow ->
        let stmt' = {stmt with skind = S_assume(e')} in
        let flow' = map_local T_cur (D.exec stmt') man flow in
        Post.singleton flow'
      )

    | S_remove_var _ | S_rename_var _ | S_project_vars _ ->
      Some (
        map_local T_cur (D.exec stmt) man flow |>
        Post.singleton
      )

    | _ ->
      None

  let eval zone exp man flow =
    match ekind exp with
    | E_unop(op, e) ->
      Some (
        man.eval ~zone:(Zone.top, D.zone) e flow |> Eval.bind @@ fun e' flow ->
        let exp' = {exp with ekind = E_unop(op, e')} in
        Eval.singleton exp' flow
      )

    | E_binop(op, e1, e2) ->
      Some (
        man.eval ~zone:(Zone.top, D.zone) e1 flow |> Eval.bind @@ fun e1' flow ->
        man.eval ~zone:(Zone.top, D.zone) e2 flow |> Eval.bind @@ fun e2' flow ->
        let exp' = {exp with ekind = E_binop(op, e1', e2')} in
        Eval.singleton exp' flow
      )

    | _ -> None

  let ask query man flow =
    D.ask query (get_local T_cur man flow)

end



let register_domain name modl =
  let module D = Make(val modl : S) in
  Domain.register name (module D)
