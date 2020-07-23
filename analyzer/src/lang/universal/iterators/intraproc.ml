(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Intra-procedural iterator for blocks, assignments and tests *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast


module Domain =
struct

  include GenStatelessDomainId(
    struct
      let name = "universal.iterators.intraproc"
    end
    )

  let alarms = []

  let init prog man flow = flow

  let exec stmt man flow =
    match skind stmt with
    | S_expression e ->
      man.eval e flow >>$? fun e flow ->
      Post.return flow |>
      OptionExt.return

    | S_assign(lval, rval) ->
      man.eval rval flow >>$? fun rval flow ->
      man.post ~route:Below (mk_assign lval rval stmt.srange) flow |>
      OptionExt.return

    | S_assume { ekind = E_binop (O_log_and, e1, e2) } ->
      man.post (mk_assume e1 stmt.srange) flow >>$? fun () flow ->
      man.post (mk_assume e2 stmt.srange) flow |>
      OptionExt.return

    | S_assume { ekind = E_binop (O_log_or, e1, e2) } ->
      let post1 = man.post (mk_assume e1 stmt.srange) flow in
      let post2 = man.post (mk_assume e2 stmt.srange) flow in
      Post.join post1 post2 |>
      OptionExt.return

    | S_assume { ekind = E_unop (O_log_not, { ekind = E_unop (O_log_not, e) }) } ->
      man.post (mk_assume e stmt.srange) flow |>
      OptionExt.return

    | S_assume { ekind = E_constant (C_bool true) } ->
      Post.return flow |>
      OptionExt.return

    | S_assume { ekind = E_constant (C_bool false) } ->
      Post.return (Flow.bottom_from flow) |>
      OptionExt.return

    | S_assume(e) ->
      man.eval e flow >>$? fun e flow ->
      man.post ~route:Below (mk_assume e stmt.srange) flow |>
      OptionExt.return

    | S_block(block,local_vars) ->
      Some (
        let flow = List.fold_left (fun acc stmt -> man.exec stmt acc) flow block in
        let flow = List.fold_left (fun acc var -> man.exec (mk_remove_var var stmt.srange) acc) flow local_vars in
        Post.return flow
      )

    | S_if(cond, s1, s2) ->
      let then_flow = man.exec (mk_assume cond cond.erange) flow |>
                      man.exec s1
      in
      let else_flow = Flow.copy_ctx then_flow flow |>
                      man.exec (mk_assume (mk_not cond cond.erange) cond.erange) |>
                      man.exec s2
      in
      Flow.join man.lattice then_flow else_flow |>
      Post.return |>
      OptionExt.return

    | S_print ->
      Framework.Output.Factory.print (srange stmt) (Flow.print man.lattice.print) flow;
      Some (Post.return flow)

    | _ -> None

  let eval exp man flow = None

  let ask query man flow = None

end

let () =
  register_stateless_domain (module Domain)
