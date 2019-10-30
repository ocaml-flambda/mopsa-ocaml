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
open Sig.Domain.Stateless
open Ast


module Domain =
struct

  include GenStatelessDomainId(
    struct
      let name = "universal.iterators.intraproc"
    end
    )

  let interface = {
    iexec = { provides = [Z_any]; uses = [] };
    ieval = { provides = []; uses = [] };
  }

  let init prog man flow = flow

  let exec zone stmt man flow =
    match skind stmt with
    | S_expression(e) when is_universal_type e.etyp || e.etyp = T_any ->
      Some (
        man.eval e flow >>$ fun e flow ->
        Post.return flow
      )

    | S_block(block) ->
      Some (
        List.fold_left (fun acc stmt -> man.exec ~zone stmt acc) flow block |>
        Post.return
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
      Option.return

    | S_print ->
      Framework.Output.Factory.print (srange stmt) (Flow.print man.lattice.print) flow;
      Some (Post.return flow)

    | _ -> None

  let eval zone exp man flow = None

  let ask query man flow = None

end

let () =
  register_domain (module Domain)
