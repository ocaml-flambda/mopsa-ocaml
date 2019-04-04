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
open Ast


module Domain =
struct

  let name = "universal.iterators.intraproc"
  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = { provides = [Z_any]; uses = [] };
    ieval = { provides = []; uses = [] };
  }

  let init prog man flow = flow

  let exec zone stmt man flow =
    match skind stmt with
    | S_expression(e) ->
      Some (
        man.eval e flow |>
        Post.bind_eval man.lattice @@ fun e flow ->
        Post.return flow
      )

    | S_block(block) ->
      Some (
        List.fold_left (fun acc stmt -> man.exec ~zone stmt acc) flow block |>
        Post.return
      )

    | S_if(cond, s1, s2) ->
      let range = srange stmt in
      let block1 = mk_block [mk_assume cond range; s1] range in
      let block2 = mk_block [mk_assume (mk_not cond range) range; s2] range in
      let flows = Flow.map_list man.exec [block1; block2] flow in
      let flow' = Flow.join_list man.lattice flows in
      Some (Post.return flow')

    | S_print ->
      Debug.debug ~channel:"print" "%a@\n  @[%a@]"
        pp_position (srange stmt |> get_range_start)
        (Flow.print man.lattice) flow
      ;
      Some (Post.return flow)

    | _ -> None

  let eval zone exp man flow = None

  let ask query man flow = None

end

let () =
  Framework.Core.Sig.Stateless.Domain.register_domain (module Domain)
