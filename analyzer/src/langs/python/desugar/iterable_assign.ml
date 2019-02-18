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

(** Assignments from iterables. *)

open Mopsa
open Addr
open Ast
open Universal.Ast

module Domain =
  struct

    let name = "python.desugar.iterable_assign"
    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = [Zone.Z_py]; import = [Zone.Z_py]}
    let eval_interface = {export = []; import = [Zone.Z_py, Zone.Z_py_obj]}

    let init _ _ flow = Some flow

    let rec exec zone stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_assign({ekind = E_py_tuple(el)}, exp)
        | S_assign({ekind = E_py_list(el)}, exp) ->
         man.eval (Utils.mk_builtin_call "iter" [exp] range) flow |>
           Post.bind man
             (fun iter flow ->
               assign_iter man el iter range flow
               |> Post.of_flow
             )
         |> OptionExt.return

      | _ -> None

    and assign_iter man el iter range flow =
      let stmtl =
        List.fold_left (fun acc e ->
            mk_assign
              e
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
             (Some (mk_py_object (find_builtin "StopIteration") (tag_range range "stop iter")))
             None
             (Utils.mk_builtin_raise "ValueError" (tag_range range "error raise"))
          ]
          (mk_nop (tag_range range "empty try else"))
          (mk_nop (tag_range range "empty try finally"))
          (tag_range range "try next")
      in
      man.exec stmt flow


    let eval _ _ _ _ = None
    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
