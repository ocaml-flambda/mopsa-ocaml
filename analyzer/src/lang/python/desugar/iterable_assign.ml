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
open Sig.Abstraction.Stateless
open Addr
open Ast
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.desugar.iterable_assign"
      end)

    let checks = []

    let init _ _ flow = flow

    let rec exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_assign({ekind = E_py_tuple(el)}, exp)
      | S_assign({ekind = E_py_list(el)}, exp) ->
        debug "iterable assign@\n";
        assign_iter man el exp range flow

      | _ -> None

    and assign_iter man el exp range flow =
      let tmp = mktmp ~typ:(T_py None) () in
      let assign_iterable = mk_assign (mk_var tmp range) (Utils.mk_builtin_call "iter" [exp] range) range in
      let stmtl =
        List.fold_left (fun acc e ->
            mk_assign
              e
              (Utils.mk_builtin_call "next" [mk_var tmp range] range)
              (tag_range range "next assign")
            :: acc
          ) [] el |>
          List.rev
      in
      let block = mk_block stmtl (tag_range range "next assign block") in
      let stmt_itera =
        mk_try
          block
          [mk_except
             (Some (mk_py_object (find_builtin "StopIteration") (tag_range range "stop iter")))
             None
             (Utils.mk_builtin_raise_msg "ValueError" "not enough values to unpack" (tag_range range "error raise"))
          ]
          (mk_nop (tag_range range "empty try else"))
          (mk_nop (tag_range range "empty try finally"))
          (tag_range range "try next")
      in
      let stmt = mk_block [assign_iterable; stmt_itera] range in
      man.exec stmt flow >>%
      exec_cleaner (mk_remove_var tmp range) man >>%
      Post.return |>
      OptionExt.return


    let eval _ _ _ = None
    let ask _ _ _ = None
    let print_expr _ _ _ _ = ()

  end

let () =
  register_stateless_domain (module Domain)
