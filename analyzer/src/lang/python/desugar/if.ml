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

(** Transformation of conditional expressions. *)

open Mopsa
open Universal.Ast
open Ast

module Domain =
  struct

    let name = "python.desugar.if"
    let debug fmt = Debug.debug ~channel:name fmt

    let interface = {
      iexec = {provides = [Zone.Z_py]; uses = [Zone.Z_py]};
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let init _ _ flow = flow

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_if(test, body, orelse) ->
         let tmp = mktmp () in
         let flow = man.exec
                      (mk_if
                         (Utils.mk_builtin_call "bool" [test] range)
                         (mk_assign (mk_var tmp (tag_range range "true branch lval")) body (tag_range range "true branch assign"))
                         (mk_assign (mk_var tmp (tag_range range "false branch lval")) orelse (tag_range range "false branch assign"))
                         range
                      ) flow
         in
         let exp' = {exp with ekind = E_var (tmp, STRONG)} in
         man.eval exp' flow |>
           Eval.add_cleaners [mk_remove_var tmp (tag_range range "cleaner")] |>
           Option.return

      | _ -> None


    let exec zone stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_py_if (test, sthen, selse) ->
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (Utils.mk_builtin_call "bool" [test] range) flow |>
        post_eval man (fun exp flow ->
            man.exec ~zone:Zone.Z_py (mk_if exp sthen selse range) flow |> Post.return
          )
        |> Option.return

      | _ -> None

    let ask _ _ _ = None

  end

let () =
  Framework.Core.Sig.Stateless.Domain.register_domain (module Domain)
