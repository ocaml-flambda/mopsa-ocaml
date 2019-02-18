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

(** Handling of assert statements. *)

open Mopsa
open Universal.Ast
open Ast
open Zone

module Domain =
  struct

    let name = "python.desugar.assert"
    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = [Zone.Z_py]; import = []}
    let eval_interface = {export = []; import = [Zone.Z_py, Zone.Z_py]}

    let init _ _ flow = OptionExt.return flow

    let exec zone stmt man flow =
      let range = srange stmt in
      match skind stmt with
      (* S⟦ assert(e, msg) ⟧ *)
      | S_py_assert ({ekind = E_constant (C_bool true)}, msg)->
         Post.of_flow flow |> OptionExt.return

      | S_py_assert ({ekind = E_constant (C_bool false)}, msg)->
         man.exec (Utils.mk_builtin_raise "AssertionError" range) flow |> Post.of_flow |> OptionExt.return

      | S_py_assert (e, msg)->
         man.eval e flow |>
           Post.bind man @@
             (fun e flow ->
               let ok_case = man.exec (mk_assume e (tag_range range "safe case assume")) flow in

               let fail_case =
                 debug "checking fail";
                 let flow = man.exec (mk_assume (mk_not e e.erange) (tag_range range "fail case assume")) flow in
                 if Flow.is_bottom man flow then
                   let _ = debug "no fail" in
                   Flow.bottom (Flow.get_all_annot flow)
                 else
                   man.exec (
                       Utils.mk_builtin_raise "AssertionError" (tag_range range "fail case raise")
                     ) flow
               in
               Flow.join man ok_case fail_case
               |> Post.of_flow
             )
         |> OptionExt.return

      | _ -> None

    let eval _ _ _ _ = None


    let ask _ _ _ = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
