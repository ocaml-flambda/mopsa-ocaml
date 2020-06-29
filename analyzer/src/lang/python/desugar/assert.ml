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
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Zone

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.desugar.assert"
      end)

    let interface = {
      iexec = {provides = [Zone.Z_py]; uses = [Zone.Z_py]};
      ieval = {provides = []; uses = []}
    }

    let alarms = []

    let init _ _ flow = flow

    let exec zone stmt man flow =
      let range = srange stmt in
      match skind stmt with
      (* S⟦ assert(e, msg) ⟧ *)
      | S_py_assert (e, msg)->
         Flow.join
           man.lattice
           (man.exec (mk_assume e range) flow)
           (man.exec (mk_assume (mk_py_not e range) range) flow |>
              man.exec (Utils.mk_builtin_raise "AssertionError" range))
         |> Post.return
         |> OptionExt.return

      | _ -> None

    let eval _ _ _ _ = None


    let ask _ _ _ = None

end

let () =
  register_stateless_domain (module Domain)
