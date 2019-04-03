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

(** Initialize variable with stubs. *)

open Mopsa
open Ast
open Zone

(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  let name = "c.desugar.stub_init"
  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = [Z_c]; uses = []};
    ieval = {provides = []; uses = []};
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ _ =
    None


  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration ({ vkind = V_c {var_init = Some (C_init_stub stub); var_range} } as v)->
      let stmt' = Universal.Ast.mk_block [
          mk_add_var v stmt.srange;
          Stubs.Ast.mk_stub_init v stub var_range
        ] stmt.srange
      in
      man.exec stmt' flow |>
      Post.return |>
      Option.return

    | _ -> None

  let eval _ _ _ _  = None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Simplified.Stateless.register_domain (module Domain)
