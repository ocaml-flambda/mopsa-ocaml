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

(** Desugar constant expressions. *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Zone


(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.desugar.constants"
    end)


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = []; uses = []};
    ieval = {provides = [Z_c, Z_c_low_level]; uses = []};
  }

  let alarms = []

  (** Initialization *)
  (** ============== *)

  let init _ _ flow = flow


  (** Post-condition computation *)
  (** ========================== *)

  let exec zone stmt man flow = None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow  =
    match c_expr_to_z exp with
    | None   -> None
    | Some z ->
      Eval.singleton (mk_z z ~typ:exp.etyp exp.erange) flow |>
      OptionExt.return


  (** Query handler *)
  (** ============= *)

  let ask _ _ _  = None

end

let () =
  register_stateless_domain (module Domain)
