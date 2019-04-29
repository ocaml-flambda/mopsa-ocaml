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

(** Desugar conditional expressions `cond?e1:e2`. *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Zone

(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  let name = "c.desugar.cond_expr"
  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = []; uses = []};
    ieval = {provides = [Z_c, Z_c_low_level]; uses = [Z_c, Z_c_low_level]};
  }


  (** Initialization *)
  (** ============== *)

  let init _ _ flow = flow


  (** Post-condition computation *)
  (** ========================== *)

  let exec zone stmt man flow = None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow  =
    match ekind exp with
    | E_c_conditional(cond, e1, e2) ->
      assume_eval cond
        ~fthen:(fun flow ->
            man.eval ~zone:(Z_c, Z_c_low_level) e1 flow
          )
        ~felse:(fun flow ->
            man.eval ~zone:(Z_c, Z_c_low_level) e2 flow
          )
        man flow |>
      Option.return

    | _ -> None


  (** Query handler *)
  (** ============= *)

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
