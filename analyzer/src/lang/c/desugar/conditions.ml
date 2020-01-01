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

(** Desugar conditional expressions. *)

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

  include GenStatelessDomainId(struct
      let name = "c.desugar.conditions"
    end)


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = []; uses = [Z_c_scalar]};
    ieval = {provides = [Z_c, Z_c_low_level]; uses = [Z_c, Z_c_low_level; Z_c, Z_c_scalar]};
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
    match ekind exp with
    | E_c_conditional(cond, e1, e2) ->
      man.eval cond ~zone:(Z_c,Z_c_scalar) flow >>$? fun cond flow ->
      assume cond
        ~fthen:(fun flow ->
            man.eval ~zone:(Z_c, Z_c_low_level) e1 flow
          )
        ~felse:(fun flow ->
            man.eval ~zone:(Z_c, Z_c_low_level) e2 flow
          )
        ~zone:Z_c_scalar man flow |>
      Option.return

    | E_binop(O_c_and, e1, e2) ->
      man.eval e1 ~zone:(Z_c,Z_c_scalar) flow >>$? fun e1 flow ->
      assume e1
        ~fthen:(fun flow ->
            man.eval ~zone:(Z_c, Z_c_low_level) e2 flow
          )
        ~felse:(fun flow ->
            Eval.singleton (Universal.Ast.mk_zero ~typ:exp.etyp exp.erange) flow
          )
        ~zone:Z_c_scalar
        man flow |>
      Option.return

    | E_binop(O_c_or, e1, e2) ->
      man.eval e1 ~zone:(Z_c,Z_c_scalar) flow >>$? fun e1 flow ->
      assume e1
        ~fthen:(fun flow ->
            Eval.singleton (Universal.Ast.mk_one ~typ:exp.etyp exp.erange) flow
          )
        ~felse:(fun flow ->
            man.eval ~zone:(Z_c, Z_c_low_level) e2 flow
          )
         ~zone:Z_c_scalar man flow |>
      Option.return

    | _ -> None


  (** Query handler *)
  (** ============= *)

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
