(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2020 The MOPSA Project.                               *)
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

open Mopsa
open Sig.Domain.Stateless
open Ast
open MapExt
open Addr
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
                let name = "python.types.dummy_numeric"
              end)

    let alarms = []


    let interface = {
        iexec = {provides = [Universal.Zone.Z_u_float; Universal.Zone.Z_u_int]; uses = []};
        ieval = {provides = []; uses = []}
                  (* Universal.Zone.Z_u, Universal.Zone.Z_u_float; Universal.Zone.Z_u, Universal.Zone.Z_u_int]; uses = []} *)
      }

    let init _ _ flow = flow

    let exec zone stmt man flow =
      match skind stmt with
      | S_remove { ekind = E_var _ }
        | S_invalidate { ekind = E_var _ }
        | S_add { ekind = E_var _ }
        | S_project _
        | S_rename ({ ekind = E_var _ }, { ekind = E_var _ })
        | S_forget { ekind = E_var _ }
        | S_assign ({ ekind= E_var _ }, _)
        | S_expand ({ekind = E_var _ }, _)
        | S_fold ({ekind = E_var _}, _)
        | S_assume _ ->
         Some (Post.return flow)

      | _ -> None

    let eval _ _ _ _ = None

    let ask : type r. r query -> ('a, unit) Core.Sig.Domain.Stateless.man -> 'a flow -> r option =
      fun query man flow ->
      match query with
      | Universal.Numeric.Common.Q_int_interval e ->
         Some (Nb Universal.Numeric.Common.I.minf_inf)
      | Universal.Numeric.Common.Q_float_interval e ->
         Some (Universal.Numeric.Common.F.infinities)
      | _ -> None


  end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
