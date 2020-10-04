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
open Sig.Abstraction.Stateless
open Ast
open MapExt
open Addr
open Universal.Ast

(** A dummy numerical domain for the type-only analysis. *)

module Domain =
  struct

    include GenStatelessDomainId(struct
                let name = "python.types.dummy_numeric"
              end)

    let checks = []

    let init _ _ flow = flow

    let exec stmt man flow =
      match skind stmt with
      | S_remove { ekind = E_var _ }
        | S_invalidate { ekind = E_var _ }
        | S_add { ekind = E_var _ }
        | S_project _
        | S_rename ({ ekind = E_var _ }, { ekind = E_var _ })
        | S_forget { ekind = E_var _ }
        | S_assign ({ ekind= E_var _ }, _)
        | S_expand ({ekind = E_var _ }, _)
        | S_fold ({ekind = E_var _}, _) ->
         Some (Post.return flow)

      | S_assume e when is_numeric_type @@ etyp e ->
         Some (Post.return flow)

      | _ -> None

    let eval exp man flow = None

    let ask : type r. ('a, r) query -> ('a, unit) man -> 'a flow -> r option =
      fun query man flow ->
      match query with
      | Q_avalue(e, Universal.Numeric.Common.V_int_interval _) ->
         Some (Nb Universal.Numeric.Common.I.minf_inf)
      | Q_avalue(e, Universal.Numeric.Common.V_float_interval) ->
         Some (Universal.Numeric.Common.F.infinities)
      | _ -> None

    let print_expr _ _ _ _ = ()

  end

let () = register_stateless_domain (module Domain)
