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

(** Desugar non-scalar expressions in assignments and tests *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Zone

module Domain =
struct

  let name = "universal.iterators.scalar"
  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = { provides = [Z_u]; uses = [Z_u_num] };
    ieval = { provides = []; uses = [Z_u,Z_u_num] };
  }

  let init prog man flow = flow

  let desugar e man flow =
    man.eval e ~zone:(Z_u, Z_u_num) flow

  let exec zone stmt man flow =
    match skind stmt with
    | S_assign(x, e) when Core.Zone.eval_template x Z_u_num <> Keep ||
                          Core.Zone.eval_template e Z_u_num <> Keep
      ->
      desugar x man flow |>
      Option.return |> Option.lift @@
      post_eval man @@ fun x flow ->

      desugar e man flow |>
      post_eval man @@ fun e flow ->

      man.post ~zone:Z_u_num (mk_assign x e stmt.srange) flow

    | S_assume e when Core.Zone.eval_template e Z_u_num <> Keep ->
      desugar e man flow |>
      Option.return |> Option.lift @@
      post_eval man @@ fun e flow ->

      man.post ~zone:Z_u_num (mk_assume e stmt.srange) flow


    | _ -> None

  let eval zone exp man flow = None

  let ask query man flow = None

end

let () =
  register_domain (module Domain)
