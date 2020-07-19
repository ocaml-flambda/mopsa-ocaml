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

(** Fallback transfer functions *)

open Mopsa
open Framework.Abstraction.Sig.Domain.Stateless
open Universal.Ast
open Ast
open Alarms


module Domain =
struct

  include GenStatelessDomainId(struct let name = "stubs.iterators.fallback" end)
  let dependencies = []
  let alarms = []

  let init prog man flow = flow
  let eval exp man flow = None
  let ask query man flow = None

  let exec_quantifers cond_to_stmt quants cond range man flow =
    let rec iter = function
      | [] -> cond_to_stmt cond range
      | (EXISTS,_,_)::tl -> iter tl
      | (FORALL,i,S_interval(a,b))::tl ->
        let ii = mk_var i range in
        mk_block [ mk_assign ii a range;
                   mk_while (le ii b range) (
                     mk_block [ iter tl;
                                mk_assign ii (add ii one range) range ] range
                   ) range ] range
      | (_,_,S_resource _)::tl -> assert false
    in
    let stmt = iter quants in
    man.post stmt flow


  let exec_requires cond range man flow =
    assume cond
      ~fthen:(fun flow ->
          Post.return flow
        )
      ~felse:(fun flow ->
          raise_stub_invalid_requires cond range man flow |>
          Post.return
        )
      man flow


  let exec stmt man flow =
    match skind stmt with
    | S_assume({ekind = E_stub_quantified_formula(quants,cond)}) ->
      exec_quantifers mk_assume quants cond stmt.srange man flow |>
      OptionExt.return

    | S_stub_requires({ekind = E_stub_quantified_formula(quants,cond)}) ->
      exec_quantifers mk_stub_requires quants cond stmt.srange man flow |>
      OptionExt.return

    | S_stub_requires(cond) ->
      exec_requires cond stmt.srange man flow |>
      OptionExt.return

    | _ -> None
end

let () =
  register_stateless_domain (module Domain)
