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

open Mopsa
open Universal.Ast
open Stubs.Ast


(** Compute symbolic boundaries of a quantified offset. *)
(* FIXME: works only for linear expressions *)
let rec bound offset : expr * expr =
  match ekind offset with
  | E_constant _ -> offset, offset

  | E_var (v, _) -> offset, offset

  | E_stub_quantified(FORALL, _, S_interval(l, u)) -> l, u

  | E_unop (O_minus, e) ->
    let l, u = bound e in
    { offset with ekind = E_unop (O_minus, u)},
    { offset with ekind = E_unop (O_minus, l)}

  | E_binop (O_plus, e1, e2) ->
    let l1, u1 = bound e1 in
    let l2, u2 = bound e2 in
    { offset with ekind = E_binop (O_plus, l1, l2)},
    { offset with ekind = E_binop (O_plus, u1, u2)}

  | E_binop (O_minus, e1, e2) ->
    let l1, u1 = bound e1 in
    let l2, u2 = bound e2 in
    { offset with ekind = E_binop (O_minus, l1, u2)},
    { offset with ekind = E_binop (O_minus, u1, l2)}

  | E_binop (O_mult, e, ({ ekind = E_constant (C_int c) } as const))
  | E_binop (O_mult, ({ ekind = E_constant (C_int c) } as const), e) ->
    let l, u = bound e in
    if Z.geq c Z.zero then
      { offset with ekind = E_binop (O_mult, l, { const with ekind = E_constant (C_int c) })},
      { offset with ekind = E_binop (O_mult, u, { const with ekind = E_constant (C_int c) })}
    else
      { offset with ekind = E_binop (O_mult, u, { const with ekind = E_constant (C_int c) })},
      { offset with ekind = E_binop (O_mult, l, { const with ekind = E_constant (C_int c) })}


  | _ -> panic ~loc:__LOC__
           "Quantified_offset.bound called on a non linear expression %a"
           pp_expr offset
