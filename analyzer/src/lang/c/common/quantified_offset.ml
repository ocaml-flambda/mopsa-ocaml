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
open Ast


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

  | E_c_cast(e, xplct) ->
    let l, u = bound e in
    { offset with ekind = E_c_cast (l, xplct)},
    { offset with ekind = E_c_cast (u, xplct)}

  | E_binop (O_div, e, ({ ekind = E_constant (C_int c) })) ->
    bound_div e c

  | _ -> panic_at offset.erange
           "can not compute symbolic bounds of non-linear expression %a"
           pp_expr offset


and bound_div offset den =
  let range = erange offset in
  match ekind offset with
  | E_constant _ ->
    div offset (mk_z den range) range,
    div offset (mk_z den range) range

  | E_var (v, _) ->
    div offset (mk_z den range) range,
    div offset (mk_z den range) range

  | E_stub_quantified(FORALL, _, S_interval(l, u)) ->
    div l (mk_z den range) range,
    div u (mk_z den range) range

  | E_unop (O_minus, e) ->
    let l, u = bound_div e den in
    { offset with ekind = E_unop (O_minus, u)},
    { offset with ekind = E_unop (O_minus, l)}

  | E_binop (O_plus, e1, e2) ->
    let l1, u1 = bound_div e1 den in
    let l2, u2 = bound_div e2 den in
    { offset with ekind = E_binop (O_plus, l1, l2)},
    { offset with ekind = E_binop (O_plus, u1, u2)}

  | E_binop (O_minus, e1, e2) ->
    let l1, u1 = bound_div e1 den in
    let l2, u2 = bound_div e2 den in
    { offset with ekind = E_binop (O_minus, l1, u2)},
    { offset with ekind = E_binop (O_minus, u1, l2)}

  | E_binop (O_mult, e, ({ ekind = E_constant (C_int c) } as const))
  | E_binop (O_mult, ({ ekind = E_constant (C_int c) } as const), e)
    when Z.rem c den = Z.zero ->
    let c = Z.div c den in
    let l, u = bound e in
    if Z.geq c Z.zero then
      { offset with ekind = E_binop (O_mult, l, { const with ekind = E_constant (C_int c) })},
      { offset with ekind = E_binop (O_mult, u, { const with ekind = E_constant (C_int c) })}
    else
      { offset with ekind = E_binop (O_mult, u, { const with ekind = E_constant (C_int c) })},
      { offset with ekind = E_binop (O_mult, l, { const with ekind = E_constant (C_int c) })}

  | E_c_cast(e, xplct) ->
    let l, u = bound_div e den in
    { offset with ekind = E_c_cast (l, xplct)},
    { offset with ekind = E_c_cast (u, xplct)}

  | _ -> panic_at offset.erange
      "can not compute symbolic bounds of non-linear expression %a"
      pp_expr offset

(*
let bound o =
  let r = bound o in
  Format.printf "bound %a -> [%a, %a]@." pp_expr o pp_expr (fst r) pp_expr (snd r);
  r
*)
