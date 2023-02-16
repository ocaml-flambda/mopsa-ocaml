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
open Top


(** Compute symbolic boundaries of a quantified offset. *)
(* FIXME: works only for linear expressions *)
let rec bound offset quants : expr * expr =
  match ekind @@ get_orig_expr offset with
  | E_constant _ -> offset, offset

  | E_var (v, _) when is_forall_quantified_var v quants ->
    find_quantified_var_interval v quants

  | E_var _ ->
    offset, offset

  | E_unop (O_minus, e) ->
    let l, u = bound e quants in
    { offset with ekind = E_unop (O_minus, u)},
    { offset with ekind = E_unop (O_minus, l)}

  | E_binop (O_plus, e1, e2) ->
    let l1, u1 = bound e1 quants in
    let l2, u2 = bound e2 quants in
    { offset with ekind = E_binop (O_plus, l1, l2)},
    { offset with ekind = E_binop (O_plus, u1, u2)}

  | E_binop (O_minus, e1, e2) ->
    let l1, u1 = bound e1 quants in
    let l2, u2 = bound e2 quants in
    { offset with ekind = E_binop (O_minus, l1, u2)},
    { offset with ekind = E_binop (O_minus, u1, l2)}

  | E_binop (O_mult, e, ({ ekind = E_constant (C_int c) } as const))
  | E_binop (O_mult, ({ ekind = E_constant (C_int c) } as const), e) ->
    let l, u = bound e quants in
    if Z.geq c Z.zero then
      { offset with ekind = E_binop (O_mult, l, { const with ekind = E_constant (C_int c) })},
      { offset with ekind = E_binop (O_mult, u, { const with ekind = E_constant (C_int c) })}
    else
      { offset with ekind = E_binop (O_mult, u, { const with ekind = E_constant (C_int c) })},
      { offset with ekind = E_binop (O_mult, l, { const with ekind = E_constant (C_int c) })}

  | E_c_cast(e, xplct) ->
    let l, u = bound e quants in
    { offset with ekind = E_c_cast (l, xplct)},
    { offset with ekind = E_c_cast (u, xplct)}

  | _ -> panic_at offset.erange
           "can not compute symbolic bounds of non-linear expression %a"
           pp_expr offset



(** [is_aligned o n man flow] checks whether the value of an
      expression [o] is aligned w.r.t. size sz *)
let is_aligned e sz man flow =
  (sz = Z.one) || (is_c_expr_equals_z e Z.zero) ||
  (man.eval e flow ~translate:"Universal" |>
   Cases.for_all_result (fun ee flow ->
       let open Universal.Numeric.Common in
       let i , c = man.ask (mk_int_congr_interval_query ee) flow in
       match i with
       | Bot.Nb(I.B.Finite a, I.B.Finite b) when  a = b && Z.rem a sz = Z.zero -> true
       | _ -> Universal.Numeric.Common.C.included_bot c (Bot.Nb (sz,Z.zero))
     )
  )


(** Compute symbolic boundaries of offset / den *)
let bound_div (offset:expr) (den:Z.t) quants man flow : (expr * expr) with_top =
  let rec doit offset den  =
    let range = erange offset in
    match ekind @@ get_orig_expr offset with
    | E_constant (C_int c) when Z.rem c den = Z.zero ->
      let r =
        if den = Z.one then offset
        else if c = Z.zero then offset
        else div offset (mk_z den range) range in
      r, r

    | E_var (v, _) when is_forall_quantified_var v quants ->
      if den = Z.one then
        find_quantified_var_interval v quants
      else
        raise Found_TOP

    | E_var (v,_) ->
      if not (is_aligned offset den man flow) then raise Found_TOP;
      let r = if den = Z.one then offset else div offset (mk_z den range) range in
      r, r

    | E_unop (O_minus, e) ->
      let l, u = doit e den in
      { offset with ekind = E_unop (O_minus, u)},
      { offset with ekind = E_unop (O_minus, l)}

    | E_binop (O_plus, e1, e2) ->
      let l1, u1 = doit e1 den in
      let l2, u2 = doit e2 den in
      { offset with ekind = E_binop (O_plus, l1, l2)},
      { offset with ekind = E_binop (O_plus, u1, u2)}

    | E_binop (O_minus, e1, e2) ->
      let l1, u1 = doit e1 den in
      let l2, u2 = doit e2 den in
      { offset with ekind = E_binop (O_minus, l1, u2)},
      { offset with ekind = E_binop (O_minus, u1, l2)}

    | E_binop (O_mult, e1, e2) ->
      let e1, c, e2 = match e1, e2 with
        | { ekind = E_constant (C_int c) }, _ -> e1, c, e2
        | _, { ekind = E_constant (C_int c) } -> e2, c, e1
        | _ -> raise Found_TOP
      in
      let gcd = Z.gcd c den in
      let c, den = Z.div c gcd, Z.div den gcd in
      let l, u = doit e2 den in
      if c = Z.one then l, u
      else if c >= Z.zero then
        { offset with ekind = E_binop (O_mult, l, { e1 with ekind = E_constant (C_int c) })},
        { offset with ekind = E_binop (O_mult, u, { e1 with ekind = E_constant (C_int c) })}
      else
        { offset with ekind = E_binop (O_mult, u, { e1 with ekind = E_constant (C_int c) })},
        { offset with ekind = E_binop (O_mult, l, { e1 with ekind = E_constant (C_int c) })}

    | E_binop (O_div, e, ({ ekind = E_constant (C_int c) })) ->
      doit e (Z.mul den c)

    | E_c_cast(e, xplct) ->
      let l, u = doit e den in
      { offset with ekind = E_c_cast (l, xplct)},
      { offset with ekind = E_c_cast (u, xplct)}

    | _ ->
      (*
      panic_at offset.erange
        "can not compute symbolic bounds of non-linear expression %a / %a"
        pp_expr offset Z.pp_print den
      *)
      raise Found_TOP
  in
  retop (doit offset) den

(*
let bound_div offset den man flow =
  Format.printf "bound_div %a / %a@." pp_expr offset Z.pp_print den;
  let l,u = bound_div offset den man flow in
  Format.printf "  [%a,@.   %a]@." pp_expr l pp_expr u;
  l, u
*)
