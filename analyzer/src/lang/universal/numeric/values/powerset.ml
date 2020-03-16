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

(** Finite powerset of numeric constants *)

open Mopsa
open Core.Sig.Value.Simplified
open Ast
open Zone
open Top


module Value =
struct

  module Powerset = Framework.Lattices.Powerset.Make(struct
      type t = Z.t
      let compare = Z.compare
      let print = Z.pp_print
    end)

  include Powerset


  include GenValueId(
    struct
        type nonrec t = t
        let name = "universal.numeric.values.powerset"
        let display = "zero"
    end
    )

  let zones = [Z_u_num]

  let mem_type = function T_int | T_bool -> true | _ -> false

  let constant = function
    | C_int n -> singleton n
    | _ -> TOP

  let unop op a =
    match op with
    | O_log_not -> map Z.lognot a
    | O_minus  -> map Z.neg a
    | O_plus  -> a
    | _ -> top

  let binop op a1 a2 =
    if is_bottom a1 || is_bottom a2 then bottom else
    if is_top a1 || is_top a2 then top
    else
      let doit f =
        fold
          (fun n1 acc ->
             fold
               (fun n2 acc ->
                  add (f n1 n2) acc
               ) a2 acc
          ) a1 empty
      in
      let doitn f =
        try fold
          (fun n1 acc ->
             fold
               (fun n2 acc ->
                  add (f n1 (Z.to_int n2)) acc
               ) a2 acc
          ) a1 empty
        with _ -> top
      in
      match op with
      | O_plus -> doit Z.add
      | O_minus -> doit Z.sub
      | O_mult -> doit Z.mul
      | O_div -> doit Z.div
      | O_mod -> doit Z.(mod)
      | O_pow -> doitn Z.pow
      | O_bit_and -> doit Z.(land)
      | O_bit_or -> doit Z.(lor)
      | O_bit_xor -> doit Z.(lxor)
      | O_bit_lshift -> doitn Z.shift_left
      | O_bit_rshift -> doitn Z.shift_right
      | _     -> top

  let filter a b =
    if b then remove Z.zero a
    else meet a (singleton Z.zero)

  let bwd_unop op abs rabs = default_bwd_unop op abs rabs

  let bwd_binop op a1 a2 r = default_bwd_binop op a1 a2 r

  let predicate op x r = default_predicate op x r

  let compare op a1 a2 r =
    let op = if r then op else negate_comparison_op op in
    debug "compare %a: a1 = %a, a2 = %a" pp_operator op print a1 print a2;
    match op with
    | O_eq -> let a = meet a1 a2 in a,a
    | O_ne ->
      if is_top a1 || is_top a2 then a1, a2
      else
        let a1 = if is_singleton a2 then diff a1 a2 else a1 in
        let a2 = if is_singleton a1 then diff a2 a1 else a2 in
        a1,a2
    | O_le | O_lt | O_ge | O_gt ->
      if is_top a1 || is_top a2 then a1, a2 else
      let f1,f2 = match op with
        | O_le -> Z.((<=)), Z.((>=))
        | O_lt -> Z.((<)), Z.((>))
        | O_ge -> Z.((>=)), Z.((<=))
        | O_gt -> Z.((>)), Z.((<))
        | _ -> assert false
      in
      let a1 = Powerset.filter (fun n -> exists (f1 n) a2) a1 in
      let a2 = Powerset.filter (fun n -> exists (f2 n) a1) a2 in
      a1,a2
    | _ -> default_compare op a1 a2 r


end

let () =
  register_value (module Value)
