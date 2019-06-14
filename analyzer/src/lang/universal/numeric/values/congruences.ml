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

(** Congruence abstraction of integer values. *)

open Mopsa
open Core.Sig.Value.Simplified
open Ast
open Bot


module Value =
struct

  module C = CongUtils.IntCong

  type v = C.t
  type t = v with_bot

  include GenValueId(
    struct
        type nonrec t = t
        let name = "universal.numeric.values.congruences"
        let display = "congruences"
    end
    )

  let zones = [Zone.Z_u_num]

  let types = [T_int; T_bool]

  let bottom = BOT

  let top = Nb (C.minf_inf)

  let is_bottom abs =
    bot_dfl1 true (fun itv -> not (C.is_valid itv)) abs

  let subset (a1:t) (a2:t) : bool = C.included_bot a1 a2

  let join (a1:t) (a2:t) : t = C.join_bot a1 a2

  let meet (a1:t) (a2:t) : t = C.meet_bot a1 a2

  let widen (a1:t) (a2:t) : t = join a1 a2

  let print fmt (a:t) = C.fprint_bot fmt a

  let of_constant = function
    | C_int i -> Nb (C.cst i)

    | C_int_interval (i1,i2) -> Nb (C.of_range i1 i2)

    | _ -> top

  let unop op a =
    match op with
    | O_log_not -> bot_lift1 C.log_not a
    | O_minus  -> bot_lift1 C.neg a
    | O_plus  -> a
    | _ -> top

  let binop op a1 a2 =
    match op with
    | O_plus   -> bot_lift2 C.add a1 a2
    | O_minus  -> bot_lift2 C.sub a1 a2
    | O_mult   -> bot_lift2 C.mul a1 a2
    | O_div    -> bot_absorb2 C.div a1 a2
    | O_log_or   -> bot_lift2 C.log_or a1 a2
    | O_log_and  -> bot_lift2 C.log_and a1 a2
    | O_mod    -> bot_absorb2 C.rem a1 a2
    | O_bit_rshift -> bot_absorb2 C.shift_right a1 a2
    | O_bit_lshift -> bot_absorb2 C.shift_left a1 a2
    | _     -> top

  let filter a b =
    if b then bot_absorb1 C.meet_nonzero a
    else bot_absorb1 C.meet_zero a

  let bwd_unop op abs rabs =
    try
      let a, r = bot_to_exn abs, bot_to_exn rabs in
      let aa = match op with
        | O_log_not -> assert false
        | O_minus  -> bot_to_exn (C.bwd_neg a r)
        | _ -> a
      in
      Nb aa
    with Found_BOT ->
      bottom

  let bwd_binop op a1 a2 r =
    try
      let a1, a2, r = bot_to_exn a1, bot_to_exn a2, bot_to_exn r in
      let aa1, aa2 =
        match op with
        | O_plus   -> bot_to_exn (C.bwd_add a1 a2 r)
        | O_minus  -> bot_to_exn (C.bwd_sub a1 a2 r)
        | O_mult   -> bot_to_exn (C.bwd_mul a1 a2 r)
        | O_div    -> bot_to_exn (C.bwd_div a1 a2 r)
        | O_mod    -> bot_to_exn (C.bwd_rem a1 a2 r)
        | O_bit_rshift -> bot_to_exn (C.bwd_shift_right a1 a2 r)
        | O_bit_lshift -> bot_to_exn (C.bwd_shift_left a1 a2 r)
        | _ -> a1, a2
      in
      Nb aa1, Nb aa2
    with Found_BOT ->
      bottom, bottom

  let compare op a1 a2 r =
    try
      let a1, a2 = bot_to_exn a1, bot_to_exn a2 in
      let op = if r then op else negate_comparison op in
      let aa1, aa2 =
        match op with
        | O_eq -> bot_to_exn (C.filter_eq a1 a2)
        | O_ne -> bot_to_exn (C.filter_neq a1 a2)
        | O_lt -> bot_to_exn (C.filter_lt a1 a2)
        | O_gt -> bot_to_exn (C.filter_gt a1 a2)
        | O_le -> bot_to_exn (C.filter_leq a1 a2)
        | O_ge -> bot_to_exn (C.filter_geq a1 a2)
        | _ -> a1, a2
      in
      Nb aa1, Nb aa2
    with Found_BOT ->
      bottom, bottom


  let ask : type r. r query -> (expr -> t) -> r option =
    fun query eval ->
      match query with
      | Common.Q_int_congr_interval e ->
        let c = eval e in
        let ret =
          match c with
          | BOT -> Intervals.Integer.Value.bottom, C.minf_inf
          | Nb cc -> Intervals.Integer.Value.top, cc
        in
        Option.return ret

      | _ -> None


  let refine channel v = Channel.return v

end

let () =
  register_value (module Value)
