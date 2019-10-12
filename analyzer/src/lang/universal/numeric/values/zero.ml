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

(** Abstraction of zero and non-zero integer values. *)

open Mopsa
open Core.Sig.Value.Simplified
open Ast


module Value =
struct

  type v = Zero | NonZero

  type t =
    | TOP
    | BOT
    | ZERO
    | NON_ZERO

  include GenValueId(
    struct
        type nonrec t = t
        let name = "universal.numeric.values.zero"
        let display = "zero"
    end
    )

  let zones = [Zone.Z_u_num]

  let types = [T_int; T_bool]

  let bottom = BOT

  let top = TOP

  let is_bottom a = (a = BOT)

  let subset (a1:t) (a2:t) : bool =
    match a1, a2 with
    | _, TOP -> true
    | TOP,_ -> false
    | BOT, _ -> true
    | _, BOT -> false
    | ZERO, ZERO -> true
    | NON_ZERO, NON_ZERO -> true
    | _ -> false

  let join (a1:t) (a2:t) : t =
    match a1,a2 with
    | TOP,_ | _, TOP -> TOP
    | BOT,x | x,BOT -> x
    | ZERO, ZERO -> ZERO
    | NON_ZERO, NON_ZERO -> NON_ZERO
    | ZERO, NON_ZERO | NON_ZERO, ZERO -> TOP


  let meet (a1:t) (a2:t) : t =
    match a1,a2 with
    | TOP,x | x,TOP -> x
    | BOT,_ | _,BOT -> BOT
    | ZERO, ZERO -> ZERO
    | NON_ZERO, NON_ZERO -> NON_ZERO
    | ZERO, NON_ZERO | NON_ZERO, ZERO -> BOT

  let widen ctx (a1:t) (a2:t) : t = join a1 a2

  let print fmt (a:t) =
    match a with
    | TOP -> Format.fprintf fmt "⊤"
    | BOT -> Format.fprintf fmt "⊥"
    | ZERO -> Format.fprintf fmt "0"
    | NON_ZERO -> Format.fprintf fmt "≠ 0"

  let of_constant = function
    | C_int i when Z.equal i Z.zero -> ZERO

    | C_int i -> NON_ZERO

    | C_int_interval (i1,i2) when Z.equal i1 Z.zero &&
                                  Z.equal i1 Z.zero ->
      ZERO

    | C_int_interval (i1,i2) when Z.gt i1 Z.zero ||
                                  Z.lt i2 Z.zero ->
      NON_ZERO

    | _ -> TOP

  let unop op a =
    match op with
    | O_log_not -> begin match a with
        | TOP -> TOP
        | BOT -> BOT
        | ZERO -> NON_ZERO
        | NON_ZERO -> ZERO
      end
    | O_minus  -> a
    | O_plus  -> a
    | _ -> top

  let binop op a1 a2 =
    match op with
    | O_plus -> begin match a1, a2 with
        | BOT, _ | _, BOT -> BOT
        | TOP, _ | _, TOP -> TOP
        | ZERO, x | x, ZERO -> x
        | _ -> TOP
      end
    | _     -> top

  let filter a b =
    match a with
    | TOP -> TOP
    | BOT -> BOT
    | ZERO -> if b then BOT else ZERO
    | NON_ZERO -> if b then NON_ZERO else BOT

  let bwd_unop op abs rabs = default_bwd_unop op abs rabs

  let bwd_binop op a1 a2 r = default_bwd_binop op a1 a2 r

  let compare op a1 a2 r =
    let op = if r then op else negate_comparison_op op in
    match op with
    | O_eq ->
      let a = meet a1 a2 in
      a, a

    | O_ne | O_gt | O_lt ->
      begin match a1, a2 with
        | BOT, _ | _, BOT -> BOT, BOT
        | ZERO, ZERO -> BOT, BOT
        | ZERO, TOP -> ZERO, NON_ZERO
        | TOP, ZERO -> NON_ZERO, ZERO
        | _ -> a1, a2
      end

    | _ -> default_compare op a1 a2 r

  let ask q eval = None

  let refine channel v = Channel.return v

end

let () =
  register_value (module Value)
