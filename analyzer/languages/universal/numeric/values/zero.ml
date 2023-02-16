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
open Sig.Abstraction.Simplified_value
open Ast


module Value =
struct

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

  let accept_type = function
    | T_int | T_bool -> true
    | _ -> false

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

  let print printer (a:t) =
    match a with
    | TOP -> pp_string printer "⊤"
    | BOT -> pp_string printer "⊥"
    | ZERO -> pp_string printer "0"
    | NON_ZERO -> pp_string printer "≠ 0"

  include DefaultValueFunctions

  let constant c t =
    match c with
    | C_bool true -> NON_ZERO
    | C_bool false -> ZERO
    | C_int i when Z.equal i Z.zero -> ZERO
    | C_int i -> NON_ZERO
    | C_int_interval (i1,i2) when ItvUtils.IntBound.is_zero i1 &&
                                  ItvUtils.IntBound.is_zero i2 ->
      ZERO
    | C_int_interval (i1,i2) when ItvUtils.IntBound.is_positive_strict i1 ||
                                  ItvUtils.IntBound.is_negative_strict i2 ->
      NON_ZERO
    | _ -> TOP

  let unop op t a tr =
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

  let binop op t1 a1 t2 a2 tr =
    match op with
    | O_plus | O_minus ->
      begin match a1, a2 with
        | BOT, _ | _, BOT -> BOT
        | TOP, _ | _, TOP -> TOP
        | ZERO, x | x, ZERO -> x
        | _ -> TOP
      end

    | O_mult ->
      begin match a1, a2 with
        | BOT, _ | _, BOT -> BOT
        | TOP, _ | _, TOP -> TOP
        | ZERO, x | x, ZERO -> ZERO
        | _ -> TOP
      end
    | _     -> top

  let filter b t a =
    match a with
    | TOP -> TOP
    | BOT -> BOT
    | ZERO -> if b then BOT else a
    | NON_ZERO -> if b then a else BOT

  let compare op b t1 a1 t2 a2 =
    let op = if b then op else negate_comparison_op op in
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

    | _ -> default_compare op b t1 a1 t2 a2

end

let () =
  register_simplified_value_abstraction (module Value)
