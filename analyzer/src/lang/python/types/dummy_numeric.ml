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
open Sig.Abstraction.Simplified_value
open Ast
open MapExt
open Addr
open Universal.Ast
open Bot

(** A dummy numerical domain for the type-only analysis. *)

module Value =
  struct

  type t = unit with_bot

  include GenValueId(
    struct
        type nonrec t = t
        let name = "python.types.dummy_numeric"
        let display = "dummy_numeric"
    end
    )

  let accept_type = function
    | T_int | T_bool | T_float _ -> true
    | _ -> false

  let bottom = BOT

  let top = Nb ()

  let is_bottom = function
    | BOT -> true
    | _   -> false

  let subset (a1:t) (a2:t) : bool =
    bot_included (fun _ _ -> true) a1 a2

  let join (a1:t) (a2:t) : t =
    bot_neutral2 (fun _ _ -> ()) a1 a2

  let meet (a1:t) (a2:t) : t =
    bot_absorb2 (fun _ _ -> top) a1 a2

  let widen ctx (a1:t) (a2:t) : t = join a1 a2

  let print printer (a:t) : unit =
    match a with
    | BOT   -> pp_string printer "⊥"
    | Nb () -> pp_string printer "⊤:num"

  include DefaultValueFunctions

  let constant c t = top

  let unop op t a tr = top

  let binop op t1 a1 t2 a2 tr = top

  let avalue : type r. r avalue_kind -> t -> r option =
    fun avk a ->
    match avk with
    | Universal.Numeric.Common.V_float_interval _
    | Universal.Numeric.Common.V_int_interval _
    | Universal.Numeric.Common.V_int_congr_interval ->
      Some (top_avalue avk)

    | _ -> None
end

let () = register_simplified_value_abstraction (module Value)
