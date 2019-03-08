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

(** Constant string abstraction of string values *)

open Mopsa
open Ast

module Value =
struct
  type t =
    | T | B
    | V of string

  let zone = Zone.Z_u_string

  type _ Query.query +=
    | Q_string : expr -> t Query.query

  include Framework.Core.Id.GenValueId(struct
      type typ = t
      let name = "universal.strings.value", "strings"
    end)

  let bottom = B
  let top = T
  let is_bottom = function
    | B -> true
    | _ -> false
  let is_top = function
    | T -> true
    | _ -> false
  let join annot u v = match u, v with
    | T, _ -> T
    | _, T -> T
    | B, _ -> v
    | _, B -> u
    | V u, V v when u = v -> V u
    | _ -> T
  let meet annot u v = match u, v with
    | T, _ -> v
    | _, T -> u
    | B, _ -> B
    | _, B -> B
    | V u, V v when u = v -> V u
    | _ -> B

  let subset u v = match u, v with
    | _, T -> true
    | B, _ -> true
    | V u, V v when u = v -> true
    | _ -> false

  let widen = join
  let print fmt = function
    | T -> Format.fprintf fmt "top"
    | B -> Format.fprintf fmt "bot"
    | V u -> Format.fprintf fmt "%s" u
  let of_constant _ = function
    | C_string s -> V s
    | _ -> T
  let unop _ op a = return T
  let binop _ op a1 a2 =
    return (
      match op, a1, a2 with
      | O_concat, B, _ -> B
      | O_concat, _, B -> B
      | O_concat, V u, V v -> V (u ^ v)
      | _ -> T
    )
  let filter _ _ _ = return T
  let bwd_unop _ _ _ _ = return T
  let bwd_binop _ _ _ _ _ = return (T, T)
  let compare _ _ _ _ _ = return (T, T)

  let ask : type r. r Query.query -> (expr -> t) -> r option =
    fun query eval ->
      match query with
      | Q_string e ->
        Some (eval e)
      | _ -> None


end

let () =
  register_value (module Value)
