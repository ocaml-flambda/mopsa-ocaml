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

(** Extensible type of operators. *)


open Mopsa_utils
open Format

type operator = ..

(** Basic operators *)
type operator +=
  | O_eq         (** == *)
  | O_ne         (** != *)
  | O_lt         (** < *)
  | O_le         (** <= *)
  | O_gt         (** > *)
  | O_ge         (** >= *)

  | O_log_not    (** Logical negation *)
  | O_log_or     (** || *)
  | O_log_and    (** && *)
  | O_log_xor    (** xor *)

  | O_cast       (** cast operator *)


let operator_compare_chain = TypeExt.mk_compare_chain (fun o1 o2 ->
    compare o1 o2
  )

let operator_pp_chain = TypeExt.mk_print_chain (fun fmt op ->
    match op with
    | O_lt -> pp_print_string fmt "<"
    | O_le -> pp_print_string fmt "<="
    | O_gt -> pp_print_string fmt ">"
    | O_ge -> pp_print_string fmt ">="
    | O_eq -> pp_print_string fmt "=="
    | O_ne -> pp_print_string fmt "!="
    | O_log_or -> pp_print_string fmt "or"
    | O_log_and -> pp_print_string fmt "and"
    | O_log_not -> pp_print_string fmt "not"
    | O_log_xor -> pp_print_string fmt "xor"
    | O_cast -> pp_print_string fmt "cast"
    | _ -> Exceptions.panic "operator_pp_chain: unknown operator"
  )


let register_operator (info: operator TypeExt.info) : unit =
  TypeExt.register info operator_compare_chain operator_pp_chain

let register_operator_compare cmp = TypeExt.register_compare cmp operator_compare_chain

let register_operator_pp pp = TypeExt.register_print pp operator_pp_chain

let compare_operator o1 o2 = TypeExt.compare operator_compare_chain o1 o2

let pp_operator fmt operator = TypeExt.print operator_pp_chain fmt operator

let is_comparison_op = function
  | O_eq | O_ne | O_gt | O_ge | O_lt | O_le -> true
  | _ -> false

let is_logic_op = function
  | O_log_and | O_log_or | O_log_not | O_log_xor -> true
  | _ -> false

let negate_logic_op = function
  | O_log_and -> O_log_or
  | O_log_or  -> O_log_and
  | op -> Exceptions.panic "don't know how to negate operator %a" pp_operator op

let negate_comparison_op = function
  | O_eq -> O_ne
  | O_ne -> O_eq
  | O_lt -> O_ge
  | O_le -> O_gt
  | O_gt -> O_le
  | O_ge -> O_lt
  | op -> Exceptions.panic "don't know how to negate operator %a" pp_operator op
