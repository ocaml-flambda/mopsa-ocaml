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

(** Operators

    This module allows adding new operators to the extensible Mopsa AST.

    To add a new operator, first extend type [operator] with a new variant
    constructor. For instance,
    {[
      type operator += O_eq
    ]}
    adds a new constructor for an equality operator.

    After adding the new variant, register it by declaring a compare and
    a print function:
    {[
      let () = register_operator {
          compare = (fun next o1 o2 ->
              match o1, o2 with
              | O_eq, O_eq -> 0
              | _          -> next o1 o2
            );
          print = (fun next -> function
              | O_eq -> Format.pp_print_string fmt "=="
              | _    -> next fmt o
            );
        }
    ]}
    Note that the comparison function can be reduced in this cast to 
    [compare = (fun next -> next)] because the operator [O_eq] doesn't have
    a structure and the pervasive [compare] used by default is sufficient.

    Any registered constant can be compared and printed with functions
    [compare_constant] and [pp_constant].

 *)


open Mopsa_utils


type operator = ..
(** Extensible type of operators *)

val compare_operator : operator -> operator -> int
(** Total order between operators *)

val pp_operator : Format.formatter -> operator -> unit
(** Pretty-printer of operators *)


(****************************************************************************)
(**                            {1 Registration}                             *)
(****************************************************************************)

val register_operator : operator TypeExt.info -> unit
(** [register_operator info] registers a new operator by registering its 
    compare function [info.compare] and pretty-printer [info.print] *)

val register_operator_compare : operator TypeExt.compare -> unit
(** Register a comparison function for operators *)

val register_operator_pp : operator TypeExt.print -> unit
(** Register a pretty-printer for operators *)


(****************************************************************************)
(**                        {1 Some common operators}                        *)
(****************************************************************************)

(** Common operators *)
type operator +=
  | O_eq         (** equality == *)
  | O_ne         (** inequality != *)
  | O_lt         (** less than < *)
  | O_le         (** less or equal <= *)
  | O_gt         (** greater than > *)
  | O_ge         (** greater or equal >= *)
  | O_log_not    (** logical negation *)
  | O_log_or     (** logical disjunction || *)
  | O_log_and    (** logical conjunction && *)
  | O_cast       (** type cast *)

val is_comparison_op : operator -> bool
(** Test whether an operator is a comparison operator *)

val is_logic_op : operator -> bool
(** Test whether an is a logical operator *)
  
val negate_comparison_op : operator -> operator
(** Return the negation of a comparison operator *)
