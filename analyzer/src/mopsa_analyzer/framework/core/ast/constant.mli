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

(** Constant

    This module allows adding new constants to the extensible Mopsa AST.

    To add a new constant, first extend type [constant] with a new variant
    constructor. For instance,
    {[
      type constant += C_int of int
    ]}
    adds a new constructor for integer constants.

    The new constant needs to be registered by declaring a comparison and
    pretty-printing functions. For the previous example, the registration is
    as follows:
    {[
      let () = register_constant {
          compare = (fun next c1 c2 ->
              match c1, c2 with
              | C_int n1, C_int n2 -> compare n1 n2
              | _ -> next n1 n2
            );
          print = (fun next fmt c ->
              match c with
              | C_int n -> Format.pp_print_int fmt n
              | _ -> next fmt c
            );
        }
    ]}

    Any registered constant can be compared and printed with functions
    [compare_constant] and [pp_constant].
*)


open Mopsa_utils


type constant = ..
(** Extensible constants *)

val compare_constant : constant -> constant -> int
(** [compare_constant c1 c2] provides a total order between any pair [c1] and
    [c2] of registered constants *) 

val pp_constant : Format.formatter -> constant -> unit
(** [pp_constant fmt c] pretty-prints a registered constant [c] with formatter
    [fmt]*)


(****************************************************************************)
(**                            {1 Registration}                             *)
(****************************************************************************)

val register_constant : constant TypeExt.info -> unit
(** [register_constant info] registers a new constant with a comparison
    function [info.compare] and a pretty-printer [info.print]
*)

val register_constant_compare : constant TypeExt.compare -> unit
(** [register_constant_compare compare] registers a new comparison function
    for constants *)

val register_constant_pp : constant TypeExt.print -> unit
(** [register_constant_compare compare] registers a new pretty-printer for
    constants *)


(****************************************************************************)
(**                         {1 Common constants}                            *)
(****************************************************************************)

type constant += C_top of Typ.typ
