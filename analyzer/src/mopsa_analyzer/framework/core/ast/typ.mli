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

(** Types 

    This modules allows extending Mopsa with new types. This is done by
    extending the type [typ] with new variants. For example
    {[ 
      type typ += T_int
    ]}
    extends Mopsa types with a new integer type. Registration is performed with 
    {[
      let () = register_typ {
          compare = (fun next -> next);
          print = (fun next -> function
              | T_int -> Format.pp_print_string fmt "int"
              | t    -> next fmt t
            );
        }
    ]}
    Note that is this simple example where the type doesn't have an inner
    structure no comparison function is required as {!Stdlib.compare} is
    sufficient to provide a total order.
*)

open Mopsa_utils

type typ = ..
(** Extensible types *)

val compare_typ : typ -> typ -> int
(** Compare two types *)

val pp_typ : Format.formatter -> typ -> unit
(** Pretty-print a type *)


(****************************************************************************)
(**                            {1 Registration}                             *)
(****************************************************************************)

val register_typ : typ TypeExt.info -> unit
(** Register a new type *)

val register_typ_compare : typ TypeExt.compare -> unit
(** Register a new type comparison *)

val register_typ_pp : typ TypeExt.print -> unit
(** Register a new type pretty-printer *)


(****************************************************************************)
(**                            {1 Common types}                             *)
(****************************************************************************)

type typ += T_any (** Generic unknown type *)
         | T_addr (** Heap addresses type *)
