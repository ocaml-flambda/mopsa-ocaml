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

(** Programs

    This module allows defining new kinds of programs to be analyzed by Mopsa.
    A program encapsulates the static information present in the source files
    that may be needed during the analysis, such as the list of functions 
    variables, etc.
 *)

open Mopsa_utils


type prog_kind = ..
(** Extensible type of program kinds *)

type program = {
  prog_kind : prog_kind;       (** kind of the program *)
  prog_range : Location.range; (** program location *)
}
(** Programs *)

val compare_program : program -> program -> int
(** Total order between programs *)

val pp_program : Format.formatter -> program -> unit
(** Pretty-printer for programs *)


(****************************************************************************)
(**                            {1 Registration}                             *)
(****************************************************************************)

val register_program : program TypeExt.info -> unit
(** [register_program info] registers a new program kind by registering its
    comparison function [info.compare] and pretty-printer [info.print] *)

val register_program_compare : program TypeExt.compare -> unit
(** Register a comparison function between programs *)

val register_program_pp : program TypeExt.print -> unit
(** Register a pretty-printer for programs *)
