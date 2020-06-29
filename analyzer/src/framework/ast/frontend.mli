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

(** Frontends

    This module manages the frontends of supported languages that translate
    input source files into a Mopsa {!Program.program}.
*)

open Program

type frontend = {
  lang: string;
  (** Language of the frontend *)

  parse: string list -> program;
  (** Parser function that translates a list of input source files into
      a Mopsa {!Program.program} *)
}
(** Frontends *)

val register_frontend : frontend -> unit
(** Register a new frontend *)

val find_language_frontend : string -> frontend
(** Find the frontend of a given language *)
