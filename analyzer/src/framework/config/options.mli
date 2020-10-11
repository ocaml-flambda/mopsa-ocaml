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

(** Management of command-line options *)

open ArgExt


(** {2 Registration} *)
(** **************** *)

(** Register a built-in option *)
val register_builtin_option : arg -> unit

(** Register a language option. *)
val register_language_option : string -> arg -> unit

(** Register a domain option. *)
val register_domain_option : string -> arg -> unit

(** Register a shared option *)
val register_shared_option : string -> arg -> unit

(** Import a shared option into a domain *)
val import_shared_option : string -> string -> unit


(** {2 Filters} *)
(** *********** *)

(** Return the list of options *)
val get_options : unit -> arg list

(** Return the list of built-in options *)
val get_builtin_options : unit -> arg list

(** Return the list of options of a language *)
val get_language_options : string -> arg list

(** Return the list of options of a domain *)
val get_domain_options : string -> arg list

val help : unit -> unit
