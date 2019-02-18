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

(** Configuration parser.

    Build the global abstract domain from a json configuration file.

    The supported syntax for a [domain] is as follows:

    - a string denotes a leaf domain or a non-relational value
   abstraction.

    - [\{"functor": domain, "arg": domain\}] creates a functor domain
   parameterized by an argument domain.

    - [\{"iter": \[domain list\]\}] uses the iterator composer to
   combine a list of domains in sequence.

    - [\{"stack": domain, "over": domain\}] combines two domains with
   a stack configuration.

    - [\{"product": \[domain/value list\], "reductions": \[string
   list\]\}, "over": domain] constructs a reduced product of a set of
   abstract domains and non-relational value abstractions with some
   reduction rules. A common sub-domain can be provided to construct a
   stacked product.

*)

val opt_config : string ref
(** Path to the configuration file *)

val parse : unit -> string * (module Core.Domain.DOMAIN)
(** [parse ()] constructs an abstract domain from the current configuration file *)

val language : unit -> string
(** [language ()] returns the language of the current configuration file *)

val domains : unit -> string list
(** [domains ()] returns the list of domains used in the current configuration file *)
