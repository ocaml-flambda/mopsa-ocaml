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

(** Typer of configuration files *)

open Visitor
open Yojson.Basic

(** {2 Types used by the typer} *)
(** *************************** *)

(** Abstractions present in MOPSA *)
type abstraction =
  | A_domain
  | A_stack
  | A_value

(** Signature levels of an abstraction *)
type signature =
  | S_lowlevel
  | S_intermediate
  | S_simplified
  | S_stateless

(** Configuration of an abstraction *)
type config = {
  abstraction : abstraction;
  signature   : signature;
  structure : structure;
}

(** Structure of a configuration *)
and structure =
  | S_leaf    of string (** Leaf configuration with a name *)
  | S_chain   of operator * config list (** Chain configurations connected by an operator *)
  | S_apply   of config * config (** Application of a stack on a domain *)
  | S_product of config list * string list (** Reduced product *)
  | S_nonrel  of config (** Non-relational domain *)
  | S_cast    of config (** Cast of a configuration to a lower signature *)

(** Operator of a chain transformer *)
and operator =
  | O_seq
  | O_compose
  | O_disjoint



(** {2 Analyzer specification} *)
(** ************************** *)

(** A specification determines which transformers are provided by the framework *)
type spec = {
  chain   : abstraction -> operator -> signature -> bool;
  apply   : signature -> bool;
  product : abstraction -> signature -> bool;
}


(** {2 Typer entry points} *)
(** ********************** *)


(** Create a domain configuration from a json object *)
val domain : spec -> json -> config


(** Create a stack configuration from a json object *)
val stack : spec -> json -> config


(** Create a value configuration from a json object *)
val value : spec -> json -> config

(** Cast a configuration to a given signature *)
val cast : signature -> config -> config

(** Print a configuration *)
val pp_config : Format.formatter -> config -> unit

(** Print a signature *)
val pp_signature : Format.formatter -> signature -> unit
