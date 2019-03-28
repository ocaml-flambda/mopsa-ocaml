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

(** Abstractions are the encapsulation used by the analyzer to access
    the transfer function of the abstract domain.

    There are three main differences with domains. First, maps of zoned
    transfer functions are constructed at creation. Second, transfer functions
    are not partial functions and return always a result. Finally, post
    conditions of statements are merged into flows.
*)

open Context
open Ast.All
open Flow
open Manager
open Eval
open Zone

module type ABSTRACTION =
sig

  (** {2 Lattice declaration} *)
  (** *********************** *)

  type t
  (** Type of an abstract elements. *)

  type a
  (** Type of the global abstraction. *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: uctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> (a, t) man -> a flow -> a flow
  (** Initialization function *)

  val exec : zone -> stmt -> (a, t) man -> a flow -> a flow
  (** Post-state of statements *)

  val eval : (zone * zone) -> zone -> expr -> (a, t) man -> a flow -> (expr, a) eval
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> (a, t) man -> a flow -> 'r
  (** Handler of queries *)

end
