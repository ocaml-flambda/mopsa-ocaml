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
open Log


(** Signature of an encapsulated abstraction *)
module type ABSTRACTION =
sig

  (** {2 Lattice declaration} *)
  (** *********************** *)

  type t
  (** Type of an abstract elements. *)

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

  val merge: t -> t * log -> t * log -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> (t, t) man -> t flow -> t flow
  (** Initialization function *)

  val exec_flow : zone -> stmt -> (t, t) man -> t flow -> t flow
  (** Computation of post-conditions on a flow abstraction *)

  val exec_state : zone -> stmt -> (t, t) man -> t -> t
  (** Same as [exec_flow], but limited to one flow *)

  val eval : (zone * zone) -> zone -> expr -> (t, t) man -> t flow -> (expr, t) eval
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> (t, t) man -> t flow -> 'r
  (** Handler of queries *)

  (** {2 Standalone manager} *)
  (** ********************** *)

  val man : (t,t) man

end
