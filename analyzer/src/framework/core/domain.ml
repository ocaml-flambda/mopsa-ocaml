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

(** Unified signatures of abstract domains.

    Two signatures are defined:

    1. Domains implementing the LEAF signature represent standalone
   abstractions. Their γ function, lattice operators and transfer
   functions do not depend on other external abstractions.

    2. Domains implementing the STACK signature represent
   parameterized abstractions. Their γ function, lattice operators and
   transfer functions depend on argument abstractions. Unlike classic
   OCaml functors, argument abstractions are not fixed at module
   creation. Instead, stack domains receive a record encapsulation of
   the transfer functions of their argument abstractions at
   runtime. This allows sharing the arguments among other stack
   domains (in a product for example).

*)


open Ast
open Annotation
open Flow
open Manager
open Eval
open Post
open Zone
open Eq



(** {2 Useful types} *)
(** **************** *)

(** Zone interface of a transfer function *)
type 'a interface = {
  provides : 'a list;
  uses :     'a list;
}


(** Domain identifier *)
type _ domain = ..



(** {2 Unified signatures} *)
(** ********************** *)

(** Unified signature of leaf abstract domains *)
module type LEAF =
sig

  (** {2 Structure} *)
  (** ************* *)

  type t
  (** Type of an abstract elements. *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Predicates} *)
  (** ************** *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)


  (** {2 Operators} *)
  (** ************* *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: 'a annot -> t -> t -> t
  (** [widen annot a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Printing} *)
  (** ************ *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Identification} *)
  (** ****************** *)

  val id : t domain
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val identify : 'a domain -> (t, 'a) eq option
  (** Check the identity of the domain *)


  (** {2 Interface of transfer functions} *)
  (** *********************************** *)

  val exec_interface : zone interface
  (** Interface of the [exec] transfer function *)

  val eval_interface : (zone * zone) interface
  (** Interface of the eval transfer function *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t) man -> 'a flow -> 'a flow option
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
  (** Handler of queries *)

end


(** Unified signature of stack abstract domains *)
module type STACK =
sig

  (** {2 Structure} *)
  (** ************* *)

  type t
  (** Type of an abstract elements. *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Predicates} *)
  (** ************** *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: 'a arg -> t * 'a flow -> t * 'a flow -> bool * 'a post * 'a post
  (** Partial order relation. [subset arg (a1, arg1) (a2, arg2)] tests
     whether [a1] is related to (or included in) [a2] and unifies the
     arguments [arg1] and [arg2]. *)


  (** {2 Operators} *)
  (** ************* *)

  val join: 'a arg -> t * 'a flow -> t * 'a flow -> t * 'a post * 'a post
  (** [join arg (a1, arg1) (a2, arg2)] computes an upper bound of [a1]
     and [a2] and unifies the arguments [arg1] and [arg2]. *)

  val meet: 'a arg -> t * 'a flow -> t * 'a flow -> t * 'a post * 'a post
  (** [meet arg (a1, arg1) (a2, arg2)] computes a lower bound of of [a1]
     and [a2] and unifies the arguments [arg1] and [arg2]. *)

  val widen:
    'a Annotation.annot -> 'a arg -> t * 'a flow -> t * 'a flow ->
    t * bool * 'a post * 'a post
  (** [widen annot arg (a1, arg1) (a2, arg2)] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Printing} *)
  (** ************ *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Identification} *)
  (** ****************** *)

  val id : t domain
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val identify : 'a domain -> (t, 'a) eq option
  (** Check the identity of the domain *)


  (** {2 Interface of transfer functions} *)
  (** *********************************** *)

  val exec_interface : zone interface
  (** Interface of the [exec] transfer function *)

  val eval_interface : (zone * zone) interface
  (** Interface of the eval transfer function *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, t) man -> 'a arg -> 'a flow -> 'a post option
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t) man -> 'a arg -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t) man -> 'a arg -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, t) man -> 'a arg -> 'a flow -> 'r option
  (** Handler of queries *)

end



(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let leaves : (module LEAF) list ref = ref []
let stacks : (module STACK) list ref = ref []

let register_leaf_domain info = leaves := info :: !leaves
let register_stack_domain info = stacks := info :: !stacks
