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


(** Manager - access to the top-level lattice and transfer functions *)


open Ast.Stmt
open Ast.Expr
open Ast.Semantic
open Lattice
open Flow
open Eval
open Post
open Effect
open Cases
open Route
open Query
open Print


(*==========================================================================*)
(**                             {2 Managers}                                *)
(*==========================================================================*)


(** Managers provide access to full analyzer.

    ['a] is the type of the toplevel abstract element, and ['t] is the type of
    local abstract element (that is, the type of the domain that calls the
    manager). *)
type ('a, 't) man = {
  lattice : 'a lattice;
  (** Access to lattice operators on the toplevel abstract element ['a]. *)

  (** {3 Accessors to domain's abstract element}

      Each domain can get and set its abstract element in the transfer functions
      with [get] and [set].  *)

  get : 'a -> 't;
  (** Returns the domain's abstract element ['t]. *)

  set : 't -> 'a -> 'a;
  (** Sets the domain's abstract element ['t]. *)

  (** {3 Toplevel Transfer Functions}

      The domains access the top-level lattice transfer function through the
      manager with these functions. It is possible to ask the manager to route
      the transfer functions (e.g. [eval]) to different domains with the
      optional [?route] parameter. If [?route] is not set, the routing starts
      from the root of the domains DAG. *)

  exec : ?route:route -> stmt -> 'a flow -> 'a post;
  (** [man.exec stmt flow] executes [stmt] in [flow] and returns the post state. *)

  eval : ?route:route -> ?translate:semantic -> ?translate_when:(semantic*(expr->bool)) list -> expr -> 'a flow -> 'a eval;
  (** [man.eval expr flow] evaluates [expr] in [flow] and returns the result
      expression.

      There are two kinds of evaluations: within the same semantic
      (simplifications), or to another semantic (translations). Calling
      [man.eval expr flow] performs both kinds of evaluations. The result [e']
      of [man.eval expr flow] is a {i simplification} of [e] within the same
      semantic. To retrieve a translation to another semantic, one can use
      the [?translate] parameter: [man.eval expr flow ~translate:semantic] is a
      {i translation} of the {i simplification} of [e] in [semantic]. A common
      use case is to translate expressions to Universal with
      [man.eval expr flow ~translate:"Universal"]. It is possible to control
      when the translation is applied with [?translate_when]. *)

  ask : 'r. ?route:route -> ('a,'r) query -> 'a flow -> ('a, 'r) cases;
 (** [man.ask query flow] performs a query to other domains. If no domain can
      answer the query, [man.ask query flow] results in a runtime error. *)

  print_expr : ?route:route -> 'a flow -> (printer -> expr -> unit);
  (** [man.print_expr flow] is the expression printer for the type ['a]. *)

  (** {3 Accessors to the Domain's Effects Tree} *)

  get_effects : teffect -> teffect;
  (** Gets the effects tree. *)

  set_effects : teffect -> teffect -> teffect;
  (** Sets the effect tree. *)
}


(** Managers provide access to the sub-tree of stacked domain *)
type ('a, 's) stack_man = {
  (* Accessors the sub-domain's abstract element ['s] within ['a] *)
  get_sub : 'a -> 's;
  set_sub : 's -> 'a -> 'a;
}
