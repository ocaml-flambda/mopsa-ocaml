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
open Lattice
open Token
open Flow
open Eval
open Post
open Effect
open Context
open Cases
open Route
open Query
open Print
open Ast.Semantic


(*==========================================================================*)
(**                             {2 Managers}                                *)
(*==========================================================================*)


(** Managers provide access to full analyzer *)
type ('a, 't) man = {
  (* Lattice operators of the toplevel abstract element ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (* Toplevel transfer functions *)
  exec : ?route:route -> stmt -> 'a flow -> 'a post;
  eval : ?route:route -> ?translate:semantic -> ?translate_when:(semantic*(expr->bool)) list -> expr -> 'a flow -> 'a eval;
  ask : 'r. ?route:route -> ('a,'r) query -> 'a flow -> ('a, 'r) cases;
  print_expr : ?route:route -> 'a flow -> (printer -> expr -> unit);

  (* Accessors to the domain's effects tree *)
  get_effects : teffect -> teffect;
  set_effects : teffect -> teffect -> teffect;
}


(** Managers provide access to the sub-tree of stacked domain *)
type ('a, 's) stack_man = {
  (* Accessors the sub-domain's abstract element ['s] within ['a] *)
  get_sub : 'a -> 's;
  set_sub : 's -> 'a -> 'a;
}
