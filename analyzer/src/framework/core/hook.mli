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

(** Hooks are modules that can observe the execution of the transfer
    functions without modifying their output. They can be used however to enrich the
    analysis by adding information to the context.
*)

open Ast.Stmt
open Ast.Expr
open Lattice
open Flow
open Context
open Post
open Eval
open Zone
open Interface


(** Signature of a hook *)
module type HOOK =
sig

  val name : string
  (** Name of the hook *)

  val exec_zones : zone list
  (** List of exec zones to capture *)

  val eval_zones: (zone * zone) list
  (** List of eval zones to capture *)

  val init : 'a ctx -> 'a ctx
  (** Initialization of the hook *)

  val before_exec : zone -> stmt -> 'a lattice -> 'a flow -> 'a ctx
  (** Event fired before an exec is performed *)

  val after_exec : zone -> stmt -> 'a lattice -> 'a post -> 'a ctx
  (** Event fired after an exec is performed *)

  val before_eval : (zone * zone) -> expr -> 'a lattice -> 'a flow -> 'a ctx
  (** Event fired before an eval is performed *)

  val after_eval : (zone * zone) -> expr -> 'a lattice -> 'a eval -> 'a ctx
  (** Event fired after an eval is performed *)
end


val register_hook : (module HOOK) -> unit
(** Register a new hook *)

val init : interface -> 'a ctx -> 'a ctx
(** Initialize all hooks *)

val before_exec : zone -> stmt -> 'a lattice -> 'a flow -> 'a ctx
(** Call [before_exec] on all hooks attached to [zone] *)

val after_exec : zone -> stmt -> 'a lattice -> 'a post -> 'a ctx
(** Call [after_exec] on all hooks attached to [zone] *)

val before_eval : (zone * zone) -> expr -> 'a lattice -> 'a flow -> 'a ctx
(** Call [before_eval] on all hooks attached to [zone] *)

val after_eval : (zone * zone) -> expr -> 'a lattice -> 'a eval -> 'a ctx
(** Call [after_eval] on all hooks attached to [zone] *)
