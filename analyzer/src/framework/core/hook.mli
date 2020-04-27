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
open Sig.Domain.Manager


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

  val on_before_exec : zone -> stmt -> ('a,'a) man  -> 'a flow -> 'a ctx
  (** Event fired before an exec is performed *)

  val on_after_exec : zone -> stmt -> ('a,'a) man -> 'a flow -> 'a post -> 'a ctx
  (** Event fired after an exec is performed *)

  val on_before_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> 'a ctx
  (** Event fired before an eval is performed *)

  val on_after_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> 'a eval -> 'a ctx
  (** Event fired after an eval is performed *)

  val on_finish : ('a,'a) man -> 'a flow -> unit
  (** Event fired after the analysis has terminated *)
end

(** Signature of a stateless hook *)
module type STATELESS_HOOK =
sig
  val name : string
  (** Name of the hook *)

  val exec_zones : zone list
  (** List of exec zones to capture *)

  val eval_zones: (zone * zone) list
  (** List of eval zones to capture *)

  val init : 'a ctx -> unit
  (** Initialization of the hook *)

  val on_before_exec : zone -> stmt -> ('a,'a) man  -> 'a flow -> unit
  (** Event fired before an exec is performed *)

  val on_after_exec : zone -> stmt -> ('a,'a) man -> 'a flow -> 'a post -> unit
  (** Event fired after an exec is performed *)

  val on_before_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> unit
  (** Event fired before an eval is performed *)

  val on_after_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> 'a eval -> unit
  (** Event fired after an eval is performed *)

  val on_finish : ('a,'a) man -> 'a flow -> unit
  (** Event fired after the analysis has terminated *)
end


val register_hook : (module HOOK) -> unit
(** Register a new hook *)

val register_stateless_hook : (module STATELESS_HOOK) -> unit
(** Register a new stateless hook *)

val activate_hook : string -> unit
(** Activate a registered hook *)

val find_hook : string -> (module HOOK)
(** Find a registered hook *)

val mem_hook : string -> bool
(** Check whether a hook exits *)

val deactivate_hook : string -> ('a,'a) man -> 'a flow -> unit
(** Deactivate an active hook *)

val init : interface -> unit
(** Initialize hooks manager *)

val init_hook : string -> 'a ctx -> 'a ctx
(** Initialize an active hook *)

val init_active_hooks : 'a ctx -> 'a ctx
(** Initialize all active hooks *)

val on_before_exec : zone -> stmt -> ('a,'a) man -> 'a flow -> 'a ctx
(** Call [on_before_exec] on all hooks attached to [zone] *)

val on_after_exec : zone -> stmt -> ('a,'a) man -> 'a flow -> 'a post -> 'a ctx
(** Call [on_after_exec] on all hooks attached to [zone] *)

val on_before_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> 'a ctx
(** Call [on_before_eval] on all hooks attached to [zone] *)

val on_after_eval : (zone * zone) -> expr -> ('a,'a) man -> 'a flow -> 'a eval -> 'a ctx
(** Call [on_after_eval] on all hooks attached to [zone] *)

val on_finish : ('a,'a) man -> 'a flow -> unit
(** Call [on_finish] on all active hooks *)
