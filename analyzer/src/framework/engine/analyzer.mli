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

(** Analyzer - Central orchestrer of the analysis architecture. *)

open Ast.Program
open Ast.Expr
open Ast.Stmt
open Core
open Flow
open Eval
open Manager
open Zone

module Make(Domain : Sig.Domain.DOMAIN) :
sig

  val init : program -> (Domain.t, Domain.t) man -> Domain.t flow

  val exec : ?zone:zone -> stmt -> (Domain.t, Domain.t) man -> Domain.t flow -> Domain.t flow

  val eval : ?zone:(zone * zone) -> ?via:zone -> expr -> (Domain.t, Domain.t) man -> Domain.t flow -> (expr, Domain.t) eval

  val ask : 'r Query.query -> Domain.t Flow.flow -> 'r

  val man : (Domain.t, Domain.t) man

  val interactive_man : (Domain.t, Domain.t) man

end
