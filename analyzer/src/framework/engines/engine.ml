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

(** Signature of an analysis engine *)

open Ast.Program
open Ast.Expr
open Ast.Stmt
open Core
open Flow
open Eval
open Manager
open Zone
open Abstraction

module type ENGINE =
sig

  type t

  val init : program -> t flow

  val exec : ?zone:zone -> stmt -> t flow -> t flow

  val eval : ?zone:(zone * zone) -> ?via:zone -> expr -> t flow -> (expr, t) eval

  val ask : 'r Query.query -> t flow -> 'r

  val man : (t, t) man

end
