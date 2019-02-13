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

open Manager

module Make(Domain : Domain.DOMAIN) :
sig

  val init : Ast.program -> (Domain.t, Domain.t) man -> Domain.t flow

  val exec : ?zone:Zone.zone -> Ast.stmt -> (Domain.t, Domain.t) man -> Domain.t flow -> Domain.t flow

  val eval : ?zone:(Zone.zone * Zone.zone) -> ?via:Zone.zone -> Ast.expr -> (Domain.t, Domain.t) man -> Domain.t flow -> (Domain.t, Ast.expr) evl

  val ask : 'r Query.query -> Domain.t Flow.flow -> 'r

  val man : (Domain.t, Domain.t) man

  val interactive_man : (Domain.t, Domain.t) man

end
