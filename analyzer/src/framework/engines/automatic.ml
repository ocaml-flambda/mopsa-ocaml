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

(** Engine of an automatic analysis *)

open Ast.Stmt
open Ast.Expr
open Core
open Lattice
open Flow
open Manager
open Eval
open Zone
open Query
open Abstraction
open Engine

(** Create an automatic analysis engine over an abstraction. *)
module Make(Abstraction : ABSTRACTION) : ENGINE with type t = Abstraction.t =
struct

  type t = Abstraction.t

  let rec init prog =
    Abstraction.init prog man

  and exec ?(zone=any_zone) stmt flow =
    Abstraction.exec ~zone stmt man flow

  and post ?(zone=any_zone) stmt flow =
    Abstraction.post ~zone stmt man flow

  and eval ?(zone=any_zone,any_zone) ?(via=any_zone) exp flow =
    Abstraction.eval ~zone ~via exp man flow

  and ask : type r. r query -> Abstraction.t flow -> r =
    fun query flow ->
      Abstraction.ask query man flow

  and lattice : Abstraction.t lattice = {
    bottom = Abstraction.bottom;
    top = Abstraction.top;
    is_bottom = (fun a -> Abstraction.is_bottom man a);
    subset = (fun a a' -> Abstraction.subset man a a');
    join = (fun a a' -> Abstraction.join man a a');
    meet = (fun a a' -> Abstraction.meet man a a');
    widen = (fun ctx a a' -> Abstraction.widen man ctx a a');
    print = (fun fmt a -> Abstraction.print man fmt a);
  }

  and man : (Abstraction.t, Abstraction.t) man = {
    lattice;
    get = (fun flow -> flow);
    set = (fun flow _ -> flow);
    get_log = (fun log -> log);
    set_log = (fun log _ -> log);
    exec = exec;
    post = post;
    eval = eval;
    ask = ask;
  }


end
