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

(** Engine for an automatic analysis without user interaction *)

open Ast.Stmt
open Ast.Expr
open Core
open Lattice
open Flow
open Eval
open Zone
open Query
open Toplevel
open Manager
open Engine


module Make(Toplevel : TOPLEVEL) : ENGINE with type t = Toplevel.t =
struct

  type t = Toplevel.t

  let rec init prog =
    Toplevel.init prog man

  and exec ?(zone=any_zone) stmt flow =
    Toplevel.exec ~zone stmt man flow

  and post ?(zone=any_zone) stmt flow =
    Toplevel.post ~zone stmt man flow

  and eval ?(zone=any_zone,any_zone) ?(via=any_zone) exp flow =
    Toplevel.eval ~zone ~via exp man flow

  and ask : type r. r query -> Toplevel.t flow -> r =
    fun query flow ->
      Toplevel.ask query man flow

  and lattice : Toplevel.t lattice = {
    bottom = Toplevel.bottom;
    top = Toplevel.top;
    is_bottom = Toplevel.is_bottom;
    subset = (fun ctx a a' -> Toplevel.subset man ctx a a');
    join = (fun ctx a a' -> Toplevel.join man ctx a a');
    meet = (fun ctx a a' -> Toplevel.meet man ctx a a');
    widen = (fun ctx a a' -> Toplevel.widen man ctx a a');
    merge = Toplevel.merge;
    print = Toplevel.print;
  }

  and man : (Toplevel.t, Toplevel.t, unit) man = {
    lattice;
    get = (fun a -> a);
    set = (fun a _ -> a);
    get_sub = (fun _ -> ());
    set_sub = (fun () a -> a);
    get_log = (fun log -> log);
    set_log = (fun log _ -> log);
    exec = exec;
    post = post;
    eval = eval;
    ask = ask;
  }


end
