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

open Core.All
open Toplevel
open Engine


module Make(Toplevel : TOPLEVEL) : ENGINE with type t = Toplevel.t =
struct

  type t = Toplevel.t

  let rec init prog =
    Toplevel.init prog man

  and exec ?(route=toplevel)stmt flow =
    Toplevel.exec ~route stmt man flow

  and eval ?(route=toplevel) exp flow =
    Toplevel.eval ~route exp man flow

  and ask : type r. ?route:route -> (Toplevel.t,r) query -> Toplevel.t flow -> r =
    fun ?(route=toplevel) query flow ->
      Toplevel.ask ~route query man flow

  and print_expr ?(route=toplevel) flow printer exp =
    Toplevel.print_expr ~route man flow printer exp

  and lattice : Toplevel.t lattice = {
    bottom = Toplevel.bottom;
    top = Toplevel.top;
    is_bottom = Toplevel.is_bottom;
    subset = (fun ctx a a' -> Toplevel.subset man ctx a a');
    join = (fun ctx a a' -> Toplevel.join man ctx a a');
    meet = (fun ctx a a' -> Toplevel.meet man ctx a a');
    widen = (fun ctx a a' -> Toplevel.widen man ctx a a');
    merge = Toplevel.merge;
    print = Toplevel.print_state;
  }

  and man : (Toplevel.t, Toplevel.t) man = {
    lattice;
    get = (fun a -> a);
    set = (fun a _ -> a);
    get_effects = (fun e -> e);
    set_effects = (fun e _ -> e);
    exec = exec;
    eval = eval;
    ask = ask;
    print_expr = print_expr;
  }


end
