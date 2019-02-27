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

(** Post-conditions of statement transfer functions *)

open Ast
open Flow
open Zone

(*==========================================================================*)
(**                          {2 Post-states}                                *)
(*==========================================================================*)

(** Combiner logs *)
type clog =
   | L_leaf
   | L_product of clog list
   | L_compose of clog * log

(** Post-state logs *)
and log = Ast.stmt list * clog

(** Post-state case *)
type 'a post_case = {
  post_flow : 'a flow;
  post_log  : log;
}

(** Post-state *)
type 'a post = 'a post_case Dnf.t

let of_flow flow = Dnf.singleton {
  post_flow = flow;
  post_log = [], L_leaf;
}

let to_flow lattice post =
  Dnf.substitute
    (fun case -> case.post_flow)
    (Flow.join lattice)
    (Flow.meet lattice)
    post

let return_flow flow = Some (of_flow flow)

let map_flow f post =
  Dnf.map (fun case -> {case with post_flow = f case.post_flow}) post

let print lattice fmt post =
  Dnf.print (fun fmt case -> Flow.print lattice fmt case.post_flow) fmt post


let bind_eval (f:'e -> 'a flow -> 'a post) (evl:('e, 'a) Eval.eval) : 'a post =
  Eval.to_dnf evl |>
  Dnf.bind (fun (e, flow) ->
      match e with
      | None -> of_flow flow
      | Some ee -> f ee flow
    )
