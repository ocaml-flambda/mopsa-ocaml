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

(** Post - Post-states of statement execution. *)

open Ast.Stmt
open Token
open Flow
open Log
open Context
open Lattice
open Cases


type 'a post = ('a, unit) cases


let return ?(log=Log.empty) ?(cleaners=[]) flow : 'a post =
  Cases.return (Some ()) ~log ~cleaners flow


let print pp fmt post =
  Cases.print (fun fmt _ flow ->
      Flow.print pp fmt flow
    ) fmt post


let join (p1:'a post) (p2:'a post) : 'a post =
  Cases.join p1 p2

let join_list ~empty l =
  Cases.join_list ~empty l


let meet (p1:'a post) (p2:'a post) : 'a post =
  Cases.meet p1 p2



let join_list ~(empty:unit -> 'a post) (l:'a post list) : 'a post =
  Cases.join_list ~empty l



let meet_list ~(empty:unit -> 'a post) (l:'a post list) : 'a post =
  Cases.join_list ~empty l


let get_ctx (p:'a post) : 'a ctx =
  Cases.get_ctx p


let set_ctx (ctx:'a ctx) (p:'a post) : 'a post =
  Cases.set_ctx ctx p


let bind f post = Cases.bind (fun _ flow -> f flow) post
