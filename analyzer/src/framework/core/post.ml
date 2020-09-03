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
open Log



type 'a post = ('a, unit) cases

include Cases

let return ?(cleaners=[]) flow : 'a post =
  Cases.return () ~cleaners flow

let print pp fmt post =
  Cases.print (fun fmt _ flow ->
      Flow.print pp fmt flow
    ) fmt post

let bind_opt f post =
  Cases.bind_opt
    (fun case flow ->
       match case with
       | Result _ | Empty -> f flow
       | NotHandled       -> Some (not_handled flow)
    ) post

let (>>%?) post f = bind_opt f post

let bind f post =
  post |> bind_opt (fun flow -> Some (f flow)) |>
  OptionExt.none_to_exn

let (>>%) post f = bind f post

