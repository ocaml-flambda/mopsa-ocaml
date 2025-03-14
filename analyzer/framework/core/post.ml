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
open Context
open Lattice
open Cases
open Change
open Mopsa_utils



type 'a post = ('a, unit) cases

include Cases

let return ?(cleaners=[]) flow : 'a post =
  Cases.return () ~cleaners flow

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

let remove_duplicates lattice post =
  (* Collapse all handled partitions *)
  Cases.remove_duplicates
    ~equal:(fun case case' ->
       match case,case' with
       | NotHandled,NotHandled -> true
       | _,NotHandled -> false
       | NotHandled,_ -> false
       | _ -> true
    ) lattice post
  >>% fun flow ->
  (* This return has the effect of changing Empty to Result *)
  return flow
