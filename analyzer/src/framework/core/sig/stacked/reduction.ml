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

(** Reduction rules for products of stack domains *)

open Ast.Var
open Ast.Stmt
open Context
open Id
open Query
open Channel
open Zone
open Flow
open Post

(** Manager used reduction rules *)
type 'a man = {
  get : 't. 't domain -> 'a -> 't;
  set : 't. 't domain -> 't -> 'a -> 'a;
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  sub_exec : ?zone:zone -> stmt -> 'a flow -> 'a post;
  ask : 'r. 'r query -> 'a flow -> 'r;
}



(** Signature of a reduction rule *)
module type REDUCTION =
sig
  val name   : string
  val reduce : stmt -> 'a man -> 'a flow -> 'a flow -> 'a flow
end


(** Registered reductions *)
let reductions : (module REDUCTION) list ref = ref []


(** Register a new reduction *)
let register_reduction rule =
  reductions := rule :: !reductions

(** Find a reduction by its name *)
let find_reduction name =
  List.find (fun v ->
      let module V = (val v : REDUCTION) in
      compare V.name name = 0
    ) !reductions
