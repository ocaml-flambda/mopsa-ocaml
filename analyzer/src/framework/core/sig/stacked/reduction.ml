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

(** Reduction rules for abstract evaluations *)

open Ast.Stmt
open Ast.Expr
open Zone
open Id
open Lattice
open Flow
open Eval


(* Product evaluations *)
type ('e, 'a) peval = ('e, 'a) eval option list

(** Manager used by eval reduction rules *)
type ('a,'s) eman = {
  lattice : 'a lattice;
  get_man : 't. 't domain -> ('a, 't, 's) Manager.man;
  get_eval : 't. 't domain -> (expr, 'a) peval -> (expr, 'a) eval option;
  set_eval : 't. 't domain -> (expr, 'a) eval option -> (expr, 'a) peval -> (expr, 'a) peval;
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
}



(** Signature of a reduction rule *)
module type EREDUCTION =
sig
  val name   : string
  val reduce : expr -> ('a,'s) eman -> (expr,'a) peval  -> (expr,'a) peval
end


(** Registered reductions *)
let reductions : (module EREDUCTION) list ref = ref []


(** Register a new reduction *)
let register_eval_reduction rule =
  reductions := rule :: !reductions

(** Find a reduction by its name *)
let find_eval_reduction name =
  List.find (fun v ->
      let module V = (val v : EREDUCTION) in
      compare V.name name = 0
    ) !reductions
