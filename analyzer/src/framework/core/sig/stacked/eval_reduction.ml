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

open Ast.Expr
open Id
open Flow
open Eval


type ('e, 'a) evals = ('e, 'a) eval option list

(** Manager used by reduction rules *)
type 'a man = {
  get : 't. 't domain -> (expr, 'a) evals -> (expr, 'a) eval option;
  set : 't. 't domain -> (expr, 'a) eval option -> (expr, 'a) evals -> (expr, 'a) evals;
}



(** Signature of a reduction rule *)
module type REDUCTION =
sig
  val name   : string
  val reduce : expr -> 'a man -> (expr,'a) evals  -> (expr,'a) evals
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
