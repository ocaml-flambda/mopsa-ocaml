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

(**
   Reduction operator for refining the interval of a variable x when
   another variable y is filtered and a relation exists between the
   two variables x and y.
*)

open Mopsa
open Framework.Domains.Reduced_product.Reductions.Post_reduction
open Framework.Domains.Reduced_product.Pool
open Ast

let name = "universal.numeric.reductions.implicit_assume_relation"
let debug fmt = Debug.debug ~channel:name fmt

(* module Reduction : REDUCTION =
 * struct
 * 
 *   module I = Values.Intervals.Value
 *   module O = Relational.Oct
 *   module P = Relational.Poly
 * 
 *   let trigger = None
 * 
 *   let var_relations v dman cur =
 *     let doit : type t. t domain -> 'a -> 'a = fun id acc ->
 *       match O.identify id, P.identify id with
 *       | Some Eq, None ->
 *         (\* Octagons *\)
 *         let o = dman.get_env O.id cur in
 *         O.var_relations v o @ acc
 * 
 *       | None, Some Eq ->
 *         (\* Polyhedra *\)
 *         let p = dman.get_env P.id cur in
 *         P.var_relations v p @ acc
 * 
 *       | _ -> acc
 *     in
 * 
 *     dman.fold {doit} []
 * 
 * 
 *   let get_rel_interval v dman cur =
 *     let doit : type t. t domain -> 'a -> 'a = fun id acc ->
 *       match O.identify id, P.identify id with
 *       | Some Eq, None ->
 *         (\* Octagons *\)
 *         let o = dman.get_env O.id cur in
 *         O.get_interval v o
 * 
 *       | None, Some Eq ->
 *         (\* Polyhedra *\)
 *         let p = dman.get_env P.id cur in
 *         P.get_interval v p
 * 
 *       | _ -> acc
 *     in
 * 
 *     dman.fold {doit} I.top
 * 
 *   let reduce stmt dman nrman man flow =
 *     match skind stmt with
 *     | S_assume(e) ->
 *       (\* Get variables involved explicitly in the filter statement *\)
 *       let vars = Framework.Visitor.expr_vars e in
 *       let cur = Flow.get T_cur man flow in
 * 
 *       (\* Fold over the relations of the variables *\)
 *       let cur' = List.fold_left (fun acc v ->
 *           var_relations v dman cur |>
 *           List.fold_left (fun acc v' ->
 *               (\* Refine the interval of the variable [v] in relation with [v'] *\)
 *               let itv = nrman.get_var_value I.id v' cur in
 *               let itv' = get_rel_interval v' dman cur in
 *               nrman.set_var_value I.id v' (I.meet (Flow.get_all_annot flow) itv itv') acc
 *             ) acc
 *         ) cur vars
 *       in
 * 
 *       Flow.set T_cur cur' man flow
 * 
 *     | _ -> flow
 * 
 * end
 * 
 * 
 * let () =
 *   register_reduction name (module Reduction) *)
