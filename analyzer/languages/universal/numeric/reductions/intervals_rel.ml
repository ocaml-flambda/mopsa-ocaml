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

(** Reduction operator for computing the most precise interval in a
    post-condition and inform the intervals domain about it.
*)

open Mopsa
open Sig.Reduction.Simplified


module Reduction =
struct

  let name = "universal.numeric.reductions.intervals_rel"
  let debug fmt = Debug.debug ~channel:name fmt

  module I = Values.Intervals.Integer.Value
  module R = Relational.Domain

  (** Get a list of variables related numerically to [v] *)
  let get_related_vars v man ctx a = man.ask (R.Q_related_vars v) ctx a


  (** Get the list of modified variables *)
  let get_modified_vars stmt man ctx a =
    match skind stmt with
    | S_assign({ekind = E_var (v, _)}, _) -> [v]

    | S_assume e ->
      (* In case of a filter, we search for the relations of the
         variables present in the expression *)
      let vars = Visitor.expr_vars e in
      List.fold_left (fun acc v -> get_related_vars v man ctx a @ acc) vars vars |>
      List.sort_uniq compare_var

    | _ -> []



  (** Reduction operator *)
  let reduce stmt man ctx (pre:'a) (post:'a) : 'a =
    (* Get the modified variables *)
    let vars = get_modified_vars stmt man ctx pre in

    (* Refine the interval of each variable *)
    List.fold_left (fun post var ->
        (* Get the interval of var in the box domain *)
        let itv = man.get_value I.id var post in

        (* Get the interval in all domain *)
        let itv' = man.ask (Common.mk_int_interval_query ~fast:false (mk_var var stmt.srange)) ctx post in

        (* Check if box is less precise *)
        if not (I.subset itv itv')
        then man.set_value I.id var itv' post
        else post

      ) post vars

end


let () =
  register_simplified_reduction (module Reduction)
