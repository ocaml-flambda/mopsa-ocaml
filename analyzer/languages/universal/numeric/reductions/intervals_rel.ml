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
open Ast

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
  (* TODO: 
     - factor with universal.numeric.packing.intervals_static_scope.ml
     - float reduction
  *)
  let reduce stmt man ctx (pre:'a) (post:'a) : 'a =
    let (module CR : Relational.Instances.RELATIONAL) = !Relational.Instances.numeric_domain in

    (* Get the modified variables *)
    let vars = get_modified_vars stmt man ctx pre |>
               List.filter (fun v -> is_int_type (vtyp v)) in

    (* Refine the interval of each variable *)
    List.fold_left (fun post var ->
        (* Get the interval of var in the box domain *)
        let itv = man.get_value I.id var post in

        (* Get the interval in the relational domain *)
        let itv' = CR.bound_var var (man.get_env CR.id post) in

        (* Combine data *)
        let itv'' = I.meet itv itv' in

        let () = debug "%a %a %a %a %a" pp_var var pp_typ (vtyp var) (format I.print) itv (format I.print) itv' (format I.print) itv'' in

        (* Check if box is less precise *)
        let post = if not (I.subset itv itv')
          then man.set_value I.id var itv'' post
          else
            post
        in

        (* Check if rel is less precise *)
        if not (I.subset itv' itv)
        then
          let range = stmt.srange in
          let ev = mk_var var range in
          match itv'' with
          | Bot.BOT -> man.set_env CR.id CR.bottom post
          | Bot.Nb _ ->
            let ol, oh = I.bounds_opt itv'' in
            let assume s post =
              let oenv = CR.assume s (fun q -> man.ask q ctx post) (man.get_env CR.id post) in
              OptionExt.apply (fun env -> man.set_env CR.id env post) post oenv in
            let post =
              OptionExt.apply (fun l ->
                  assume (mk_assume (mk_binop ~etyp:T_bool (mk_constant ~etyp:T_int (C_int l) range) O_le ev range) range) post) post ol in
            let post =
              OptionExt.apply (fun h -> assume (mk_assume (mk_binop ~etyp:T_bool ev O_le (mk_constant ~etyp:T_int (C_int h) range) range) range) post) post oh in
            post
        else post

      ) post vars

end


let () =
  register_simplified_reduction (module Reduction)
