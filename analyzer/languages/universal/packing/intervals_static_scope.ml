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

(** Reduction between intervals and packed relational domains with the static
    scope strategy.
*)

open Mopsa
open Sig.Reduction.Simplified
open Sig.Abstraction.Simplified
open Static
open Ast
open Bot_top

module ReductionMake(S : Static.STRATEGY) : SIMPLIFIED_REDUCTION =
struct

  let name = "reductions." ^ S.name
  let debug fmt = Debug.debug ~channel:name fmt

  module I = Numeric.Values.Intervals.Integer.Value
  module M = Framework.Lattices.Partial_map

  (** Signature of relational numeric domains with the additional functions
      [related_vars], [bound_var] and [vars] functions.
  *)

  (** Packing map with its underlying relational domain *)
  type pack_map = PM : (S.pack,'a) M.map * (module Relational.Instances.RELATIONAL with type t = 'a) -> pack_map

  (** Get the packing map and the underlying relational domain *)
  let get_pack_map man a : pack_map =
    let (module R: Relational.Instances.RELATIONAL) = !Relational.Instances.numeric_domain in
    let aa = man.get_env (D_static_packing (S.id,R.id)) a in
    PM (aa, (module R))

  (** Get the interval of a variable in all packs *)
  let get_var_interval_in_packs var man ctx post =
    (* Get the packing map and the underlying rel domain *)
    let PM (a, domain) = get_pack_map man post in
    let module Domain = (val domain) in
    match a with
    | BOT -> I.bottom
    | TOP -> I.top
    | Nbt m ->
      (* Get the packs of the variable *)
      let packs = S.packs_of_var ctx var in
      (* Fold over the packs to compute the meet of the intervals *)
      packs |> List.fold_left (fun acc pack ->
          try
            M.PMap.find pack m |>
            Domain.bound_var var |>
            I.meet acc
          with Not_found -> acc
      ) I.top


  (** Refine the interval of a variable in the box domain *)
  let refine_var_interval var man ctx post range =
    (* Get the interval of the variable in the box domain *)
    let itv = man.get_value I.id var post in

    (** Get the interval of the variable in all packs *)
    let itv' = get_var_interval_in_packs var man ctx post in

    (* Combine data *)
    let itv'' = I.meet itv itv' in

    (* Check if box is less precise *)
    let post = if not (I.subset itv itv')
      then man.set_value I.id var itv'' post
      else
        post
    in

    (* Check if rel is less precise *)
    if not (I.subset itv' itv)
    then
      let PM (a, domain) = get_pack_map man post in
      let module CR = (val domain) in
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

  (** Reduction after a test *)
  let reduce_assume cond man ctx pre post =
    let PM (a, domain) = get_pack_map man post in
    let module Domain = (val domain) in
    match a with
    | BOT -> post
    | TOP -> post
    | Nbt m ->
      (* Get the variables in the condition *)
      let vars = Visitor.expr_vars cond in
      let post', _ = vars |> List.fold_left (fun (acc,past) var ->
          (* Fold over the packs of var and search for the variables in the same pack *)
          let packs = S.packs_of_var ctx var in
          packs |> List.fold_left (fun (acc,past) pack ->
              try
                let aa = M.PMap.find pack m in
                (* Get the variables in this pack that were not handled before *)
                let vars = Domain.vars aa |> VarSet.of_list in
                let vars' = VarSet.diff vars past in
                (* Refine the interval of these variables *)
                let acc' = VarSet.fold (fun var' acc ->
                    try refine_var_interval var' man ctx acc cond.erange
                    with Not_found -> acc
                  ) vars' acc
                in
                acc', VarSet.union vars' past
              with Not_found -> (acc,past)
            ) (acc,past)
        ) (post,VarSet.empty)
      in
      post'


  (** Reduction operator *)
  let reduce stmt man ctx (pre:'a) (post:'a) : 'a =
    match skind stmt with
    | S_assign ({ ekind = E_var (v,_) },_) ->
      refine_var_interval v man ctx post stmt.srange

    | S_assume cond ->
      reduce_assume cond man ctx pre post

    | _ ->
      post

end


let register_itv_packing_reduction s =
  let module S = (val s : STRATEGY) in
  let module Reduction = ReductionMake(S) in
  register_simplified_reduction (module Reduction)
