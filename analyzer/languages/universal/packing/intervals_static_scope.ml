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

  let packs_of_var ctx var = 
    let r = S.packs_of_var ctx var in
    let () = debug "packs_of_var %a = %a" pp_var var (Format.pp_print_list (format S.print)) r in
    r

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
      let packs = packs_of_var ctx var in
      (* Fold over the packs to compute the meet of the intervals *)
      packs |> List.fold_left (fun acc pack ->
          try
            M.PMap.find pack m |>
            Domain.bound_var var |>
            I.meet acc
          with Not_found -> acc
      ) I.top


  (** Refine the interval of a variable in the box domain *)
  (* TODO:
     - factor with universal.numeric.reductions.intervals_rels.ml
     - handle floating point
  *)
  let refine_var_interval var man ctx post range =
    (* FIXME: int or bool *)
    if compare_typ (vtyp var) T_int <> 0 then post else
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
      let (module R: Relational.Instances.RELATIONAL) = !Relational.Instances.numeric_domain in
      let packing_id = (D_static_packing (S.id,R.id)) in
      let rel_packs  = man.get_env packing_id post in
      match itv'', rel_packs with
      | Bot.BOT, _ -> man.set_env packing_id BOT post
      | Bot.Nb _, (BOT | TOP) -> post 
      | Bot.Nb _, Nbt rel_packs ->
        let ol, oh = I.bounds_opt itv'' in
        let packs = packs_of_var ctx var in
        let ev = mk_var var range in
        (* FIXME: shall this really be done in ALL packs? *)
        let new_rel_packs =
          List.fold_left (fun m pack ->
            try
              let a_pack = M.PMap.find pack m in
              (* in you suspect your packing is buggy, you may want to uncomment the condition below: we previously encountered a case where the assume below introduced var in new packs besides the one it was defined in, which is clearly not the point *)
              (* if List.exists (fun v -> compare_var var v = 0) (R.vars a_pack) then *)
                let assume s p =
                  OptionExt.default p
                    (R.assume s (fun q -> man.ask q ctx post) p) in
                let a_pack =
                  OptionExt.apply
                    (fun l -> assume
                        (mk_assume (mk_binop ~etyp:T_bool (mk_constant ~etyp:T_int (C_int l) range) O_le ev range) range) a_pack)
                    a_pack ol in
                let a_pack =
                  OptionExt.apply
                    (fun h -> assume
                        (mk_assume (mk_binop ~etyp:T_bool ev O_le (mk_constant ~etyp:T_int (C_int h) range) range) range)
                        a_pack) a_pack oh in
                M.PMap.add pack a_pack m
              (* else *)
              (*   m *)
            with Not_found -> m) rel_packs packs in
        man.set_env packing_id (Nbt new_rel_packs) post 
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
      let vars =
        (* Find all variables related to vars in all packs *)
        let packs = List.fold_left (fun acc var -> packs_of_var ctx var @ acc) [] vars |>
                    List.sort_uniq S.compare in
        List.fold_left (fun acc pack ->
            let aa = M.PMap.find pack m in
            List.fold_left (fun acc v -> Domain.related_vars v aa @ acc) acc vars
          ) vars packs |>
        List.sort_uniq compare_var |>
        List.filter (fun v -> compare_typ (vtyp v) T_int = 0) in
      let post' =
        List.fold_left (fun acc var ->
            try
              refine_var_interval var man ctx acc cond.erange
            with Not_found -> acc
          ) post vars in
      let () = debug "end reduce assume" in 
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
