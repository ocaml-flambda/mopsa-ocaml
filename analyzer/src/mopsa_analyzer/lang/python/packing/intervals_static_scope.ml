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
(* open Universal.Packing.Static *)
open Bot_top

module Reduction =
struct

  let name = "python.packing.reductions.intervals_static_scope"
  let debug fmt = Debug.debug ~channel:name fmt

  module I = Universal.Numeric.Values.Intervals.Integer.Value
  module S = Static_scope.Strategy
  module M = Framework.Lattices.Partial_map
  module O = Universal.Numeric.Relational.Instances.Octagon
  module P = Universal.Numeric.Relational.Instances.Polyhedra
  module L = Universal.Numeric.Relational.Instances.LinEqualities

  (** Signature of relational numeric domains with the additional functions
      [related_vars], [bound_var] and [vars] functions.
  *)
  module type REL =
  sig
    include SIMPLIFIED
    val related_vars : var -> t -> var list
    val bound_var : var -> t -> I.t
    val vars : t -> var list
  end

  (** Packing map with its underlying relational domain *)
  type pack_map = PM : (S.pack,'a) M.map * (module REL with type t = 'a) -> pack_map


  (** Get the packing map and the underlying relational domain *)
  let get_pack_map man a : pack_map =
    match !Universal.Numeric.Relational.Instances.opt_numeric with
    | "octagon" ->
      let aa = man.get_env (Universal.Packing.Static.D_static_packing (S.id,O.id)) a in
      PM (aa, (module O))

    | "polyhedra" ->
      let aa = man.get_env (Universal.Packing.Static.D_static_packing (S.id,P.id)) a in
      PM (aa, (module P))

    | _ -> assert false



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
  let refine_var_interval var man ctx a =
    (* Get the interval of the variable in the box domain *)
    let itv = man.get_value I.id var a in
    (** Get the interval of the variable in all packs *)
    let itv' = get_var_interval_in_packs var man ctx a in
    if I.subset itv itv' then a
    else
      let itv'' = I.meet itv itv' in
      man.set_value I.id var itv'' a


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
                    refine_var_interval var' man ctx acc
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
      refine_var_interval v man ctx post

    | S_assume cond ->
      reduce_assume cond man ctx pre post

    | _ ->
      post

end


let () =
  register_simplified_reduction (module Reduction)
