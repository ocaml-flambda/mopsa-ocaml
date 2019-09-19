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
open Core.Sig.Domain.Reduction
open Core.Sig.Domain.Simplified
open Universal.Packing.Static
open Bot_top

module Reduction =
struct

  let name = "c.memory.packing.reductions.intervals_static_scope"
  let debug fmt = Debug.debug ~channel:name fmt

  module I = Universal.Numeric.Values.Intervals.Integer.Value
  module S = Static_scope.Strategy
  module M = Framework.Lattices.Partial_map
  module O = Universal.Numeric.Relational.Instances.Octagon
  module P = Universal.Numeric.Relational.Instances.Polyhedra

  (** Signature of relational numeric domains with the additional functions
      [related_vars] and [bound_var] functions.
  *)
  module type REL =
  sig
    include DOMAIN
    val related_vars : var -> t -> var list
    val bound_var : var -> t -> I.t
  end

  (** Packing map with its underlying relational domain *)
  type pack_map = PM : (S.pack,'a) M.map * (module REL with type t = 'a) -> pack_map


  (** Get the packing map and the underlying relational domain *)
  let get_pack_map man a : pack_map =
    match !Universal.Numeric.Relational.Instances.opt_numeric with
    | "octagon" ->
      let aa = man.get (D_static_packing (S.id,O.id)) a in
      PM (aa, (module O))

    | "polyhedra" ->
      let aa = man.get (D_static_packing (S.id,P.id)) a in
      PM (aa, (module P))

    | _ -> assert false



  (** Get the list of variables related numerically to v *)
  let get_related_vars ctx v man a =
    let PM (aa, domain) = get_pack_map man a in
    let module Domain = (val domain) in
    match aa with
    | BOT -> []
    | TOP -> []
    | Nbt m ->
      let packs = S.packs_of_var ctx v in
      List.fold_left (fun acc pack ->
          try
            let aaa = M.PMap.find pack m in
            let rel = Domain.related_vars v aaa in
            rel @ acc
          with Not_found -> acc
        ) [] packs


  (** Get the list of modified variables *)
  let get_modified_vars ctx stmt man a =
    match skind stmt with
    | S_assign({ekind = E_var (v, _)}, _) -> [v]

    | S_assume e ->
      (* In case of a filter, we search for the relations of the
         variables present in the expression *)
      let vars = Visitor.expr_vars e in
      List.fold_left (fun acc v -> get_related_vars ctx v man a @ acc) vars vars |>
      List.sort_uniq compare_var

    | _ -> []



  (** Get the interval of v in pack p *)
  let get_interval_in_pack v p man a =
    let PM (aa, domain) = get_pack_map man a in
    let module Domain = (val domain) in
    match aa with
    | BOT -> I.bottom
    | TOP -> I.top
    | Nbt m ->
      try
        let aaa = M.PMap.find p m in
        Domain.bound_var v aaa
      with Not_found -> I.top


  (** Reduction operator *)
  let reduce ctx stmt man (pre:'a) (post:'a) : 'a =
    (* Get the modified variables *)
    let vars = get_modified_vars ctx stmt man pre in

    (* Refine the interval of each variable *)
    List.fold_left (fun post var ->
        (* Get the interval of var in the box domain *)
        let itv = man.get_value I.id var post in

        (** Get the packs of the variable *)
        let packs = S.packs_of_var ctx var in

        (* Get the most precise interval in packs *)
        let itv' = List.fold_left (fun acc pack ->
            let i = get_interval_in_pack var pack man post in
            I.meet acc i
          ) I.top packs
        in

        (* Check if box is less precise *)
        if not (I.subset itv itv')
        then
          let () = debug "reducing the interval of %a from %a to %a"
              pp_var var
              I.print itv
              I.print itv'
          in
          man.set_value I.id var itv' post
        else post

      ) post vars

end


let () =
  register_reduction (module Reduction)
