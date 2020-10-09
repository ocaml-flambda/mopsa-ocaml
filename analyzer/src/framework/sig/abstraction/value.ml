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

(** Signature of a value abstraction. *)

open Core.All


(*==========================================================================*)
(**                          {2 Value manager}                              *)
(*==========================================================================*)

type ('v,'t) value_man = {
  bottom : 'v;
  top    : 'v;
  is_bottom : 'v -> bool;
  subset : 'v -> 'v -> bool;
  join : 'v -> 'v -> 'v;
  meet : 'v -> 'v -> 'v;
  print : printer -> 'v -> unit;
  get  : 'v -> 't;
  set  : 't -> 'v -> 'v;
  eval : expr -> 'v;
  avalue : 'r. 'r avalue_kind -> 'v -> 'r;
  ask : 'a 'r. ('a,'r) query -> 'r;
}


(*==========================================================================*)
(**                        {2 Valued expressions}                           *)
(*==========================================================================*)

type 'v vexpr =
  | Map of ('v * 'v vexpr) ExprMap.t

let empty_vexpr = Map ExprMap.empty

let singleton_vexpr e v ve =
  Map (ExprMap.singleton e (v,ve))

let root_vexpr = function
  | Map map -> Map (ExprMap.map (fun (v,ve) -> (v,empty_vexpr)) map)

let add_vexpr e v ve = function
  | Map map -> Map (ExprMap.add e (v,ve) map)

let refine_vexpr e v = function
  | Map map ->
    let r =
      match ExprMap.find_opt e map with
      | Some (_,ve) -> (v,ve)
      | None -> (v,empty_vexpr)
    in
    Map (ExprMap.add e r map)

let rec find_vexpr e = function
  | Map map ->
    try ExprMap.find e map
    with Not_found ->
      let rec iter = function
        | [] -> raise Not_found
        | (_,(_,vee))::tl ->
          try find_vexpr e vee
          with Not_found -> iter tl
      in
      iter (ExprMap.bindings map)

let find_vexpr_opt e ve =
  try Some (find_vexpr e ve)
  with Not_found -> None

let rec map_vexpr f = function
  | Map map ->
    let map' =
      ExprMap.map
        (fun (v,ve) -> (f v,map_vexpr f ve))
        map
    in
    Map map'

let fold_root_vexpr f init = function
  | Map map ->
    ExprMap.fold
      (fun e (v,ve) acc -> f acc e v ve)
      map init

let rec fold_vexpr f init = function
  | Map map ->
    ExprMap.fold
      (fun e (v,ve) acc -> fold_vexpr f (f acc e v ve) ve)
      map init

let rec map2_vexpr f1 f2 f ve1 ve2 =
  match ve1,ve2 with
  | Map m1, Map m2 ->
    let m =
      ExprMap.map2o
        (fun e (v1,ve1) -> f1 v1, map_vexpr f1 ve1)
        (fun e (v2,ve2) -> f2 v2, map_vexpr f2 ve2)
        (fun e (v1,ve1) (v2,ve2) ->
           (f v1 v2, map2_vexpr f1 f2 f ve1 ve2)
        )
        m1 m2 in
    Map m

let rec merge_vexpr vmerge ve1 ve2 =
  match ve1,ve2 with
  | Map m1, Map m2 ->
    let m =
      ExprMap.map2zo
        (fun e ve1 -> ve1)
        (fun e ve2 -> ve2)
        (fun e (v1,ve1) (v2,ve2) ->
           (vmerge v1 v2, merge_vexpr vmerge ve1 ve2)
        )
        m1 m2 in
    Map m

(*==========================================================================*)
(**                          {2 Value domain}                               *)
(*==========================================================================*)


module type VALUE =
sig
  type t
  val id : t id
  val accept_type : typ -> bool
  val name : string
  val display : string
  val bottom: t
  val top: t
  val is_bottom: t -> bool
  val subset: t -> t -> bool
  val join: t -> t -> t
  val meet: t -> t -> t
  val widen: 'a ctx -> t -> t -> t
  val eval : ('v,t) value_man -> expr -> t
  val avalue : 'r avalue_kind -> t -> 'r option
  val backward : ('v,t) value_man -> expr -> t vexpr -> 'v -> t vexpr
  val filter : bool -> typ -> t -> t
  val compare : ('v,t) value_man -> operator -> bool -> expr -> t -> expr -> t -> (t * t)
  val eval_ext : ('v,t) value_man -> expr -> 'v option
  val backward_ext : ('v,t) value_man -> expr -> 'v vexpr -> 'v -> 'v vexpr option
  val compare_ext : ('v,t) value_man -> operator -> bool -> expr -> 'v -> expr -> 'v -> ('v * 'v) option
  val ask : ('v,t) value_man -> ('a,'r) query -> 'r option
  val print: printer -> t -> unit
end

let default_filter b t v = v
let default_backward man e ve v = ve
let default_compare man op b e1 v1 e2 v2 = (v1,v2)

module DefaultValueFunctions =
struct
  let filter = default_filter
  let backward= default_backward
  let compare = default_compare
  let eval_ext man e = None
  let backward_ext man e ve v = None
  let compare_ext man op b e1 v1 e2 v2 = None
  let avalue avk v = None
  let ask man q = None
end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)

let values : (module VALUE) list ref = ref []

let register_value_abstraction dom =
  values := dom :: !values

let find_value_abstraction name =
  List.find (fun dom ->
      let module D = (val dom : VALUE) in
      compare D.name name = 0
    ) !values

let mem_value_abstraction name =
  List.exists (fun dom ->
      let module D = (val dom : VALUE) in
      compare D.name name = 0
    ) !values

let value_abstraction_names () =
  List.map (fun dom ->
      let module D = (val dom : VALUE) in
      D.name
    ) !values
