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

type 'v vexpr =
  | Map of ('v * 'v vexpr) ExprMap.t

let empty_vexpr = Map ExprMap.empty

let singleton_vexpr e v ve =
  Map (ExprMap.singleton e (v,ve))

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

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  type t
  (** Type of the abstract value. *)

  val id : t id
  (** Identifier of the value domain *)

  val accept_type : typ -> bool
  (** Predicate of types abstracted by the value domain *)

  val name : string
  (** Name of the value domain *)

  val display : string
  (** Display name used in debug messages *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: 'a ctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Local semantics} *)
  (** ******************* *)

  val eval : ('v,t) value_man -> expr -> t
  (** Forward evaluation of expressions *)

  val filter : ('v,t) value_man -> bool -> expr -> t option
  (** Keep values that may represent the argument truth value of an expression *)

  val backward : ('v,t) value_man -> expr -> t vexpr -> 'v -> t vexpr
  (** Backward evaluation of expressions.
      [backward man e ve r] returns the values of the sub-expressions such that
      the evaluation of the expression is in [r]
      i.e., we filter the sub-values [ve] knowing that, after
      applying the evaluating the expression, the result is in [r]
  *)

  val compare : ('v,t) value_man -> operator -> bool -> expr -> t -> expr -> t -> (t * t)
  (** Backward evaluation of boolean comparisons.
      [compare man op true e1 v1 e2 v2] returns (v1',v2') where:
       - v1' abstracts the set of v  in v1 such that v1' op v' is true for some v' in v2'
       - v2' abstracts the set of v' in v2 such that v2' op v' is true for some v  in v1'
       i.e., we filter the abstract values v1 and v2 knowing that the test is true
  *)

  val avalue : 'r avalue_kind -> t -> 'r option

  (** {2 Extended semantics} *)
  (** ********************** *)

  val eval_ext : ('v,t) value_man -> expr -> 'v option

  val backward_ext : ('v,t) value_man -> expr -> 'v vexpr -> 'v -> 'v vexpr option

  val compare_ext : ('v,t) value_man -> operator -> bool -> expr -> 'v -> expr -> 'v -> ('v * 'v) option


  (** {2 Communication handlers } *)
  (** *************************** *)

  val ask : ('v,t) value_man -> ('a,'r) query -> 'r option
  (** Handler of reduction hints *)


  (** {2 Pretty printer} *)
  (** ****************** *)

  val print: printer -> t -> unit
  (** Printer of an abstract element. *)

end



module DefaultValueFunctions =
struct
  let filter man b e = None
  let eval_ext man e = None
  let backward_ext man e ve r = None
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
