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

(** [Product ∈ (𝒱 × ... × 𝒱) × (𝓡 × ... × 𝓡)→ 𝒱] creates an n-ary reduced
    product from n value abstractions and m reduction rules.
*)

open Ast.All
open Core.All
open Core.Sig.Value.Lowlevel
open Core.Sig.Value.Reduction
open Utils.Value_list


(** Specification of a reduced product *)
module type SPEC =
sig
  type t
  val pool : t vlist
  val rules : (module REDUCTION) list
end



(** Factory functor *)
module Make(Spec: SPEC) : VALUE with type t = Spec.t =
struct

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  type t = Spec.t

  include GenValueId(
    struct
      type typ = t

      let name = "framework.combiners.value.product"

      let display =
        let f = fun (type a) (m:a vmodule) ->
          let module Value = (val m) in
          Value.display
        in
        let l = vlist_map { f } Spec.pool in
        "(" ^ (String.concat " ∧ " l) ^ ")"
    end
    )

  let zones =
    let f = fun (type a) (m:a vmodule) ->
      let module Value = (val m) in
      Value.zones
    in
    vlist_map { f } Spec.pool |>
    List.flatten

  let types =
    let f = fun (type a) (m:a vmodule) ->
        let module Value = (val m) in
        Value.types
    in
    vlist_map { f } Spec.pool |>
    List.flatten


  let bottom =
    let f = fun (type a) (m:a vmodule) ->
      let module Value = (val m) in
      Value.bottom
    in
    vlist_apply { f } Spec.pool


  let top =
    let f = fun (type a) (m:a vmodule) ->
      let module Value = (val m) in
      Value.top
    in
    vlist_apply { f } Spec.pool


  let print fmt v =
    let f = fun (type a) (m:a vmodule) fmt (v:a) ->
      let module Value = (val m) in
      Value.print fmt v
    in
    vlist_print { f } Spec.pool fmt " ∧ " v

  let is_bottom v =
    let f = fun (type a) (m:a vmodule) (v:a) ->
      let module Value = (val m) in
      Value.is_bottom v
    in
    vlist_exists { f } Spec.pool v

  let subset v v' =
    let f = fun (type a) (m:a vmodule) (v:a) (v':a) ->
      let module Value = (val m) in
      Value.subset v v'
    in
    vlist_all2 { f } Spec.pool v v'

  let join v v' =
    let f = fun (type a) (m:a vmodule) (v:a) (v':a) ->
      let module Value = (val m) in
      Value.join v v'
    in
    vlist_apply2 { f } Spec.pool v v'

  let meet v v' =
    vlist_apply2 { f = (fun (type a) (m:a vmodule) (v:a) (v':a) ->
        let module Value = (val m) in
        Value.meet v v'
      )} Spec.pool v v'

  let widen ctx v v' =
    let f = fun (type a) (m:a vmodule) (v:a) (v':a) ->
      let module Value = (val m) in
      Value.widen ctx v v'
    in
    vlist_apply2 { f } Spec.pool v v'

  let mem_types types t =
    List.exists (fun  t' -> compare_typ t t' = 0) types

  let of_constant t c =
    let f = fun (type a) (m:a vmodule) ->
      let module Value = (val m) in
      if mem_types Value.types t then
        Value.of_constant t c
      else
        Exceptions.panic "of_constant called on unsupported constant %a of type %a"
          ~loc:__LOC__
          pp_constant c
          pp_typ t
    in
    vlist_apply { f } Spec.pool

  let vrman = {
    get = (fun (type a) (id:a Core.Id.value) (v:t) ->
        let rec aux : type b. b vlist -> b -> a =
          fun l v ->
            match l, v with
            | Nil, () -> raise Not_found
            | Cons(hd,tl), (vhd,vtl) ->
              let module Value = (val hd) in
              match Core.Id.veq Value.id id with
              | Some Eq.Eq -> vhd
              | None -> aux tl vtl
        in
        aux Spec.pool v
      );
    set = (fun (type a) (id:a Core.Id.value) (x:a) (v:t) ->
        let rec aux : type b. b vlist -> b -> b =
          fun l v ->
            match l, v with
            | Nil, () -> raise Not_found
            | Cons(hd,tl), (vhd,vtl) ->
              let module Value = (val hd) in
              match Core.Id.veq Value.id id with
              | Some Eq.Eq -> x,vtl
              | None -> vhd,aux tl vtl
        in
        aux Spec.pool v
      )
  }


  let reduce (v:t) : t =
    let apply v =
      List.fold_left (fun acc r ->
          let module R = (val r : REDUCTION) in
          R.reduce vrman acc
        ) v Spec.rules
    in
    let rec lfp v =
      let v' = apply v in
      if subset v v' then v else lfp v'
    in
    lfp v

  let reduce_pair (v1,v2) =
    let v1' = reduce v1 in
    let v2' = reduce v2 in
    (v1',v2')


  let unop man t op v =
    let f = fun (type a) (m:a vmodule) man ->
      let module Value = (val m) in
      if mem_types Value.types t then
        Value.unop man t op v
      else
        Exceptions.panic "unop called on unsupported operator %a of type %a"
          ~loc:__LOC__
          pp_operator op
          pp_typ t
    in
    vlist_man_apply { f } Spec.pool man |>
    reduce


  let binop man t op v1 v2 =
    let f = fun (type a) (m:a vmodule) man ->
      let module Value = (val m) in
      if mem_types Value.types t then
        Value.binop man t op v1 v2
      else
        Exceptions.panic "binop called on unsupported operator %a of type %a"
          ~loc:__LOC__
          pp_operator op
          pp_typ t
    in
    vlist_man_apply { f } Spec.pool man |>
    reduce

  let filter man v b =
    let f = fun (type a) (m:a vmodule) man ->
      let module Value = (val m) in
      Value.filter man v b
    in
    vlist_man_apply { f } Spec.pool man |>
    reduce

  let bwd_unop man t op v r =
    let f = fun (type a) (m:a vmodule) man ->
      let module Value = (val m) in
      if mem_types Value.types t then
        Value.bwd_unop man t op v r
      else
        Exceptions.panic "bwd_unop called on unsupported operator %a of type %a"
          ~loc:__LOC__
          pp_operator op
          pp_typ t
    in
    vlist_man_apply { f } Spec.pool man |>
    reduce

  let bwd_binop man t op v1 v2 r =
    let f = fun (type a) (m:a vmodule) man ->
      let module Value = (val m) in
      if mem_types Value.types t then
        Value.bwd_binop man t op v1 v2 r
      else
        Exceptions.panic "bwd_binop called on unsupported operator %a of type %a"
          ~loc:__LOC__
          pp_operator op
          pp_typ t
    in
    vlist_man_apply_pair { f } Spec.pool man |>
    reduce_pair


  let compare man t op v1 v2 r =
    let f = fun (type a) (m:a vmodule) man ->
      let module Value = (val m) in
      if mem_types Value.types t then
        Value.compare man t op v1 v2 r
      else
        Exceptions.panic "compare called on unsupported operator %a of type %a"
          ~loc:__LOC__
          pp_operator op
          pp_typ t
    in
    vlist_man_apply_pair { f } Spec.pool man |>
    reduce_pair

  let cast man id v =
    let f = fun (type a) (m:a vmodule) man ->
      let module Value = (val m) in
      Value.cast man id v
    in
    vlist_ret_man_opt { f } Spec.pool man


  let ask man query =
    let f = fun (type a) (m:a vmodule) man ->
      let module Value = (val m) in
      Value.ask man query
    in
    let replies = vlist_map_man_opt { f } Spec.pool man in
    match replies with
    | [] -> None
    | [hd] -> Some hd
    | hd :: tl ->
      let r =
        List.fold_left (fun acc r -> Query.join query acc r) hd tl
      in
      Some r

  let refine man channel v =
    let f = fun (type a) (m:a vmodule) man acc ->
      let module Value = (val m) in
      Core.Channel.bind (Value.refine man channel) acc
    in
    vlist_man_fold { f } Spec.pool man (Core.Channel.return v)

end




(** Factory function *)

type vpool = V : 'a vlist -> vpool

let type_value (type a) (v : (module VALUE with type t = a)) =
    let module V = (val v) in
    (module V : VALUE with type t = a)

let rec type_value_pool : (module VALUE) list -> vpool = function
  | [] -> V Nil
  | hd :: tl ->
    let module V = (val hd) in
    let v = type_value (module V) in
    let V tl = type_value_pool tl in
    V (Cons (v, tl))

let make
    (values: (module VALUE) list)
    (rules: (module REDUCTION) list)
  : (module VALUE) =

  let V pool = type_value_pool values in

  let create_product (type a) (pool: a vlist) =
    let module V = Make(
      struct
        type t = a
        let pool = pool
        let rules = rules
      end)
    in
    (module V : VALUE)
  in

  create_product pool
