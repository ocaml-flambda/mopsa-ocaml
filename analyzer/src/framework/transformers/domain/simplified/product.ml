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

(** The operator [Product ∈ (𝒟 × ... × 𝒟) × (𝓡 × ... × 𝓡) → 𝒟] creates a
    reduced product between n domains using a set of reduction rules.
*)

open Ast.All
open Core.All
open Log
open Sig.Domain.Simplified
open Sig.Domain.Reduction



(** Specification of a reduced product *)
module type SPEC =
sig
  type t
  val pool : t dlist
  val rules : (module REDUCTION) list
end



(** Factory functor *)
module Make(Spec: SPEC) : DOMAIN with type t = Spec.t =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  type t = Spec.t

  include Core.Id.GenDomainId(
    struct
      type typ = t
      let name = "transformers.domain.product"
    end
    )

  let zones =
    let f = fun (type a) (m:a dmodule) acc ->
      let module Domain = (val m) in
      Domain.zones @ acc
    in
    dlist_fold { f } Spec.pool []



  (** {2 Lattice operators} *)
  (** ********************* *)

  let bottom : t =
    let f = fun (type a) (m:a dmodule) ->
      let module Domain = (val m) in
      Domain.bottom
    in
    dlist_create { f } Spec.pool


  let top : t =
    let f = fun (type a) (m:a dmodule) ->
      let module Domain = (val m) in
      Domain.top
    in
    dlist_create { f } Spec.pool


  let print fmt a =
    let f = fun (type a) (m: a dmodule) fmt aa ->
      let module Domain = (val m) in
      Domain.print fmt aa
    in
    dlist_print { f } Spec.pool "" fmt a


  let is_bottom a =
    let f = fun (type a) (m: a dmodule) aa ->
      let module Domain = (val m) in
      Domain.is_bottom aa
    in
    dlist_exists { f } Spec.pool a

  let subset a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.subset aa1 aa2
    in
    dlist_forall2 { f } Spec.pool a1 a2

  let join a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.join aa1 aa2
    in
    dlist_apply2 { f } Spec.pool a1 a2

  let meet a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.meet aa1 aa2
    in
    dlist_apply2 { f } Spec.pool a1 a2

  let widen ctx a1 a2 =
    let f = fun (type a) (m: a dmodule) aa1 aa2 ->
      let module Domain = (val m) in
      Domain.widen ctx aa1 aa2
    in
    dlist_apply2 { f } Spec.pool a1 a2

  let merge pre (post1,log1) (post2,log2) =
    let f = fun (type a) (m: a dmodule) pre post1 post2 ->
      let module Domain = (val m) in
      Domain.merge pre (post1,log1) (post2,log2)
    in
    dlist_apply3 { f } Spec.pool pre post1 post2


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog =
    let f = fun (type a) (m: a dmodule) ->
      let module Domain = (val m) in
      Domain.init prog
    in
    dlist_create { f } Spec.pool


  let ask query a =
    let f = fun (type a) (m: a dmodule) acc aa ->
      let module Domain = (val m) in
      let rep = Domain.ask query aa in
      Option.neutral2 (fun rep acc ->
          Query.meet query rep acc
        ) rep acc
    in
    dlist_fold_apply { f } Spec.pool None a


  let refine channel a =
    let f = fun (type a) (m: a dmodule) aa ->
      let module Domain = (val m) in
      Domain.refine channel aa
    in
    dlist_apply_with_channel { f } Spec.pool a


  let reduction_man : Spec.t man = {
    get = (fun id a ->
        assert false
      );

    set = (fun id v a ->
        assert false
      );

    get_value = (fun id var a ->
        assert false
      );

    set_value = (fun id var v a ->
        assert false
      );

    ask = (fun query a ->
        assert false
      );

    refine = (fun channel a ->
        assert false
      );
  }

  let reduce stmt a =
    List.fold_left (fun acc rule ->
        let module R = (val rule : REDUCTION) in
        R.reduce stmt reduction_man acc
      ) a Spec.rules

  let exec stmt a =
    let f = fun (type a) (m: a dmodule) aa ->
      let module Domain = (val m) in
      Domain.exec stmt aa
    in
    dlist_apply_opt { f } Spec.pool a |>
    Option.lift (reduce stmt)




end




(** Factory function *)

type dpool = D : 'a dlist -> dpool

let type_domain (type a) (d : (module DOMAIN with type t = a)) =
    let module D = (val d) in
    (module D : DOMAIN with type t = a)

let rec type_domain_pool : (module DOMAIN) list -> dpool = function
  | [] -> D Nil
  | hd :: tl ->
    let module D = (val hd) in
    let d = type_domain (module D) in
    let D tl = type_domain_pool tl in
    D (Cons (d, tl))

let make
    (domains: (module DOMAIN) list)
    (rules: (module REDUCTION) list)
  : (module DOMAIN) =

  let D pool = type_domain_pool domains in

  let create_product (type a) (pool: a dlist) =
    let module D = Make(
      struct
        type t = a
        let pool = pool
        let rules = rules
      end)
    in
    (module D : DOMAIN)
  in

  create_product pool
