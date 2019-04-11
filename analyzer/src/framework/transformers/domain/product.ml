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
open Sig.Domain.Lowlevel
open Sig.Domain.Reduction
open Utils.Domain_list



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

  let interface =
    let f = fun (type a) (m:a dmodule) acc ->
      let module Domain = (val m) in
      Interface.concat acc Domain.interface
    in
    dlist_fold { f } Spec.pool Interface.empty



  (** {2 Lattice operators} *)
  (** ********************* *)

  let bottom : t =
    let f = fun (type a) (m:a dmodule) ->
      let module Domain = (val m) in
      Domain.bottom
    in
    dlist_apply { f } Spec.pool


  let top : t =
    let f = fun (type a) (m:a dmodule) ->
      let module Domain = (val m) in
      Domain.top
    in
    dlist_apply { f } Spec.pool


  let print man fmt a =
    let f = fun (type a) (m: a dmodule) man fmt ->
      let module Domain = (val m) in
      Domain.print man fmt a
    in
    dlist_man_print { f } Spec.pool man fmt ""


  let is_bottom man a =
    let f = fun (type a) (m: a dmodule) man ->
      let module Domain = (val m) in
      Domain.is_bottom man a
    in
    dlist_man_exists { f } Spec.pool man

  let subset man a1 a2 =
    let f = fun (type a) (m: a dmodule) man ->
      let module Domain = (val m) in
      Domain.subset man a1 a2
    in
    dlist_man_forall { f } Spec.pool man

  let join man a1 a2 =
    let f = fun (type a) (m: a dmodule) man ->
      let module Domain = (val m) in
      Domain.join man a1 a2
    in
    dlist_man_apply { f } Spec.pool man

  let meet man a1 a2 =
    let f = fun (type a) (m: a dmodule) man ->
      let module Domain = (val m) in
      Domain.meet man a1 a2
    in
    dlist_man_apply { f } Spec.pool man

  let widen man ctx a1 a2 =
    let f = fun (type a) (m: a dmodule) man ->
      let module Domain = (val m) in
      Domain.widen man ctx a1 a2
    in
    dlist_man_apply { f } Spec.pool man

  let merge man pre (post1,log1) (post2,log2) =
    let f = fun (type a) (m: a dmodule) man ->
      let module Domain = (val m) in
      Domain.merge man pre (post1,log1) (post2,log2)
    in
    dlist_man_apply { f } Spec.pool man


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog man flow =
    let f = fun (type a) (m: a dmodule) man acc ->
      let module Domain = (val m) in
      Domain.init prog man acc
    in
    dlist_man_fold { f } Spec.pool man flow

  let reduce stmt man flow =
    Post.return flow

  let exec zone stmt man flow =
    let f = fun (type a) (m: a dmodule) man acc ->
      let module Domain = (val m) in
      let post = Domain.exec zone stmt man flow in
      Option.neutral2 (fun post acc ->
          Post.merge (fun tk (a,log) (acc,acclog) ->
              let acc' = man.set (man.get a) acc in
              let acclog' = man.set_log (man.get_log log) acclog in
              acc', acclog'
            ) post acc
        ) post acc
    in
    dlist_man_fold { f } Spec.pool man None |>
    Option.lift (Post.bind @@ reduce stmt man)


  let eval zone exp man flow =
    Exceptions.panic ~loc:__LOC__ "eval of reduced products not implemented"

  let ask query man flow =
    let f = fun (type a) (m: a dmodule) man acc ->
      let module Domain = (val m) in
      let rep = Domain.ask query man flow in
      Option.neutral2 (fun rep acc ->
          Query.meet query rep acc
        ) rep acc
    in
    dlist_man_fold { f } Spec.pool man None


  (** {2 Reduction refinement} *)
  (** ************************ *)

  let refine channel man flow =
    assert false

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
