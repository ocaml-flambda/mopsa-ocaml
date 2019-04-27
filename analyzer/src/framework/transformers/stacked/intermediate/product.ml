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


open Ast.All
open Core.All
open Sig.Stacked.Intermediate
open Sig.Stacked.Reduction
open Core.Manager
open Log

(** The [Sequence.Product ∈ (𝒮 × ... × 𝒮) × (𝓡 × ... × 𝓡) → 𝒮] creates a
    reduced product of stack domains, refined by a set of reduction rules.
*)


(** Specification of a reduced product *)
module type SPEC =
sig
  type t
  val pool : t slist
  val rules : (module REDUCTION) list
end


(** Product functor *)
module Make(Spec:SPEC) : STACK with type t = Spec.t =
struct

  (** {2 Stack header} *)
  (** **************** *)

  type t = Spec.t

  include Core.Id.GenDomainId(
    struct
      type typ = t
      let name = "transformers.stacked.intermediate.product"
    end
    )

  let interface =
    let f = fun (type a) (m:a smodule) acc ->
      let module S = (val m) in
      Interface.concat acc S.interface
    in
    slist_fold { f } Spec.pool Interface.empty

  let bottom : t =
    let f = fun (type a) (m:a smodule) ->
      let module S = (val m) in
      S.bottom
    in
    slist_create { f } Spec.pool

  let top : t =
    let f = fun (type a) (m:a smodule) ->
      let module S = (val m) in
      S.top
    in
    slist_create { f } Spec.pool


  let print fmt a =
    let f = fun (type a) (m: a smodule) fmt aa ->
      let module S = (val m) in
      S.print fmt aa
    in
    slist_print { f } Spec.pool "" fmt a

  let is_bottom a =
    let f = fun (type a) (m: a smodule) aa ->
      let module S = (val m) in
      S.is_bottom aa
    in
    slist_exists { f } Spec.pool a


  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset sman (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) acc (a1,s1) (a2,s2) ->
      let module S = (val m) in
      let b, s1, s2 = S.subset sman (a1,s1) (a2,s2) in
      b && acc, s1, s2
    in
    slist_fold_sub2 { f } Spec.pool true (a1,s1) (a2,s2)

  let join sman (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) (a1,s1) (a2,s2) ->
      let module S = (val m) in
      S.join sman (a1,s1) (a2,s2)
    in
    slist_apply_sub2 { f } Spec.pool (a1,s1) (a2,s2)

  let meet sman (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) (a1,s1) (a2,s2) ->
      let module S = (val m) in
      S.meet sman (a1,s1) (a2,s2)
    in
    slist_apply_sub2 { f } Spec.pool (a1,s1) (a2,s2)

  let widen sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) (a1,s1) (a2,s2) stable ->
      let module S = (val m) in
      let a, s1, s2, stable' = S.widen sman ctx (a1,s1) (a2,s2) in
      a, s1, s2, stable && stable'
    in
    slist_fold_apply_sub_ext2 { f } Spec.pool (a1,s1) (a2,s2) true

  let merge pre (a1,log1) (a2,log2) =
    Exceptions.panic ~loc:__LOC__ "merge not implemented"


  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog man flow =
    let f = fun (type a) (m:a smodule) (man:('a,a) man) flow ->
      let module S = (val m) in
      S.init prog man flow
    in
    slist_man_fold { f } Spec.pool man flow


  (** {2 Abstract transformer} *)
  (** ************************ *)

  (** Entry point of abstract transformers *)
  let exec zone =

    (* Check coverage of exported zones of each stack *)
    let coverage =
      let f = fun (type a) (m:a smodule) ->
        let module S = (val m) in
        Interface.sat_exec zone S.interface
      in
      slist_map { f } Spec.pool
    in

    (fun stmt man sman flow : 'a post option ->
       (* Compute the list of post-conditions by pointwise application *)
       let f = fun (type a) (m:a smodule) covered (man:('a,a) man) ->
         if not covered then None
         else
           let module S = (val m) in
           debug "exec %a in stack %s" pp_stmt stmt S.name;
           let r = S.exec zone stmt man sman flow in
           match r with
           | Some post ->
             debug "stack %s analyzed statement %a" S.name pp_stmt stmt;
             Some (log_post_stmt stmt man post)
           | None ->
             debug "stack %s did not analyze statement %a" S.name pp_stmt stmt;
             None
       in

       let pointwise_posts = slist_man_map_combined { f } Spec.pool coverage man in

       (* Merge post-conditions *)
       let f = fun (type a) (m:a smodule) post man pred ->
         match post with
         | None -> pred
         | Some post ->
           let module S = (val m) in
           (* Put the computed state of S in all previous posts *)
           let pred' =
             pred |> List.map (fun post' ->
                 Post.merge (fun tk (a,log) (a',log') ->
                     man.set (man.get a) a',
                     man.set_log (man.get_log log) log'
                   ) post post'
               )
           in
           post :: pred'
       in
       let merged_posts = slist_man_fold_combined { f } Spec.pool pointwise_posts man [] in

       (* Meet post-conditions *)
       let rec aux = function
         | [] -> None
         | post :: tl ->
           match aux tl with
           | None -> Some post
           | Some post' -> Some (Post.meet man.lattice post post')
       in
       aux merged_posts
    )


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  let eval zone exp man flow =
    Exceptions.panic ~loc:__LOC__ "eval not implemented"


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow =
    let f = fun (type a) (m:a smodule) (man:('a,a) man) acc ->
      let module S = (val m) in
      S.ask query man flow |>
      Option.neutral2 (fun ret1 ret2 ->
          Query.meet query ret1 ret2
        ) acc
    in
    slist_man_fold { f } Spec.pool man None


  (** {2 Broadcast reductions} *)
  (** ************************ *)

  let refine channel man flow =
    Exceptions.panic ~loc:__LOC__ "refine not implemented"


end





(** Factory function *)

type spool = S : 'a slist -> spool

let type_stack (type a) (s : (module STACK with type t = a)) =
    let module S = (val s) in
    (module S : STACK with type t = a)

let rec type_stack_pool : (module STACK) list -> spool = function
  | [] -> S Nil
  | hd :: tl ->
    let module S = (val hd) in
    let s = type_stack (module S) in
    let S tl = type_stack_pool tl in
    S (Cons (s, tl))

let make
    (stacks: (module STACK) list)
    (rules: (module REDUCTION) list)
  : (module STACK) =

  let S pool = type_stack_pool stacks in

  let create_product (type a) (pool: a slist) =
    let module S = Make(
      struct
        type t = a
        let pool = pool
        let rules = rules
      end)
    in
    (module S : STACK)
  in

  create_product pool
