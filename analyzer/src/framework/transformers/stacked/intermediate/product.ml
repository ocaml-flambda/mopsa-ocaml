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

(** The transformer [Product ∈ (𝒮 × ... × 𝒮) × (𝓡 × ... × 𝓡) → 𝒮] creates
    an n-ary reduced product of stack domains, refined by a set of reduction
    rules.
*)


open Ast.All
open Core.All
open Sig.Stacked.Reduction
open Sig.Stacked.Intermediate
open Log



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

  let subset sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) acc (a1,s1) (a2,s2) ->
      let module S = (val m) in
      let b, s1, s2 = S.subset sman ctx (a1,s1) (a2,s2) in
      b && acc, s1, s2
    in
    slist_fold_sub2 { f } Spec.pool true (a1,s1) (a2,s2)

  let join sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) (a1,s1) (a2,s2) ->
      let module S = (val m) in
      S.join sman ctx (a1,s1) (a2,s2)
    in
    slist_apply_sub2 { f } Spec.pool (a1,s1) (a2,s2)

  let meet sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) (a1,s1) (a2,s2) ->
      let module S = (val m) in
      S.meet sman ctx (a1,s1) (a2,s2)
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

  let init prog (man:('a,t,'s) man) flow =
    let f = fun (type a) (m:a smodule) (man:('a,a,'s) man) flow ->
      let module S = (val m) in
      S.init prog man flow
    in
    slist_man_fold { f } Spec.pool man flow


  (** {2 Abstract transformer} *)
  (** ************************ *)

  (** Entry point of abstract transformers *)
  let exec zone =

    (* Compute the coverage mask of the required zone *)
    let coverage =
      let f = fun (type a) (m:a smodule) ->
        let module S = (val m) in
        Interface.sat_exec zone S.interface
      in
      slist_map { f } Spec.pool
    in

    (fun stmt man flow : 'a post option ->

       (* Compute the list of post-conditions by pointwise application.
          Most recent context is propagated through applications *)
       let f = fun (type a) (m:a smodule) covered (man:('a,a,'s) man) (acc,ctx) ->
         if not covered then
           (None :: acc, ctx)
         else
           let module S = (val m) in
           let flow' = Flow.set_ctx ctx flow in
           match S.exec zone stmt man flow' with
           | None -> (None :: acc, ctx)
           | Some post ->
             let post' = log_post_stmt stmt man post in
             let ctx' = Post.choose_ctx post' in
             (Some post' :: acc, ctx')
       in

       let pointwise_posts, ctx = slist_man_fold_combined { f } Spec.pool coverage man ([], Flow.get_ctx flow) in

       (* Merge post-conditions *)
       let f = fun (type a) (m:a smodule) post (man:('a,a,'s) man) acc ->
         Option.neutral2 (fun post acc ->
             let module S = (val m) in
             (* Patch the accumulated post by:
                a. putting the newly state of S
                b. merging the sub-tree state
                c. meet the other states
             *)
             Post.merge (fun tk (a,log) (a',log') ->
                 debug "merging@, a = %a@, log = %a@, a' = %a@, log' = %a"
                   man.lattice.print a
                   Log.print log
                   man.lattice.print a'
                   Log.print log'
                 ;

                 (* a. Update the state of S *)
                 let a' = man.set (man.get a) a' in

                 (* b. Merge the sub-tree *)
                 let pre = Flow.get tk man.lattice flow |> man.get_sub in
                 let slog = man.get_sub_log log in
                 let slog' = man.get_sub_log log' in
                 let merged = man.merge_sub
                     pre
                     (man.get_sub a, slog)
                     (man.get_sub a', slog')
                 in

                 (* Meet the other states *)
                 let a = man.set_sub merged a in
                 let a' = man.set_sub merged a' in

                 debug "merge@, a = %a@, a' = %a" man.lattice.print a man.lattice.print a';

                 let aa = man.lattice.meet (Context.get_unit ctx) a a' in
                 debug "meet@, aa = %a" man.lattice.print aa;

                 aa, man.set_sub_log (Log.concat slog slog') log'
               ) post acc
           ) post acc
       in
       slist_man_fold_combined { f } Spec.pool pointwise_posts man None
    )


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Entry point of abstract evaluations *)
  let eval zone =

    (* Compute the coverage mask of the required zone *)
    let coverage =
      let f = fun (type a) (m:a smodule) ->
        let module S = (val m) in
        Interface.sat_eval zone S.interface
      in
      slist_map { f } Spec.pool
    in

    (fun exp man flow : (expr,'a) eval option ->

       (* Compute the list of evaluations by pointwise application.
          Most recent context is propagated through applications. 
       *)
       let f = fun (type a) (m:a smodule) covered (man:('a,a,'s) man) (acc,ctx) ->
         if not covered then
           None :: acc, ctx
         else
           let module S = (val m) in
           let flow' = Flow.set_ctx ctx flow in
           match S.eval zone exp man flow' with
           | None -> None :: acc, ctx
           | Some evl ->
             let ctx' = Eval.choose_ctx evl in
             Some evl :: acc, ctx'
       in

       let pointwise_evl, _ = slist_man_fold_combined { f } Spec.pool coverage man ([], Flow.get_ctx flow) in

       (* Meet evaluations *)
       let rec aux = function
         | [] -> None
         | None :: tl -> aux tl
         | Some evl :: tl ->
           match aux tl with
           | None -> Some evl
           | Some evl' ->
             Some (Eval.meet evl evl')
       in
       aux pointwise_evl
    )


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow =
    let f = fun (type a) (m:a smodule) (man:('a,a,'s) man) acc ->
      let module S = (val m) in
      S.ask query man flow |>
      Option.neutral2 (meet_query query) acc
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
