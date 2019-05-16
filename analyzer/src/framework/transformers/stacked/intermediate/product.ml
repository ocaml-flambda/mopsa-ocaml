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



(****************************************************************************)
(**                      {2 List representation}                            *)
(****************************************************************************)


(** Abstract stack module *)
type 't smodule = (module STACK with type t = 't)


(** List of stack modules *)
type _ slist =
  | Nil : unit slist
  | Cons : 't smodule * 'b slist -> ('t * 'b) slist



type 'b map = {
  f: 't. 't smodule -> 'b;
}


let slist_map f l =
  let rec aux : type t. t slist -> 'b list =
    fun l ->
      match l with
      | Nil -> []
      | Cons(hd,tl) ->
        f.f hd :: aux tl
  in
  aux l


type ('a,'b) map_combined = {
  f: 't. 't smodule -> 'a -> 'b;
}


let slist_map_combined f l1 l2 =
  let rec aux : type t. t slist -> 'a list -> 'b list =
    fun l1 l2 ->
      match l1, l2 with
      | Nil, [] -> []
      | Cons(hd1,tl1), hd2 :: tl2 ->
        f.f hd1 hd2 :: aux tl1 tl2
      | _ -> assert false
  in
  aux l1 l2


type 'b fold = {
  f: 't. 't smodule -> 'b -> 'b;
}


let slist_fold f l init =
  let rec aux : type t. t slist -> 'b -> 'b =
    fun l acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd acc in
        aux tl acc'
  in
  aux l init


type ('a,'b) fold_combined = {
  f: 't. 't smodule -> 'a -> 'b -> 'b;
}


let slist_fold_combined f l1 l2 init =
  let rec aux : type t. t slist -> 'a list -> 'b -> 'b =
    fun l1 l2 acc ->
      match l1, l2 with
      | Nil, [] -> acc
      | Cons(hd1,tl1), hd2::tl2 ->
        let acc' = f.f hd1 hd2 acc in
        aux tl1 tl2 acc'
      | _ -> assert false
  in
  aux l1 l2 init


type ('a,'b) fold_sub2 = {
  f: 't. 't smodule -> 'a -> 't * 'b -> 't * 'b -> 'a * 'b * 'b;
}


let slist_fold_sub2 f l init (a1,s1) (a2,s2) =
  let rec aux : type t. t slist -> 'a -> t * 'b -> t * 'b -> 'a * 'b * 'b =
    fun l acc (a1,s1) (a2,s2) ->
      match l,a1,a2 with
      | Nil,(),() -> acc,s1,s2
      | Cons(hd,tl), (hda1,tla1), (hda2,tla2) ->
        let acc,s1,s2 = f.f hd acc (hda1,s1) (hda2,s2) in
        aux tl acc (tla1,s1) (tla2,s2)
  in
  aux l init (a1,s1) (a2,s2)


type 'b apply_sub2 = {
  f: 't. 't smodule -> 't * 'b -> 't * 'b -> 't * 'b * 'b;
}


let slist_apply_sub2 f l (a1,s1) (a2,s2) =
  let rec aux : type t. t slist -> t * 'b -> t * 'b -> t * 'b * 'b =
    fun l (a1,s1) (a2,s2) ->
      match l,a1,a2 with
      | Nil,(),() -> (),s1,s2
      | Cons(hd,tl), (hda1,tla1), (hda2,tla2) ->
        let hda,s1,s2 = f.f hd (hda1,s1) (hda2,s2) in
        let tla,s1,s2 = aux tl (tla1,s1) (tla2,s2) in
        (hda,tla), s1, s2
  in
  aux l (a1,s1) (a2,s2)



type ('ext,'b) fold_apply_sub_ext2 = {
  f: 't. 't smodule -> 't * 'b -> 't * 'b -> 'ext -> 't * 'b * 'b * 'ext;
}


let slist_fold_apply_sub_ext2 f l (a1,s1) (a2,s2) ext =
  let rec aux : type t. t slist -> t * 'b -> t * 'b -> 'ext -> t * 'b * 'b * 'ext=
    fun l (a1,s1) (a2,s2) ext ->
      match l,a1,a2 with
      | Nil,(),() -> (),s1,s2,ext
      | Cons(hd,tl), (hda1,tla1), (hda2,tla2) ->
        let hda,s1,s2,ext = f.f hd (hda1,s1) (hda2,s2) ext in
        let tla,s1,s2,ext = aux tl (tla1,s1) (tla2,s2) ext in
        (hda,tla), s1, s2,ext
  in
  aux l (a1,s1) (a2,s2) ext



type create = {
  f: 't. 't smodule -> 't;
}

let slist_create f l =
  let rec aux : type t. t slist -> t =
    fun l ->
      match l with
      | Nil -> ()
      | Cons(hd,tl) ->
        f.f hd, aux tl
  in
  aux l



type 'a print = {
  f: 't. 't smodule -> Format.formatter -> 't -> unit;
}


(** Print an abstract value *)
let slist_print f l sep fmt a =
  let rec aux : type t. t slist -> Format.formatter -> t -> unit =
    fun l fmt a ->
      match l, a with
      | Nil, () -> ()
      | Cons(m,Nil), (aa, ()) -> f.f m fmt aa
      | Cons(hd,tl), (hda,tla) ->
        f.f hd fmt hda;
        Format.fprintf fmt "%s" sep;
        aux tl fmt tla
  in
  aux l fmt a


type 'a pred = {
  f: 't. 't smodule -> 't -> bool;
}

(** Test an ∃ predicate *)
let slist_exists f l a =
  let rec aux : type t. t slist -> t -> bool =
    fun l a ->
      match l, a with
      | Nil, () -> false
      | Cons(hd,tl), (hda,tla) ->
        f.f hd hda || aux tl tla
  in
  aux l a


let hdman man = {
  man with
  get = (fun a -> man.get a |> fst);
  set = (fun hd a -> man.set (hd, man.get a |> snd) a);
  get_log = (fun log -> man.get_log log |> Log.first);
  set_log = (fun l log -> man.set_log (Log.tuple (l, man.get_log log |> Log.second)) log);
}

let tlman man = {
  man with
  get = (fun a -> man.get a |> snd);
  set = (fun tl a -> man.set (man.get a |> fst, tl) a);
  get_log = (fun log -> man.get_log log |> Log.second);
  set_log = (fun l log -> man.set_log (Log.tuple (man.get_log log |> Log.first, l)) log);
}


type ('a,'b,'s) man_fold = {
  f: 't. 't smodule -> ('a, 't,'s) man -> 'b -> 'b;
}

let slist_man_fold f l man init =
  let rec aux : type t. t slist -> ('a,t,'s) man -> 'b -> 'b =
    fun l man acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd (hdman man) acc in
        aux tl (tlman man) acc'
  in
  aux l man init


type ('a,'b,'s) man_map = {
  f: 't. 't smodule -> ('a, 't,'s) man -> 'b;
}

let slist_man_map f l man =
  let rec aux : type t. t slist -> ('a,t,'s) man -> 'b list =
    fun l man ->
      match l with
      | Nil -> []
      | Cons(hd,tl) ->
        f.f hd (hdman man) :: aux tl (tlman man)
  in
  aux l man


type ('a,'b,'c,'s) man_map_combined = {
  f: 't. 't smodule -> 'b -> ('a,'t,'s) man -> 'c;
}


let slist_man_map_combined f l1 l2 man =
  let rec aux : type t. t slist -> 'b list -> ('a,t,'s) man -> 'c list =
    fun l1 l2 man ->
      match l1, l2 with
      | Nil, [] -> []
      | Cons(hd1,tl1), hd2 :: tl2 ->
        f.f hd1 hd2 (hdman man) :: aux tl1 tl2 (tlman man)
      | _ -> assert false
  in
  aux l1 l2 man


type ('a,'b,'c,'s) man_fold_combined = {
  f: 't. 't smodule -> 'b -> ('a,'t,'s) man -> 'c -> 'c;
}


let slist_man_fold_combined f l1 l2 man init =
  let rec aux : type t. t slist -> 'b list -> ('a,t,'s) man -> 'c -> 'c =
    fun l1 l2 man acc ->
      match l1, l2 with
      | Nil, [] -> acc
      | Cons(hd1,tl1), hd2::tl2 ->
        let acc' = f.f hd1 hd2 (hdman man) acc in
        aux tl1 tl2 (tlman man) acc'
      | _ -> assert false
  in
  aux l1 l2 man init




(****************************************************************************)
(**                       {2 Domain Transformer}                            *)
(****************************************************************************)


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

  let merge ctx pre (a1,log1) (a2,log2) =
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
           debug "%s turn" S.name;
           match S.exec zone stmt man flow' with
           | None -> (None :: acc, ctx)
           | Some post ->
             let ctx' = Post.choose_ctx post in
             (Some post :: acc, ctx')
       in

       let pointwise_posts, ctx = slist_man_fold_combined { f } Spec.pool coverage man ([], Flow.get_ctx flow) in

       (* Merge post-conditions *)
       let f = fun (type a) (m:a smodule) post (man:('a,a,'s) man) acc ->
         Option.neutral2 (fun post acc ->
             let module S = (val m) in
             (* Patch the accumulated post by:
                a. putting the newly state of S
                b. merging the sub-tree state
             *)
             Post.merge (fun tk (a,log) (a',log') ->
                 (* a. Update the state of S *)
                 let a' = man.set (man.get a) a' in

                 debug "@[<v 2>merging token %a@, a = @[%a@]@, log = @[%a@]@, a' = @[%a@]@, log' = @[%a@]@]"
                   pp_token tk
                   man.lattice.print a
                   Log.print log
                   man.lattice.print a'
                   Log.print log'
                 ;


                 (* b. Merge the sub-tree *)
                 let pre = Flow.get tk man.lattice flow |> man.get_sub in
                 let slog = man.get_sub_log log in
                 let slog' = man.get_sub_log log' in

                 debug "slog = @[%a@]" Log.print slog;
                 debug "slog' = @[%a@]" Log.print slog';

                 let merged = man.merge_sub
                     (Context.get_unit ctx)
                     pre
                     (man.get_sub a, slog)
                     (man.get_sub a', slog')
                 in

                 let aa = man.set_sub merged a in

                 debug "merged@, a = %a@, a' = %a@, aa = %a" man.lattice.print a man.lattice.print a' man.lattice.print aa;
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
