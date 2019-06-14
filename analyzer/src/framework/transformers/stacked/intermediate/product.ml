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
open Sig.Stacked.Intermediate
module R = Sig.Stacked.Reduction
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


let map f l =
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


let map_combined f l1 l2 =
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


let fold f l init =
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


let fold_combined f l1 l2 init =
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


type ('a,'b) fold_ext2 = {
  f: 't. 't smodule -> 'a -> 't * 'b -> 't * 'b -> 'a * 'b * 'b;
}


let fold_ext2 f l init (a1,s1) (a2,s2) =
  let rec aux : type t. t slist -> 'a -> t * 'b -> t * 'b -> 'a * 'b * 'b =
    fun l acc (a1,s1) (a2,s2) ->
      match l,a1,a2 with
      | Nil,(),() -> acc,s1,s2
      | Cons(hd,tl), (hda1,tla1), (hda2,tla2) ->
        let acc,s1,s2 = f.f hd acc (hda1,s1) (hda2,s2) in
        aux tl acc (tla1,s1) (tla2,s2)
  in
  aux l init (a1,s1) (a2,s2)


type 'b apply_ext2 = {
  f: 't. 't smodule -> 't * 'b -> 't * 'b -> 't * 'b * 'b;
}


let apply_ext2 f l (a1,s1) (a2,s2) =
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



type ('ext,'b) fold_apply_ext2 = {
  f: 't. 't smodule -> 't * 'b -> 't * 'b -> 'ext -> 't * 'b * 'b * 'ext;
}


let fold_apply_ext2 f l (a1,s1) (a2,s2) ext =
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

let create f l =
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
let print f l sep fmt a =
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
let exists f l a =
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

let man_fold f l man init =
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

let man_map f l man =
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


let man_map_combined f l1 l2 man =
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


let man_fold_combined f l1 l2 man init =
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
  val erules : (module R.EREDUCTION) list
end


(** Product functor *)
module Make(Spec:SPEC) : STACK with type t = Spec.t =
struct


  (** {2 Stack header} *)
  (** **************** *)

  type t = Spec.t

  include Core.Id.GenDomainId(
    struct
      type nonrec t = t
      let name = "transformers.stacked.intermediate.product"
    end
    )

  let interface =
    let f = fun (type a) (m:a smodule) acc ->
      let module S = (val m) in
      Interface.concat acc S.interface
    in
    fold { f } Spec.pool Interface.empty

  let bottom : t =
    let f = fun (type a) (m:a smodule) ->
      let module S = (val m) in
      S.bottom
    in
    create { f } Spec.pool

  let top : t =
    let f = fun (type a) (m:a smodule) ->
      let module S = (val m) in
      S.top
    in
    create { f } Spec.pool


  let print fmt a =
    let f = fun (type a) (m: a smodule) fmt aa ->
      let module S = (val m) in
      S.print fmt aa
    in
    print { f } Spec.pool "" fmt a

  let is_bottom a =
    let f = fun (type a) (m: a smodule) aa ->
      let module S = (val m) in
      S.is_bottom aa
    in
    exists { f } Spec.pool a


  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) acc (a1,s1) (a2,s2) ->
      let module S = (val m) in
      let b, s1, s2 = S.subset sman ctx (a1,s1) (a2,s2) in
      b && acc, s1, s2
    in
    fold_ext2 { f } Spec.pool true (a1,s1) (a2,s2)

  let join sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) (a1,s1) (a2,s2) ->
      let module S = (val m) in
      S.join sman ctx (a1,s1) (a2,s2)
    in
    apply_ext2 { f } Spec.pool (a1,s1) (a2,s2)

  let meet sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) (a1,s1) (a2,s2) ->
      let module S = (val m) in
      S.meet sman ctx (a1,s1) (a2,s2)
    in
    apply_ext2 { f } Spec.pool (a1,s1) (a2,s2)

  let widen sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a smodule) (a1,s1) (a2,s2) stable ->
      let module S = (val m) in
      let a, s1, s2, stable' = S.widen sman ctx (a1,s1) (a2,s2) in
      a, s1, s2, stable && stable'
    in
    fold_apply_ext2 { f } Spec.pool (a1,s1) (a2,s2) true

  let merge pre (a1,log1) (a2,log2) =
    Exceptions.panic ~loc:__LOC__ "merge not implemented"


  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog (man:('a,t,'s) man) flow =
    let f = fun (type a) (m:a smodule) (man:('a,a,'s) man) flow ->
      let module S = (val m) in
      S.init prog man flow
    in
    man_fold { f } Spec.pool man flow


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
      map { f } Spec.pool
    in

    (fun stmt man flow : 'a post option ->

       (* Compute the list of post-conditions by pointwise application.
          Most recent context is propagated through applications *)
       let f = fun (type a) (m:a smodule) covered (man:('a,a,'s) man) (acc,ctx) ->
         let module S = (val m) in
         if not covered then
           (S.name, None) :: acc, ctx
         else
           let flow' = Flow.set_ctx ctx flow in
           debug "exec %a in domain %s" pp_stmt stmt S.name;
           match S.exec zone stmt man flow' with
           | None -> (S.name, None) :: acc, ctx
           | Some post ->
             let ctx' = Post.get_ctx post in
             (S.name, Some post) :: acc, ctx'
       in

       let pointwise_posts, ctx = man_fold_combined { f } Spec.pool coverage man ([], Flow.get_ctx flow) in
       let pointwise_posts = List.rev pointwise_posts |>
                             List.map (fun (name, post) -> (name, post |> Option.lift (Post.set_ctx ctx)))
       in

       debug "@[<v 2>pointwise posts:@,%a"
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
            (fun fmt (name, post) ->
               Format.fprintf fmt "%s: %a"
                 name
                 (Option.print (Post.print man.lattice)) post
            )
         )
         pointwise_posts
       ;

       (* Merge post-conditions *)
       let f = fun (type a) (m:a smodule) (_,post) (man:('a,a,'s) man) acc ->
         let module S = (val m) in
         debug "Patching domain %s" S.name;
         debug "post = %a" (Option.print (Post.print man.lattice)) post;
         debug "acc = %a" (Option.print (Post.print man.lattice)) acc;
         Option.neutral2 (fun post acc ->
             (* Patch the accumulated post by:
                a. putting the newly state of S
                b. merging the sub-tree state
             *)
             Post.merge (fun tk (a,log) (a',log') ->
                 debug "@[<v 2>merging token %a@, a = @[%a@]@, log = @[%a@]@, a' = @[%a@]@, log' = @[%a@]@]"
                   pp_token tk
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

                 debug "slog = @[%a@]" Log.print slog;
                 debug "slog' = @[%a@]" Log.print slog';

                 let merged = man.merge_sub
                     pre
                     (man.get_sub a, slog)
                     (man.get_sub a', slog')
                 in

                 let aa = man.set_sub merged a' in

                 debug "merged@, a = %a@, a' = %a@, aa = %a"
                   man.lattice.print a
                   man.lattice.print a'
                   man.lattice.print aa
                 ;
                 aa, man.set_sub_log (Log.concat slog slog') log'
               ) post acc
           ) post acc
       in
       man_fold_combined { f } Spec.pool pointwise_posts man None
    )


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Manager used by evaluation reductions *)
  let eman (man:('a,t,'s) man) : ('a,'s) R.eman = R.{
      lattice = man.lattice;
      exec = man.exec;
      get_eval = (
        let f : type t. t domain -> (expr,'a) peval -> (expr,'a) eval option =
          fun id evals ->
            let rec aux : type t tt. t domain -> tt slist -> (expr,'a) peval -> (expr,'a) eval option =
              fun id l e ->
                match l, e with
                | Nil, [] -> None
                | Cons(hd,tl), (hde::tle) ->
                  begin
                    let module D = (val hd) in
                    match domain_id_eq D.id id with
                    | Some Eq -> hde
                    | None -> aux id tl tle
                  end
                | _ -> assert false
            in
            aux id Spec.pool evals
        in
        f
      );
      set_eval = (
        let f : type t. t domain -> (expr,'a) eval option -> (expr,'a) peval -> (expr,'a) peval =
          fun id evl evals ->
            let rec aux : type t tt. t domain -> tt slist -> (expr,'a) peval -> (expr,'a) peval =
              fun id l e ->
                match l, e with
                | Nil, [] -> raise Not_found
                | Cons(hd,tl), (hde::tle) ->
                  begin
                    let module D = (val hd) in
                    match domain_id_eq D.id id with
                    | Some Eq -> evl :: tle
                    | None -> hde :: aux id tl tle
                  end
                | _ -> assert false
            in
            aux id Spec.pool evals
        in
        f
      );

      get_man = (
        let f : type t. t domain -> ('a,t,'s) man =
          fun id ->
            let rec aux : type t tt. t domain -> tt slist -> ('a,tt,'s) man -> ('a,t,'s) man =
              fun id l man ->
                match l with
                | Nil -> raise Not_found
                | Cons(hd,tl) ->
                  let module D = (val hd) in
                  match domain_id_eq D.id id with
                  | Some Eq -> (hdman man)
                  | None -> aux id tl (tlman man)
            in
            aux id Spec.pool man
        in
        f
      );

  }


  (** Reduce evaluations using the registered rules *)
  let reduce_eval exp man pevl =
    let eman = eman man in
    List.fold_left (fun acc r ->
        let module R = (val r : R.EREDUCTION) in
        R.reduce exp eman acc
      ) pevl Spec.erules


  (** Entry point of abstract evaluations *)
  let eval zone =

    (* Compute the coverage mask of the required zone *)
    let coverage =
      let f = fun (type a) (m:a smodule) ->
        let module S = (val m) in
        Interface.sat_eval zone S.interface
      in
      map { f } Spec.pool
    in

    (fun exp man flow : (expr,'a) eval option ->

       (* Compute the list of evaluations by pointwise application.
          Most recent context is propagated through applications.
       *)
       let f = fun (type a) (m:a smodule) covered (man:('a,a,'s) man) (acc,ctx) ->
         let module S = (val m) in
         if not covered then
           (S.name, None) :: acc, ctx
         else
           let flow' = Flow.set_ctx ctx flow in
           debug "eval %a in domain %s" pp_expr exp S.name;
           match S.eval zone exp man flow' with
           | None -> (S.name, None) :: acc, ctx
           | Some evl ->
             let ctx' = Eval.get_ctx evl in
             (S.name, Some evl) :: acc, ctx'
       in

       let pointwise_evl, ctx = man_fold_combined { f } Spec.pool coverage man ([], Flow.get_ctx flow) in
       let pointwise_evl = List.rev pointwise_evl |>
                           List.map (fun (name, evl) -> (name, evl |> Option.lift (Eval.set_ctx ctx)))
       in

       debug "@[<v 2>pointwise evaluations:@,%a@]"
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
            (fun fmt (name, evl) ->
               Format.fprintf fmt "%s: %a"
                 name
                 (Option.print (Eval.print ~pp:pp_expr)) evl
            )
         )
         pointwise_evl
       ;

       let pointwise_evl = List.map snd pointwise_evl in

       (* Turn the point-wise evaluations into a dnf of singleton point-wise evaluations *)
       let dnf : (expr, 'a) R.peval Dnf.t =
         let rec aux = function
           | [] -> Dnf.singleton []

           | None :: tl ->
             aux tl |>
             Dnf.map (fun after -> None :: after)

           | Some evl :: tl ->
             let dnf = Eval.to_dnf evl in
             aux tl |>
             Dnf.bind (fun after ->
                 Dnf.map (fun (e,flow,cleaners) ->
                     Some (Eval.case e flow ~cleaners) :: after
                   ) dnf
               )
         in
         aux pointwise_evl
       in


       (* Reduce each pointwise evaluation *)
       let reduced_dnf = Dnf.map (reduce_eval exp man) dnf in

       (* Meet evaluations *)
       let rec aux : ('e,'a) R.peval -> ('e,'a) eval option = function
         | [] -> None
         | None :: tl -> aux tl
         | Some evl :: tl ->
           match aux tl with
           | None -> Some evl
           | Some evl' ->
             Some (Eval.meet evl evl')
       in
       let ret = Dnf.apply aux (Option.neutral2 Eval.join) (Option.neutral2 Eval.meet) reduced_dnf in
       debug "reduced evaluations: %a" (Option.print (Eval.print ~pp:pp_expr)) ret;
       ret
    )


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow =
    let f = fun (type a) (m:a smodule) (man:('a,a,'s) man) acc ->
      let module S = (val m) in
      S.ask query man flow |>
      Option.neutral2 (meet_query query) acc
    in
    man_fold { f } Spec.pool man None


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
    (erules: (module R.EREDUCTION) list)
  : (module STACK) =

  let S pool = type_stack_pool stacks in

  let create_product (type a) (pool: a slist) =
    let module S = Make(
      struct
        type t = a
        let pool = pool
        let erules = erules
      end)
    in
    (module S : STACK)
  in

  create_product pool
