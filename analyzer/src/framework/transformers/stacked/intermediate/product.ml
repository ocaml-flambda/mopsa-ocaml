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

(* The pool of a reduced product [S1 * ... * Sn] is represented as an
   heterogenous list [(t1 * (t2 * ... * (tn * unit))) pool] where
   each type [ti] is the type of the stack [Si].
*)


(** Abstract stack module *)
type 't stack = (module STACK with type t = 't)


(** List of stack modules *)
type _ pool =
  | Nil : unit pool
  | Cons : 't stack * 'b pool -> ('t * 'b) pool



type 'b map = {
  f: 't. 't stack -> 'b;
}


let map f l =
  let rec aux : type t. t pool -> 'b list =
    fun l ->
      match l with
      | Nil -> []
      | Cons(hd,tl) ->
        f.f hd :: aux tl
  in
  aux l


type ('a,'b) map_combined = {
  f: 't. 't stack -> 'a -> 'b;
}


let map_combined f l1 l2 =
  let rec aux : type t. t pool -> 'a list -> 'b list =
    fun l1 l2 ->
      match l1, l2 with
      | Nil, [] -> []
      | Cons(hd1,tl1), hd2 :: tl2 ->
        f.f hd1 hd2 :: aux tl1 tl2
      | _ -> assert false
  in
  aux l1 l2


type 'b fold = {
  f: 't. 't stack -> 'b -> 'b;
}


let fold f l init =
  let rec aux : type t. t pool -> 'b -> 'b =
    fun l acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd acc in
        aux tl acc'
  in
  aux l init


type ('a,'b) fold_combined = {
  f: 't. 't stack -> 'a -> 'b -> 'b;
}


let fold_combined f l1 l2 init =
  let rec aux : type t. t pool -> 'a list -> 'b -> 'b =
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
  f: 't. 't stack -> 'a -> 't * 'b -> 't * 'b -> 'a * 'b * 'b;
}


let fold_ext2 f l init (a1,s1) (a2,s2) =
  let rec aux : type t. t pool -> 'a -> t * 'b -> t * 'b -> 'a * 'b * 'b =
    fun l acc (a1,s1) (a2,s2) ->
      match l,a1,a2 with
      | Nil,(),() -> acc,s1,s2
      | Cons(hd,tl), (hda1,tla1), (hda2,tla2) ->
        let acc,s1,s2 = f.f hd acc (hda1,s1) (hda2,s2) in
        aux tl acc (tla1,s1) (tla2,s2)
  in
  aux l init (a1,s1) (a2,s2)


type 'b apply_ext2 = {
  f: 't. 't stack -> 't * 'b -> 't * 'b -> 't * 'b * 'b;
}


let apply_ext2 f l (a1,s1) (a2,s2) =
  let rec aux : type t. t pool -> t * 'b -> t * 'b -> t * 'b * 'b =
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
  f: 't. 't stack -> 't * 'b -> 't * 'b -> 'ext -> 't * 'b * 'b * 'ext;
}


let fold_apply_ext2 f l (a1,s1) (a2,s2) ext =
  let rec aux : type t. t pool -> t * 'b -> t * 'b -> 'ext -> t * 'b * 'b * 'ext=
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
  f: 't. 't stack -> 't;
}

let create f l =
  let rec aux : type t. t pool -> t =
    fun l ->
      match l with
      | Nil -> ()
      | Cons(hd,tl) ->
        f.f hd, aux tl
  in
  aux l



type 'a print = {
  f: 't. 't stack -> Format.formatter -> 't -> unit;
}


(** Print an abstract value *)
let print f l sep fmt a =
  let rec aux : type t. t pool -> Format.formatter -> t -> unit =
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
  f: 't. 't stack -> 't -> bool;
}

(** Test an ∃ predicate *)
let exists f l a =
  let rec aux : type t. t pool -> t -> bool =
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
  f: 't. 't stack -> ('a, 't,'s) man -> 'b -> 'b;
}

let man_fold f l man init =
  let rec aux : type t. t pool -> ('a,t,'s) man -> 'b -> 'b =
    fun l man acc ->
      match l with
      | Nil -> acc
      | Cons(hd,tl) ->
        let acc' = f.f hd (hdman man) acc in
        aux tl (tlman man) acc'
  in
  aux l man init


type ('a,'b,'s) man_map = {
  f: 't. 't stack -> ('a, 't,'s) man -> 'b;
}

let man_map f l man =
  let rec aux : type t. t pool -> ('a,t,'s) man -> 'b list =
    fun l man ->
      match l with
      | Nil -> []
      | Cons(hd,tl) ->
        f.f hd (hdman man) :: aux tl (tlman man)
  in
  aux l man


type ('a,'b,'c,'s) man_map_combined = {
  f: 't. 't stack -> 'b -> ('a,'t,'s) man -> 'c;
}


let man_map_combined f l1 l2 man =
  let rec aux : type t. t pool -> 'b list -> ('a,t,'s) man -> 'c list =
    fun l1 l2 man ->
      match l1, l2 with
      | Nil, [] -> []
      | Cons(hd1,tl1), hd2 :: tl2 ->
        f.f hd1 hd2 (hdman man) :: aux tl1 tl2 (tlman man)
      | _ -> assert false
  in
  aux l1 l2 man


type ('a,'b,'c,'s) man_fold_combined = {
  f: 't. 't stack -> 'b -> ('a,'t,'s) man -> 'c -> 'c;
}


let man_fold_combined f l1 l2 man init =
  let rec aux : type t. t pool -> 'b list -> ('a,t,'s) man -> 'c -> 'c =
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
  val pool : t pool
  val erules : (module R.EREDUCTION) list
end


(** Product functor *)
module Make(Spec:SPEC) : STACK with type t = Spec.t =
struct


  (** {2 Declaration header} *)
  (** ********************** *)

  type t = Spec.t

  include Core.Id.GenDomainId(
    struct
      type nonrec t = t
      let name = "transformers.stacked.intermediate.product"
    end
    )

  let interface =
    let f = fun (type a) (m:a stack) acc ->
      let module S = (val m) in
      Interface.concat acc S.interface
    in
    fold { f } Spec.pool Interface.empty

  let bottom : t =
    let f = fun (type a) (m:a stack) ->
      let module S = (val m) in
      S.bottom
    in
    create { f } Spec.pool

  let top : t =
    let f = fun (type a) (m:a stack) ->
      let module S = (val m) in
      S.top
    in
    create { f } Spec.pool


  let print fmt a =
    let f = fun (type a) (m: a stack) fmt aa ->
      let module S = (val m) in
      S.print fmt aa
    in
    print { f } Spec.pool "" fmt a

  let is_bottom a =
    let f = fun (type a) (m: a stack) aa ->
      let module S = (val m) in
      S.is_bottom aa
    in
    exists { f } Spec.pool a


  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a stack) acc (a1,s1) (a2,s2) ->
      let module S = (val m) in
      let b, s1, s2 = S.subset sman ctx (a1,s1) (a2,s2) in
      b && acc, s1, s2
    in
    fold_ext2 { f } Spec.pool true (a1,s1) (a2,s2)

  let join sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a stack) (a1,s1) (a2,s2) ->
      let module S = (val m) in
      S.join sman ctx (a1,s1) (a2,s2)
    in
    apply_ext2 { f } Spec.pool (a1,s1) (a2,s2)

  let meet sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a stack) (a1,s1) (a2,s2) ->
      let module S = (val m) in
      S.meet sman ctx (a1,s1) (a2,s2)
    in
    apply_ext2 { f } Spec.pool (a1,s1) (a2,s2)

  let widen sman ctx (a1,s1) (a2,s2) =
    let f = fun (type a) (m: a stack) (a1,s1) (a2,s2) stable ->
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
    let f = fun (type a) (m:a stack) (man:('a,a,'s) man) flow ->
      let module S = (val m) in
      S.init prog man flow
    in
    man_fold { f } Spec.pool man flow



  (** {2 Merging functions} *)
  (** ********************* *)

  (** Merge the conflicts of two flows using logs *)
  let merge_flows man pre (flow1,log1) (flow2,log2) =
    let ctx = Context.get_most_recent (Flow.get_ctx flow1) (Flow.get_ctx flow2) |>
              Context.get_unit
    in
    Flow.merge (fun tk oa1 oa2 ->
        match tk, oa1, oa2 with
        (* Logs concern only cur environments *)
        | T_cur, Some a1, Some a2 ->
          (* Merge the shared sub-tree *)
          let p = Flow.get T_cur man.lattice pre |> man.get_sub in
          let slog1 = man.get_sub_log log1 in
          let slog2 = man.get_sub_log log2 in

          let merged = man.merge_sub
              p
              (man.get_sub a1, slog1)
              (man.get_sub a2, slog2)
          in

          let a1 = man.set_sub merged a1 in
          let a2 = man.set_sub merged a2 in

          let a = man.lattice.meet ctx a1 a2 in
          if man.lattice.is_bottom a then None else Some a

        (* For the other tokens, compute the meet of the environments *)
        | _ ->
          Option.absorb2 (fun a1 a2 ->
              let a = man.lattice.meet ctx a1 a2 in
              if man.lattice.is_bottom a then None else Some a
            ) oa1 oa2
      ) AlarmSet.meet man.lattice flow1 flow2



  (** Merge the conflicts between distinct domains in a pointwise result *)
  let rec merge_inter_conflicts man pre (pointwise:('a,'r) result option list) : ('a,'r option option list) result =
    let rec aux : type t. t pool -> ('a,'r) result option list -> ('a,t,'s) man -> ('a,'r option option list) result =
      fun pool pointwise man ->
        match pointwise, pool with
        | [None], _ ->
          Result.singleton [None] pre

        | [Some r], _ ->
          r |> Result.bind @@ fun rr flow ->
          Result.singleton [Some rr] flow

        | None :: tl, Cons(hds,tls) ->
          aux tls tl (tlman man) |>
          Result.bind @@ fun after flow ->
          let after = Option.none_to_exn after in
          Result.singleton (None :: after) flow

        | Some r :: tl, Cons(hds,tls) ->
          aux tls tl (tlman man) |>
          Result.bind_full @@ fun after after_flow after_log after_cleaners ->
          let after = Option.none_to_exn after in
          r |> Result.bind_full @@ fun rr flow log cleaners ->
          if after |> List.exists (function Some _ -> true | None -> false) then
            let hdman = hdman man in
            let after_flow = Flow.set T_cur (
                let cur = Flow.get T_cur man.lattice flow in
                let after_cur = Flow.get T_cur man.lattice after_flow in
                hdman.set (hdman.get cur) after_cur
              ) man.lattice after_flow
            in
            let flow = merge_flows man pre (flow,log) (after_flow,after_log) in
            let log = Log.concat log after_log in
            let cleaners = cleaners @ after_cleaners in
            Result.return (Some (Some rr :: after)) flow ~cleaners ~log
          else
            Result.return (Some (Some rr :: after)) flow ~cleaners ~log


        | _ -> assert false
    in
    aux Spec.pool pointwise man



  (** Merge the conflicts emerging from the same domain *)
  let merge_intra_conflicts man pre (r:('a,'r) result) : ('a,'r) result =
    Result.merge_conjunctions_flow (fun (flow1,log1) (flow2,log2) ->
        merge_flows man pre (flow1,log1) (flow2,log2)
      ) r



  (** {2 Abstract transformer} *)
  (** ************************ *)

  (** Return a coverage bit mask indicating which domains provide an
     [exec] transfer function for [zone]
  *)
  let get_exec_coverage zone : bool list =
    let f = fun (type a) (m:a stack) ->
      let module S = (val m) in
      Interface.sat_exec zone S.interface
    in
    map { f } Spec.pool


  (* Apply [exec] transfer function pointwise over all domains *)
  let exec_pointwise zone coverage stmt man flow : 'a post option list option =
    let f = fun (type a) (m:a stack) covered (man:('a,a,'s) man) (acc,ctx) ->
      let module S = (val m) in
      if not covered then
        None :: acc, ctx
      else
        let flow' = Flow.set_ctx ctx flow in
        match S.exec zone stmt man flow' with
        | None -> None :: acc, ctx
        | Some post ->
          let ctx' = Post.get_ctx post in
          Some post :: acc, ctx'
    in

    let posts, ctx = man_fold_combined { f } Spec.pool coverage man ([], Flow.get_ctx flow) in
    let posts = List.map (Option.lift (Post.set_ctx ctx)) posts |>
                List.rev
    in
    if List.exists (function Some _ -> true | None -> false) posts
    then Some posts
    else None


  (** Simplify a pointwise post-state by changing lists of unit into unit *)
  let simplify_pointwise_post (pointwise:('a,unit option option list) result) : 'a post =
    pointwise |> Result.bind @@ fun r flow ->
    let rr = r |> Option.lift (fun rr -> ()) in
    Result.return rr flow


  (** Entry point of abstract transformers *)
  let exec zone =
    let coverage = get_exec_coverage zone in
    (fun stmt man flow ->
       exec_pointwise zone coverage stmt man flow |>
       Option.lift @@ fun pointwise ->
       merge_inter_conflicts man flow pointwise |>
       simplify_pointwise_post |>
       merge_intra_conflicts man flow
    )


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (* Compute the coverage bit mask of domains providing an [eval] for [zone] *)
  let get_eval_coverage zone : bool list =
    let f = fun (type a) (m:a stack) ->
      let module S = (val m) in
      Interface.sat_eval zone S.interface
    in
    map { f } Spec.pool


  (** Compute pointwise evaluations over the pool of domains *)
  let eval_pointwise zone coverage exp man flow : 'a eval option list option =
    let f = fun (type a) (m:a stack) covered (man:('a,a,'s) man) (acc,ctx) ->
      let module S = (val m) in
      if not covered then
        None :: acc, ctx
      else
        let flow' = Flow.set_ctx ctx flow in
        match S.eval zone exp man flow' with
        | None -> None :: acc, ctx
        | Some evl ->
          let ctx' = Eval.get_ctx evl in
          Some evl :: acc, ctx'
    in

    let pointwise, ctx = man_fold_combined { f } Spec.pool coverage man ([], Flow.get_ctx flow) in
    let pointwise = List.map (Option.lift (Eval.set_ctx ctx)) pointwise |>
                    List.rev
    in
    if List.exists (function Some _ -> true | None -> false) pointwise
    then Some pointwise
    else None


  (** Manager used by evaluation reductions *)
  let eman (man:('a,t,'s) man) : ('a,'s) R.eman = R.{
      lattice = man.lattice;
      post = man.post;
      get_eval = (
        let f : type t. t id -> prod_eval -> expr option =
          fun id evals ->
            let rec aux : type t tt. t id -> tt pool -> prod_eval -> expr option =
              fun id l el ->
                match l, el with
                | Nil, [] -> None
                | Cons(hd,tl), (hde::tle) ->
                  begin
                    let module D = (val hd) in
                    match equal_id D.id id with
                    | Some Eq -> (match hde with None -> None | Some x -> x)
                    | None -> aux id tl tle
                  end
                | _ -> assert false
            in
            aux id Spec.pool evals
        in
        f
      );

      del_eval = (
        let f : type t. t id -> prod_eval -> prod_eval =
          fun id evals ->
            let rec aux : type t tt. t id -> tt pool -> prod_eval -> prod_eval =
              fun id l el ->
                match l, el with
                | Nil, [] -> raise Not_found
                | Cons(hd,tl), (hde::tle) ->
                  begin
                    let module D = (val hd) in
                    match equal_id D.id id with
                    | Some Eq -> None :: tle
                    | None -> hde :: aux id tl tle
                  end
                | _ -> assert false
            in
            aux id Spec.pool evals
        in
        f
      );

      get_man = (
        let f : type t. t id -> ('a,t,'s) man =
          fun id ->
            let rec aux : type t tt. t id -> tt pool -> ('a,tt,'s) man -> ('a,t,'s) man =
              fun id l man ->
                match l with
                | Nil -> raise Not_found
                | Cons(hd,tl) ->
                  let module D = (val hd) in
                  match equal_id D.id id with
                  | Some Eq -> (hdman man)
                  | None -> aux id tl (tlman man)
            in
            aux id Spec.pool man
        in
        f
      );

  }


  (** Apply reduction rules on a pointwise evaluation *)
  let reduce_pointwise_eval exp man (pointwise:('a,expr option option list) result) : 'a eval =
    let eman = eman man in
    (* Let reduction rules roll out imprecise evaluations from [pointwise] *)
    let pointwise = List.fold_left (fun pointwise rule ->
        let module R = (val rule : R.EREDUCTION) in
        pointwise |> Result.bind_some @@ fun el flow ->
        R.reduce exp eman el flow
      ) pointwise Spec.erules
    in
    (* For performance reasons, we keep only one evaluation in each conjunction.
       THE CHOICE IS ARBITRARY: keep the first non-None result using the
       order of domains in the configuration file.
    *)
    pointwise |> Result.map_opt (fun el ->
        try List.find (function Some _ -> true | None -> false) el
        with Not_found -> None
      )


  (** Entry point of abstract evaluations *)
  let eval zone =
    let coverage = get_eval_coverage zone in
    (fun exp man flow ->
       eval_pointwise zone coverage exp man flow |>
       Option.lift @@ fun pointwise ->
       merge_inter_conflicts man flow pointwise |>
       reduce_pointwise_eval exp man |>
       merge_intra_conflicts man flow
    )


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow =
    let f = fun (type a) (m:a stack) (man:('a,a,'s) man) acc ->
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



(****************************************************************************)
(**                      {2 Functional factory}                             *)
(****************************************************************************)

(** The following functions are useful to create a reduced product
    from a list of first-class modules
*)


type spool = S : 'a pool -> spool

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

  let create_product (type a) (pool: a pool) =
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
