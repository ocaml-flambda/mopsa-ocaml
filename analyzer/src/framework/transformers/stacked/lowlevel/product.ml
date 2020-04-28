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
open Sig.Stacked.Lowlevel
open Stack_list
open Log




(** Specification of a reduced product *)
module type SPEC =
sig
  type t
  val pool : t stack_list
  val erules : (module EVAL_REDUCTION) list
  val srules : (module EXEC_REDUCTION) list
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

  let alarms =
    List.sort_uniq compare
      (fold_module
      { f = fun (type a) (m: a stack) acc ->
            let module S = (val m) in
            S.alarms @ acc
      }
      Spec.pool [])


  let interface =
    fold_module {
      f = fun (type a) (m: a stack) acc ->
        let module S = (val m) in
        Interface.concat acc S.interface
    } Spec.pool Interface.empty


  let bottom : t =
    make {
      f = fun (type a) (m:a stack) ->
        let module S = (val m) in
        S.bottom
    } Spec.pool


  let top : t =
    make {
      f = fun (type a) (m:a stack) ->
        let module S = (val m) in
        S.top
    } Spec.pool


  let print fmt a =
    iter {
      f = fun (type a) (m: a stack) aa ->
        let module S = (val m) in
        S.print fmt aa
    } Spec.pool a


  let is_bottom a =
    exists {
      f = fun (type a) (m: a stack) aa ->
        let module S = (val m) in
        S.is_bottom aa
    } Spec.pool a



  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset man ctx a1 a2 =
    let b, (a1, a2) = for_all_fold_man {
        f = fun (type a) (m: a stack) man (a1,a2) ->
          let module S = (val m) in
          let b,a1,a2 = S.subset man ctx a1 a2 in
          b, (a1,a2)
      } Spec.pool man (a1,a2)
    in
    b, a1, a2


  let join man ctx a1 a2 =
    let a, (a1,a2) = make_fold_man {
        f = fun (type a) (m: a stack) man (a1,a2) ->
          let module S = (val m) in
          let a,a1,a2 = S.join man ctx a1 a2 in
          a,(a1,a2)
      } Spec.pool man (a1, a2) in
    a,a1,a2


  let meet man ctx a1 a2 =
    let a, (a1,a2) = make_fold_man {
        f = fun (type a) (m: a stack) man (a1,a2) ->
          let module S = (val m) in
          let a,a1,a2 = S.meet man ctx a1 a2 in
          a,(a1,a2)
      } Spec.pool man (a1, a2) in
    a,a1,a2


  let widen man ctx a1 a2 =
    let a, (a1,a2,stable) = make_fold_man {
        f = fun (type a) (m: a stack) man (a1,a2,stable) ->
          let module S = (val m) in
          let a,a1,a2,stable' = S.widen man ctx a1 a2 in
          a,(a1,a2,stable&&stable')
      } Spec.pool man (a1, a2, true) in
    a,a1,a2,stable


  let merge pre (a1,log1) (a2,log2) =
    if a1 == a2 then a1 else
    if Log.is_empty_log log1 then a2 else
    if Log.is_empty_log log2 then a1 else
    if (Log.compare_log log1 log2 = 0) then a1
    else
      let f = fun (type a) (m:a stack) p a1 a2 (log1,log2)->
        let module S = (val m) in
        let l1 = Log.get_left_log log1 in
        let l2 = Log.get_left_log log2 in
        S.merge p (a1,l1) (a2,l2), (Log.get_right_log log1, Log.get_right_log log2)
      in
      let a, _ = map_fold3 { f } Spec.pool pre a1 a2 (log1,log2) in
      a



  (** {2 Initialization procedure} *)
  (** **************************** *)

  let init prog (man:('a,t,'s) man) flow =
    fold_man {
      f = fun (type a) (m:a stack) man flow ->
        let module S = (val m) in
        S.init prog man flow
    } Spec.pool man flow



  (** {2 Merging functions} *)
  (** ********************* *)

  (** Merge the conflicts of two flows using logs *)
  let merge_flows ~merge_alarms man pre (flow1,log1) (flow2,log2) =
    let ctx = Context.get_most_recent (Flow.get_ctx flow1) (Flow.get_ctx flow2) |>
              Context.get_unit
    in
    Flow.map2zo
      (fun _ a1 -> man.lattice.bottom)
      (fun _ a2 -> man.lattice.bottom)
      (fun tk a1 a2 ->
         match tk with
         (* Logs concern only cur environments *)
         | T_cur ->
           (* Merge the cur environments *)
           let p = Flow.get T_cur man.lattice pre in
           man.lattice.merge p (a1,log1) (a2,log2)

         (* For the other tokens, compute the meet of the environments *)
         | _ -> man.lattice.meet ctx a1 a2
      ) merge_alarms flow1 flow2



  (** Merge the conflicts between distinct domains in a pointwise result *)
  let merge_inter_conflicts man pre (pointwise:('a,'r) cases option list) : ('a,'r option option list) cases =
    let rec aux : type t. t stack_list -> ('a,'r) cases option list -> ('a,t,'s) man -> ('a,'r option option list * alarm_class list) cases =
      fun pool pointwise man ->
        match pointwise, pool with
        | [None], _ ->
          Cases.singleton ([None],[]) pre

        | [Some r], Cons(s,Nil) ->
          r |> Cases.bind @@ fun rr flow ->
          let module S = (val s) in
          Cases.singleton ([Some rr],S.alarms) flow

        | None :: tl, Cons(hds,tls) ->
          aux tls tl (tlman man) |>
          Cases.bind @@ fun after flow ->
          let after,alarms = OptionExt.none_to_exn after in
          Cases.singleton (None :: after, alarms) flow

        | Some r :: tl, Cons(hds,tls) ->
          aux tls tl (tlman man) |>
          Cases.bind_full @@ fun after after_flow after_log after_cleaners ->
          let after,alarms = OptionExt.none_to_exn after in
          r |> Cases.bind_full @@ fun rr flow log cleaners ->
          let module S = (val hds) in
          if after |> List.exists (function Some _ -> true | None -> false) then
            let hdman = hdman man in
            let after_flow = Flow.set T_cur (
                let cur = Flow.get T_cur man.lattice flow in
                let after_cur = Flow.get T_cur man.lattice after_flow in
                hdman.set (hdman.get cur) after_cur
              ) man.lattice after_flow
            in
            let common_alarms = List.filter (fun a -> List.mem a alarms) S.alarms in
            let merge_alarms a1 a2 =
              let a1', a1'' = AlarmSet.partition (fun a -> List.mem (get_alarm_class a) common_alarms) a1 in
              let a2', a2'' = AlarmSet.partition (fun a -> List.mem (get_alarm_class a) common_alarms) a2 in
              AlarmSet.inter a1' a2' |>
              AlarmSet.union a1'' |>
              AlarmSet.union a2''
            in
            let flow = merge_flows ~merge_alarms man pre (flow,log) (after_flow,after_log) in
            let log = Log.meet_log log after_log in
            let cleaners = cleaners @ after_cleaners in
            Cases.return (Some (Some rr :: after, S.alarms @ alarms |> List.sort_uniq compare)) flow ~cleaners ~log
          else
            Cases.return (Some (Some rr :: after, S.alarms @ alarms |> List.sort_uniq compare)) flow ~cleaners ~log


        | _ -> assert false
    in
    aux Spec.pool pointwise man |>
    Cases.map (fun (r,alarms) -> r)



  (** Merge the conflicts emerging from the same domain *)
  let merge_intra_conflicts man pre (r:('a,'r) cases) : ('a,'r) cases =
    Cases.map_fold_conjunctions (fun (flow1,log1) (flow2,log2) ->
        merge_flows ~merge_alarms:AlarmSet.inter man pre (flow1,log1) (flow2,log2)
      ) r


  (** {2 Abstract transformer} *)
  (** ************************ *)

  (** Manager used by reductions *)
  let rman (man:('a,t,'s) man) : ('a,'s) rman = {
    lattice = man.lattice;
    post = man.post;
    get_eval = (
      let f : type t. t id -> prod_eval -> expr option =
        fun id evals ->
          let rec aux : type t tt. t id -> tt stack_list -> prod_eval -> expr option =
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
          let rec aux : type t tt. t id -> tt stack_list -> prod_eval -> prod_eval =
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
          let rec aux : type t tt. t id -> tt stack_list -> ('a,tt,'s) man -> ('a,t,'s) man =
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


  (** Return a coverage bit mask indicating which domains provide an
     [exec] transfer function for [zone]
  *)
  let get_exec_coverage zone : bool list =
    make_list {
      f = fun (type a) (m:a stack) ->
        let module S = (val m) in
        Interface.sat_exec zone S.interface
    } Spec.pool


  (* Apply [exec] transfer function pointwise over all domains *)
  let exec_pointwise zone coverage stmt man flow : 'a post option list option =
    let posts, ctx = fold_man_pair {
        f = fun (type a) (m:a stack) (man:('a,a,'s) man) covered (acc,ctx) ->
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
      } (combine Spec.pool coverage) man ([], Flow.get_ctx flow)
    in
    let posts = List.map (OptionExt.lift (Post.set_ctx ctx)) posts |>
                List.rev
    in
    if List.exists (function Some _ -> true | None -> false) posts
    then Some posts
    else None


  (** Simplify a pointwise post-state by changing lists of unit into unit *)
  let simplify_pointwise_post (pointwise:('a,unit option option list) cases) : 'a post =
    pointwise |> Cases.bind @@ fun r flow ->
    let rr = r |> OptionExt.lift (fun rr -> ()) in
    Cases.return rr flow


  (** Apply reduction rules on a post-conditions *)
  let reduce_post stmt man pre post =
    let rman = rman man in
    List.fold_left (fun pointwise rule ->
        let module R = (val rule : EXEC_REDUCTION) in
        Post.bind (R.reduce stmt rman pre) post
      ) post Spec.srules


  (** Entry point of abstract transformers *)
  let exec zone =
    let coverage = get_exec_coverage zone in
    (fun stmt man flow ->
       exec_pointwise zone coverage stmt man flow |>
       OptionExt.lift @@ fun pointwise ->
       merge_inter_conflicts man flow pointwise |>
       simplify_pointwise_post |>
       merge_intra_conflicts man flow |>
       reduce_post stmt man flow
    )


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (* Compute the coverage bit mask of domains providing an [eval] for [zone] *)
  let get_eval_coverage zone : bool list =
    make_list {
      f = fun (type a) (m:a stack) ->
        let module S = (val m) in
        Interface.sat_eval zone S.interface
    } Spec.pool


  (** Compute pointwise evaluations over the pool of domains *)
  let eval_pointwise zone coverage exp man flow : 'a eval option list option =
    let pointwise, ctx = fold_man_pair {
        f = fun (type a) (m:a stack) (man:('a,a,'s) man) covered (acc,ctx) ->
          let module S = (val m) in
          if not covered then
            None :: acc, ctx
          else
            let flow' = Flow.set_ctx ctx flow in
            match S.eval zone exp man flow' with
            | None -> None :: acc, ctx
            | Some evl ->
              let evl = Eval.remove_duplicates man.lattice evl in
              let ctx' = Eval.get_ctx evl in
              Some evl :: acc, ctx'
      } (combine Spec.pool coverage) man ([], Flow.get_ctx flow)
    in
    let pointwise = List.map (OptionExt.lift (Eval.set_ctx ctx)) pointwise |>
                    List.rev
    in
    if List.exists (function Some _ -> true | None -> false) pointwise
    then Some pointwise
    else None


  (** Apply reduction rules on a pointwise evaluation *)
  let reduce_pointwise_eval exp man (pointwise:('a,expr option option list) cases) : 'a eval =
    let rman = rman man in
    (* Let reduction rules roll out imprecise evaluations from [pointwise] *)
    let pointwise = List.fold_left (fun pointwise rule ->
        let module R = (val rule : EVAL_REDUCTION) in
        pointwise |> Cases.bind_some @@ fun el flow ->
        R.reduce exp rman el flow
      ) pointwise Spec.erules
    in
    (* For performance reasons, we keep only one evaluation in each conjunction.
       THE CHOICE IS ARBITRARY: keep the first non-None result using the
       order of domains in the configuration file.
    *)
    let evl = pointwise |> Cases.map_opt (fun el ->
        try List.find (function Some _ -> true | None -> false) el
        with Not_found -> None
      )
    in
    Eval.remove_duplicates man.lattice evl



  (** Entry point of abstract evaluations *)
  let eval zone =
    let coverage = get_eval_coverage zone in
    (fun exp man flow ->
       eval_pointwise zone coverage exp man flow |>
       OptionExt.lift @@ fun pointwise ->
       merge_inter_conflicts man flow pointwise |>
       reduce_pointwise_eval exp man |>
       merge_intra_conflicts man flow
    )


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow =
    fold_man {
      f = fun (type a) (m:a stack) (man:('a,a,'s) man) acc ->
        let module S = (val m) in
        S.ask query man flow |>
        OptionExt.neutral2 (meet_query query) acc
    } Spec.pool man None


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


type pool = P : 'a stack_list -> pool

let type_stack (type a) (s : (module STACK with type t = a)) =
    let module S = (val s) in
    (module S : STACK with type t = a)

let rec type_stack_pool : (module STACK) list -> pool = function
  | [] -> P Nil
  | hd :: tl ->
    let module S = (val hd) in
    let s = type_stack (module S) in
    let P tl = type_stack_pool tl in
    P (Cons (s, tl))

let make
    (stacks: (module STACK) list)
    (erules: (module EVAL_REDUCTION) list)
    (srules: (module EXEC_REDUCTION) list)
  : (module STACK) =

  let P pool = type_stack_pool stacks in

  let create_product (type a) (pool: a stack_list) =
    let module S = Make(
      struct
        type t = a
        let pool = pool
        let erules = erules
        let srules = srules
      end)
    in
    (module S : STACK)
  in

  create_product pool
