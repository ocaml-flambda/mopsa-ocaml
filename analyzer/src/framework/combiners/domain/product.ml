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

(** Reduced product combiner with n-ary reduction rules *)


open Core.All
open Sig.Reduction.Exec
open Sig.Reduction.Eval
open Sig.Combiner.Stacked
open Common


(** Signature of a pool of domains with pointwise transfer functions *)
module type POOL =
sig
  include STACKED_COMBINER
  val alarms : alarm_class list list
  val members : domain list list
  val exec : domain list -> stmt -> ('a,t) man -> 'a flow -> 'a post option list
  val eval : string list -> expr -> ('a,t) man -> 'a flow -> 'a eval option list
end



(** Empty pool *)
module EmptyPool : POOL =
struct
  type t = unit
  let id = C_empty
  let name = "()"
  let domains = []
  let members = []
  let semantics = []
  let routing_table = empty_routing_table
  let alarms = [[]]
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let print fmt () = ()
  let subset _ _ _ ((),s) ((),s') = true,s,s'
  let join _ _ _ ((),s) ((),s') = (),s,s'
  let meet _ _ _ ((),s) ((),s') = (),s,s'
  let widen _ _ _ ((),s) ((),s') = (),s,s',true
  let merge _ _ _ = ()
  let init _ _ flow = flow
  let exec _ _ _ flow = []
  let eval _ _ _ flow = []
  let ask _ _ _ _ = None
end


(** Add a domain to a pool *)
module MakePairPool(S:STACKED_COMBINER)(P:POOL) : POOL with type t = S.t * P.t =
struct
  type t = S.t * P.t
  let id = C_pair(Product,S.id,P.id)
  let domains = S.domains @ P.domains
  let members = S.domains :: P.members
  let semantics = S.semantics @ P.semantics
  let routing_table = join_routing_table S.routing_table P.routing_table
  let alarms = S.alarms :: P.alarms
  let name = S.name ^ " ∧ " ^ P.name

  let print fmt (s,p) =
    match P.id with
    | C_empty  -> Format.fprintf fmt "%a" S.print s
    | C_pair _ -> Format.fprintf fmt "%a@\n%a" S.print s P.print p
    | _ -> assert false

  let bottom = S.bottom, P.bottom
  let top = S.top, P.top

  let is_bottom (s,p) = S.is_bottom s || P.is_bottom p

  let subset man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let b1, s, s' = S.subset (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let b2, s, s' = P.subset (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    b1 && b2, s, s'

  let join man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s' = S.join (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let aa2, s, s' = P.join (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let meet man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s' = S.meet (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let aa2, s, s' = P.meet (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let widen man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s', stable1 = S.widen (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let aa2, s, s', stable2 = P.widen (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s', stable1 && stable2

  let merge (pre1,pre2) ((a1,a2), log) ((a1',a2'), log') =
    S.merge pre1 (a1, Log.get_left_log log) (a1', Log.get_left_log log'),
    P.merge pre2 (a2, Log.get_right_log log) (a2', Log.get_right_log log')

  let init prog man flow =
    S.init prog (fst_pair_man man) flow |>
    P.init prog (snd_pair_man man)

  let exec targets =
    let f2 = P.exec targets in
    if not (sat_targets ~targets ~domains:S.domains) then
      (fun stmt man flow ->
         None :: f2 stmt (snd_pair_man man) flow
      )
    else
      let f1 = S.exec targets in
      (fun stmt man flow ->
         let post = f1 stmt (fst_pair_man man) flow in
         let ctx = OptionExt.apply Cases.get_ctx (Flow.get_ctx flow) post in
         let flow = Flow.set_ctx ctx flow in
         post :: f2 stmt (snd_pair_man man) flow
      )

  let eval targets =
    let f2 = P.eval targets in
    if not (sat_targets ~targets ~domains:S.domains) then
      (fun exp man flow ->
         None :: f2 exp (snd_pair_man man) flow
      )
    else
      let f1 = S.eval targets in
      (fun exp man flow ->
         let eval = f1 exp (fst_pair_man man) flow in
         let ctx = OptionExt.apply Cases.get_ctx (Flow.get_ctx flow) eval in
         let flow = Flow.set_ctx ctx flow in
         eval :: f2 exp (snd_pair_man man) flow
      )

  let ask targets =
    let f2 = P.ask targets in
    if not (sat_targets ~targets ~domains:S.domains) then
      (fun query man flow ->
         f2 query (snd_pair_man man) flow
      )
    else
      let f1 = S.ask targets in
      (fun query man flow ->
         OptionExt.neutral2
           (meet_query query ~meet:(fun a b -> man.lattice.meet (Flow.get_unit_ctx flow) a b))
           (f1 query (fst_pair_man man) flow)
           (f2 query (snd_pair_man man) flow))
end


(** Create a reduced product over a pool and a list of reduction rules *)
module Make
    (Pool:POOL)
    (Rules:sig
       val erules: (module EVAL_REDUCTION) list
       val srules: (module EXEC_REDUCTION) list
     end) : STACKED_COMBINER with type t = Pool.t =
struct

  include Pool

  let alarms = List.flatten Pool.alarms

  (** {2 Merging functions} *)
  (** ********************* *)

  (** Merge the conflicts of two flows using logs *)
  let merge_flows ~merge_alarms (man:('a,'t) man) pre (flow1,log1) (flow2,log2) =
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



  (** Merge the conflicts between distinct domains.
      These conflicts arise from two situations:

      1. When a domain changes its local state, this change is not present in
      the post-state of the other domains. In this case, we need to put the new
      local state of every domain in all other post-states.

      2. When two domains change (independently) the state of a shared sub-abstraction.
      In this case, we use logs to merge the two diverging states.
  *)
  let merge_inter_conflicts man pre (pointwise:('a,'r) cases option list) : ('a,'r option option list) cases =
    let rec aux : type t. t id -> ('a,t) man -> ('a,'r) cases option list -> alarm_class list list -> ('a,'r option option list) cases =
      fun id man pointwise alarms ->
        match pointwise, id, alarms with
        (* Last domain returned no answer *)
        | [None], C_pair(_, s, _), _ ->
          Cases.singleton [None] pre

        (* Last domain returned an answer *)
        | [Some r], C_pair(_, s, _), _ ->
          r >>= fun rr flow ->
          Cases.singleton [Some rr] flow

        (* One domain returned no answer *)
        | None :: tl, C_pair(_,s,pid), _::tlalarms ->
          aux pid (snd_pair_man man) tl tlalarms >>$ fun after flow ->
          Cases.singleton (None :: after) flow

        (* One domain returned an answer.
           Here we need to merge this answer with the answers of the next domains *)
        | Some r :: tl, C_pair(_,s,pid), hdalarms::tlalarms ->
          (* Compute the answer of the next domains *)
          aux pid (snd_pair_man man) tl tlalarms |>
          Cases.bind_full @@ fun after after_flow after_log after_cleaners ->
          r |> Cases.bind_full @@ fun rr flow log cleaners ->
          let after = after |> OptionExt.none_to_exn in
          (* Merge only when the next domains provided some answers *)
          if after |> List.exists (function Some _ -> true | None -> false) then
            (* Resolve the first conflict situation:
               put the post-state of the current domain in the answer of the next domains *)
            let fst_pair_man = fst_pair_man man in
            let after_flow = Flow.set T_cur (
                let cur = Flow.get T_cur man.lattice flow in
                let after_cur = Flow.get T_cur man.lattice after_flow in
                fst_pair_man.set (fst_pair_man.get cur) after_cur
              ) man.lattice after_flow
            in
            (* Resolve the second conflict situation:
               merge the post-states of any shared sub-abstraction *)
            let flow = merge_flows ~merge_alarms:AlarmSet.union man pre (flow,log) (after_flow,after_log) in
            let log = Log.meet_log log after_log in
            let cleaners = cleaners @ after_cleaners in
            Cases.return (Some (Some rr :: after)) flow ~cleaners ~log
          else
            (* Next domains returned no answer, so no merging *)
            Cases.return (Some (Some rr :: after)) flow ~cleaners ~log


        | _ -> assert false
    in
    aux Pool.id man pointwise Pool.alarms



  (** Merge the conflicts emerging from the same domain.
      This kind of conflicts arises when a same domain produces a conjunction of
      post-states.  Since these conjunctions are from the same domain, there is
      no need to merge its local state; we just merge any shared sub-abstraction.
  *)
  let merge_intra_conflicts man pre (r:('a,'r) cases) : ('a,'r) cases =
    Cases.map_fold_conjunctions (fun (flow1,log1) (flow2,log2) ->
        merge_flows ~merge_alarms:AlarmSet.inter man pre (flow1,log1) (flow2,log2)
      ) r



  (** {2 Generic pointwise processing of transfer functions *)
  (** ***************************************************** *)

  (** The successor domain is the domain below the reduced
       product. Since all member domains in the reduced product are at the
       same level, we can pick any one of them *)
  let successor =
    (* XXX This is a hack to be sure to take a member that is a user
       domain, not a composed domain, because `BelowOf` routes are
       defined for user domains only *)
    let member = List.find (function [domain] -> true | _ -> false) Pool.members |>
                 List.hd in
    BelowOf member


  (** Get the context of a pointwise result *)
  let rec get_pointwise_ctx ~default pointwise =
    match pointwise with
    | []               -> default
    | None::tl         -> get_pointwise_ctx ~default tl
    | Some cases :: tl -> Context.get_most_recent
                            (Cases.get_ctx cases)
                            (get_pointwise_ctx ~default tl)

  (** Set the context of a pointwise result *)
  let rec set_pointwise_ctx ctx pointwise =
    match pointwise with
    | []               -> []
    | None :: tl       -> None :: set_pointwise_ctx ctx tl
    | Some cases :: tl -> Some (Cases.set_ctx ctx cases) :: set_pointwise_ctx ctx tl


  (** Apply transfer function [f] pointwise over all domains *)
  let apply_pointwise f arg man flow =
    let pointwise = f arg man flow in
    if List.exists (function Some _ -> true | None -> false) pointwise
    then
      let ctx = get_pointwise_ctx pointwise ~default:(Flow.get_ctx flow) in
      Some (set_pointwise_ctx ctx pointwise)
    else None


  (** Replace missing pointwise results by calling the successor domain *)
  let add_missing_pointwise_results fsuccessor arg pointwise flow =
    (* Call the successor domain only when there are missing results *)
    if List.for_all (function Some _ -> true | None -> false) pointwise then
      pointwise
    else
      let flow = Flow.set_ctx (get_pointwise_ctx pointwise ~default:(Flow.get_ctx flow)) flow in
      let res = fsuccessor arg flow in
      List.map (function None -> Some res | x -> x) pointwise |>
      set_pointwise_ctx (Cases.get_ctx res)



  (** {2 Abstract transformer} *)
  (** ************************ *)

  (** Manager used by reductions *)
  let exec_reduction_man (man:('a, t) man) : 'a exec_reduction_man = {
    get_man = (fun id -> find_domain_man id Pool.id man);
  }


  (** Simplify a pointwise post-state by changing lists of unit into unit *)
  let simplify_pointwise_post (pointwise:('a,unit option option list) cases) : 'a post =
    pointwise |> Cases.bind @@ fun r flow ->
    let rr = r |> OptionExt.lift (fun rr -> ()) in
    Cases.return rr flow


  (** Apply reduction rules on a post-conditions *)
  let reduce_post stmt man pre post =
    let rman = exec_reduction_man man in
    post >>% fun flow ->
    (* Iterate over rules *)
    let rec iter = function
      | [] -> Post.return flow
      | rule::tl ->
        let module R = (val rule : EXEC_REDUCTION) in
        match R.reduce stmt man rman pre flow with
        | None -> iter tl
        | Some post -> post
    in
    iter Rules.srules


  (** Entry point of abstract transformers *)
  let exec targets =
    let f = Pool.exec targets in
    (fun stmt man flow ->
       apply_pointwise f stmt man flow |>
       OptionExt.lift @@ fun pointwise ->
       add_missing_pointwise_results (man.exec ~route:successor) stmt pointwise flow |>
       merge_inter_conflicts man flow |>
       simplify_pointwise_post |>
       merge_intra_conflicts man flow |>
       reduce_post stmt man flow)


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Manager used by reductions *)
  let eval_reduction_man (man:('a, t) man) : 'a eval_reduction_man = {
    get_man = (fun id -> find_domain_man id Pool.id man);
  }


  (** Apply reduction rules on a pointwise evaluation *)
  let reduce_pointwise_eval exp man input (pointwise:('a, expr option option list) cases) : 'a eval =
    pointwise >>$ fun el flow ->
    (* An empty result in a conjunction [el] makes the whole result empty *)
    if List.exists (function Some None -> true | _ -> false) el then
      Eval.empty_singleton flow
    else
    (* Otherwise, keep only cases with non-empty results and remove duplicates *)
    let el' = List.filter (function Some (Some _) -> true | _ -> false) el |>
              List.map (function Some (Some e) -> e | _ -> assert false) |>
              List.sort_uniq compare_expr
    in
    if el' = [] then Eval.empty_singleton flow
    else
      let rman = eval_reduction_man man in
      let rec iter = function
        | [] ->
          (* XXX For performance reasons, we keep only one evaluation in
             each conjunction.
             THE CHOICE IS ARBITRARY!
          *)
          Eval.singleton (List.hd el') flow

        | rule::tl ->
          let module R = (val rule : EVAL_REDUCTION) in
          match R.reduce exp man rman input el' flow with
          | None -> iter tl
          | Some evl -> evl
      in
      iter Rules.erules


  (** Entry point of abstract evaluations *)
  let eval targets =
    let f = Pool.eval targets in
    (fun exp man flow ->
       apply_pointwise f exp man flow |>
       OptionExt.lift @@ fun pointwise ->
       add_missing_pointwise_results (man.eval ~route:successor) exp pointwise flow |>
       merge_inter_conflicts man flow |>
       reduce_pointwise_eval exp man flow |>
       Eval.remove_duplicates man.lattice |>
       merge_intra_conflicts man flow)

end



let rec make_pool : (module STACKED_COMBINER) list -> (module POOL) = function
  | [] -> (module EmptyPool)
  | hd :: tl ->
    let module S = (val hd) in
    let p = make_pool tl in
    (module MakePairPool(S)(val p))


let make
    (domains: (module STACKED_COMBINER) list)
    ~(eval_rules: (module EVAL_REDUCTION) list)
    ~(exec_rules: (module EXEC_REDUCTION) list)
  : (module STACKED_COMBINER) =
  let p = make_pool domains in
  (module Make(val p)
       (struct
         let erules = eval_rules
         let srules = exec_rules
       end)
  )
