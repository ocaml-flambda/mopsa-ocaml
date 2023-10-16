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


open Mopsa_utils
open Core.All
open Sig.Reduction.Exec
open Sig.Reduction.Eval
open Sig.Combiner.Stacked
open Common
open Location


(** Signature of a pool of domains with pointwise transfer functions *)
module type POOL =
sig
  include STACKED_COMBINER
  val checks : check list list
  val members : DomainSet.t list
  val exec : DomainSet.t option -> stmt -> ('a,t) man -> 'a flow -> 'a post option list
  val eval : DomainSet.t option -> expr -> ('a,t) man -> 'a flow -> 'a eval option list
end



(** Empty pool *)
module EmptyPool : POOL =
struct
  type t = unit
  let id = C_empty
  let name = "()"
  let domains = DomainSet.empty
  let members = []
  let semantics = SemanticSet.empty
  let routing_table = empty_routing_table
  let checks = [[]]
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let subset _ _ _ ((),s) ((),s') = true,s,s'
  let join _ _ _ ((),s) ((),s') = (),s,s'
  let meet _ _ _ ((),s) ((),s') = (),s,s'
  let widen _ _ _ ((),s) ((),s') = (),s,s',true
  let merge _ _ _ = ()
  let init _ _ flow = flow
  let exec _ _ _ flow = []
  let eval _ _ _ flow = []
  let ask _ _ _ _ = None
  let print_state _ _ () = ()
  let print_expr _ _ _ _ _ = ()
end


(** Add a domain to a pool *)
module MakePairPool(S:STACKED_COMBINER)(P:POOL) : POOL with type t = S.t * P.t =
struct
  type t = S.t * P.t
  let id = C_pair(Product,S.id,P.id)
  let domains = DomainSet.union S.domains P.domains
  let members = S.domains :: P.members
  let semantics = SemanticSet.union S.semantics P.semantics
  let routing_table = join_routing_table S.routing_table P.routing_table
  let checks = S.checks :: P.checks
  let name = S.name ^ " ∧ " ^ P.name

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

  let merge (pre1,pre2) ((a1,a2), te) ((a1',a2'), te') =
    S.merge pre1 (a1, get_left_teffect te) (a1', get_left_teffect te'),
    P.merge pre2 (a2, get_right_teffect te) (a2', get_right_teffect te')

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
           Cases.meet
           (f1 query (fst_pair_man man) flow)
           (f2 query (snd_pair_man man) flow))

  let print_state targets =
    let f2 = P.print_state targets in
    if not (sat_targets ~targets ~domains:S.domains) then
      (fun printer (s,p) ->
         f2 printer p
      )
    else
      let f1 = S.print_state targets in
      (fun printer (s,p) ->
         f1 printer s;
         f2 printer p
      )

  let print_expr targets =
    let f2 = P.print_expr targets in
    if not (sat_targets ~targets ~domains:S.domains) then
      (fun man flow printer e ->
         f2 (snd_pair_man man) flow printer e
      )
    else
      let f1 = S.print_expr targets in
      (fun man flow printer e ->
         f1 (fst_pair_man man) flow printer e;
         f2 (snd_pair_man man) flow printer e
      )
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

  let checks = List.flatten Pool.checks

  let debug fmt = Debug.debug ~channel:"framework.combiners.domain.product" fmt

  (** {2 Merging functions} *)
  (** ********************* *)

  (* Combine the reports of the two functions *)
  let merge_report checks1 checks2 report1 report2 =
    map2zo_report
      (fun diag1 ->
         (* Check performed only by the left domain. Here, we are in trouble if the
            right domain is responsible for this check and did nothing! *)
         if List.mem diag1.diag_check checks2 then
           Exceptions.panic "%a: check %a is unsound"
             pp_relative_range diag1.diag_range
             pp_check diag1.diag_check
             (* Otherwise, this is fine! The second domain is not responsible for
                 this check, so add it to the result *)
         else diag1
      )
      (fun diag2 ->
         (* Same dual reasoning here *)
         if List.mem diag2.diag_check checks1 then
           Exceptions.panic "%a: check %a is unsound"
             pp_relative_range diag2.diag_range
             pp_check diag2.diag_check
         else diag2
      )
      (fun diag1 diag2 ->
         (* Check domains responsibility *)
         match List.mem diag2.diag_check checks1, List.mem diag1.diag_check checks2 with
         (* Both domains handle the check => both diagnostics are up-to-date *)
         | true, true   -> meet_diagnostic diag1 diag2
         (* Only left domain handles it => return its diagnostic *)
         | true, false  -> diag1
         (* Only right domain handles it => return its diagnostic *)
         | false, true  -> diag2
         (* None of the domains handle the check, however the check changed!
            This happens when a computation (exec/eval) is performed by one domain only.
            The computation triggered the check and updated the diagnostic.
            One easy and sound solution is to join the diagnostics. *)
         (* XXX Maybe we can be more precise by keeping the newest diagnostic,
            which can be done by comparing with the pre-state report? *)
         | false, false -> join_diagnostic diag1 diag2
      ) report1 report2


  (** Merge the conflicts between distinct domains.
      These conflicts arise from two situations:

      1. When a domain changes its local state, this change is not present in
      the post-state of the other domains. In this case, we need to put the new
      local state of every domain in all other post-states.

      2. When two domains change (independently) the state of a shared sub-abstraction.
      In this case, we use effects to merge the two diverging states.
  *)
  let merge_inter_conflicts man pre (pointwise:('a,'r) cases option list) : ('a,'r option list) cases =
    let rec aux : type t. t id -> ('a,t) man -> ('a,'r) cases option list -> check list list -> ('a,'r option list) cases =
      fun id man pointwise checks ->
        match pointwise, id, checks with
        (* Last domain returned no answer *)
        | [None], C_pair(_, s, _), _ ->
          Cases.return [None] pre

        (* Last domain returned an answer *)
        | [Some r], C_pair(_, s, _), _ ->
          r >>= fun case flow ->
          begin match case with
            | Result(res,_,_) -> Cases.return [Some res] flow
            | Empty           -> Cases.empty flow
            | NotHandled      -> Cases.return [None] flow
          end

        (* One domain returned no answer *)
        | None :: tl, C_pair(_,s,pid), _::tlchecks ->
          aux pid (snd_pair_man man) tl tlchecks >>$ fun after flow ->
          Cases.return (None :: after) flow

        (* One domain returned an answer.
           Here we need to merge this answer with the answers of the next domains *)
        | Some r :: tl, C_pair(_,s,pid), hdchecks::tlchecks ->
          (* Compute the answer of the next domains *)
          aux pid (snd_pair_man man) tl tlchecks >>= fun after_case after_flow ->
          (* Compute the answer of this domain *)
          r >>= fun case flow ->
          (* Now combine the two answers *)
          begin match case, after_case with
            (* If at least one answer is empty, all the conjunction is empty *)
            | Empty, _ | _, Empty ->
              let report = merge_report hdchecks (List.flatten tlchecks) (Flow.get_report flow) (Flow.get_report after_flow) in
              let flow = Flow.set_report report flow in
              let after_flow = Flow.set_report report flow in
              Cases.empty (Flow.meet man.lattice flow after_flow)

            (* NotHandled is transformed to None *)
            | NotHandled, Result(after_res,_,_) ->
              Cases.return (None :: after_res) after_flow

            (* Both domains replied, so merge the results *)
            | Result (res,effects,cleaners), Result(after_res,after_effects,after_cleaners) ->
              (* Merge only when the next domains provided some answers *)
              if after_res |> List.exists (function Some _ -> true | None -> false) then
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
                let flow = Flow.merge ~merge_report:(merge_report hdchecks (List.flatten tlchecks)) man.lattice pre (flow,effects) (after_flow,after_effects) in
                let effects = meet_teffect effects after_effects in
                let cleaners = StmtSet.union cleaners after_cleaners in
                Cases.case (Result (Some res :: after_res, effects, cleaners)) flow
              else
                (* Next domains returned no answer, so no merging *)
                let report = merge_report hdchecks (List.flatten tlchecks) (Flow.get_report flow) (Flow.get_report after_flow) in
                let flow = Flow.set_report report flow in
                Cases.case (Result (Some res :: after_res, effects, cleaners)) flow

            | _ -> assert false
          end

        | _ -> assert false
    in
    aux Pool.id man pointwise Pool.checks



  (** Merge the conflicts emerging from the same domain.
      This kind of conflicts arises when the same domain produces a conjunction of
      post-states. Since these conjunctions are from the same domain, there is
      no need to merge its local state; we just merge any shared sub-abstraction.
  *)
  let merge_intra_conflicts man pre (r:('a,'r) cases) : ('a,'r) cases =
    Cases.map_conjunction
      (fun conj ->
         let rec iter = function
           | [] -> assert false
           | [case,flow] -> Cases.get_case_effects case, Cases.get_case_cleaners case, flow
           | (case,flow)::tl ->
             let effects',cleaners',flow' = iter tl in
             let effects,cleaners = Cases.get_case_effects case, Cases.get_case_cleaners case in
             let flow'' = Flow.merge man.lattice ~merge_report:meet_report pre (flow,effects) (flow',effects') in
             meet_teffect effects effects', StmtSet.union cleaners cleaners', flow''
         in
         let effects,cleaners,flow = iter conj in
         List.map
           (fun (case,_) ->
              let case = Cases.set_case_effects effects case |>
                         Cases.set_case_cleaners cleaners in
              case,flow
           ) conj
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
    let member =
      List.find (fun domains -> DomainSet.cardinal domains = 1) Pool.members |>
      DomainSet.choose in
    Below member


  (** Get the context of a pointwise result *)
  let rec get_pointwise_ctx ~default pointwise =
    match pointwise with
    | []               -> default
    | None::tl         -> get_pointwise_ctx ~default tl
    | Some cases :: tl -> most_recent_ctx
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


  (** Replace missing pointwise results by calling the successor
      domain. Missing results are functions returning [None] or
      [NotHandled] cases. *)
  let add_missing_pointwise_results fsuccessor arg pointwise man flow =
    (* Separate handled and not-handled cases *)
    let handled_pointwise, not_handled =
      List.fold_left
        (fun (acc1,acc2) -> function
           | None   -> (None,true)::acc1,acc2
           | Some r ->
             let h,nh = Cases.partition (fun c _ -> match c with NotHandled -> false | _ -> true) r in
             let acc1' = (h,(if nh = None then false else true))::acc1 in
             let acc2' = OptionExt.neutral2 Cases.join nh acc2 in
             acc1',acc2'
        ) ([],None) pointwise
    in
    let handled_pointwise = List.rev handled_pointwise in
    let not_handled =
      if List.exists (function None -> true | _ -> false) pointwise
      then
        OptionExt.neutral2 Cases.join not_handled (Some (Cases.not_handled flow))
      else
        not_handled
    in
    match not_handled with
    | None -> pointwise
    | Some cases ->
      (* Merge all cases in one before calling successor domain *)
      let successor_res =
        Cases.remove_duplicates compare man.lattice cases >>= fun _ flow ->
        fsuccessor arg flow
      in
      (* Put successor's result back in the pointwise results *)
      let pointwise' =
        List.map
          (fun (r,has_not_handled_cases) ->
             match r with
             | None -> Some successor_res
             | Some rr when not has_not_handled_cases -> Some rr
             | Some rr -> Some (Cases.join rr successor_res)
          ) handled_pointwise
      in
      set_pointwise_ctx (Cases.get_ctx successor_res) pointwise'



  (** {2 Abstract transformer} *)
  (** ************************ *)

  (** Manager used by reductions *)
  let exec_reduction_man (man:('a, t) man) : 'a exec_reduction_man = {
    get_man = (fun id -> find_domain_man ~target:id ~tree:Pool.id man);
  }


  (** Simplify a pointwise post-state by changing lists of unit into unit *)
  let simplify_pointwise_post (pointwise:('a,unit option list) cases) : 'a post =
    Cases.map_result (fun _ -> ()) pointwise


  (** Apply reduction rules on a post-conditions *)
  let reduce_post stmt man pre post =
    let rman = exec_reduction_man man in
    (* Reduce each case separately *)
    post |> Cases.bind @@ fun case flow ->
    match case with
    | Empty -> Cases.empty flow
    | NotHandled -> Cases.not_handled flow
    | Result((),effects,_) ->
      (* Iterate over rules *)
      let rec iter = function
        | [] -> Post.return flow
        | rule::tl ->
          let module R = (val rule : EXEC_REDUCTION) in
          match R.reduce stmt man rman pre flow effects with
          | None -> iter tl
          | Some post -> post
      in
      iter Rules.srules


  (** Entry point of abstract transformers *)
  let exec targets =
    let f = Pool.exec targets in
    (fun stmt man flow ->
       with_effects
         (fun () ->
            apply_pointwise f stmt man flow |>
            OptionExt.lift @@ fun pointwise ->
            add_missing_pointwise_results (man.exec ~route:successor) stmt pointwise man flow |>
            merge_inter_conflicts man flow |>
            simplify_pointwise_post |>
            merge_intra_conflicts man flow |>
            reduce_post stmt man flow
         )
    )


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Manager used by reductions *)
  let eval_reduction_man (man:('a, t) man) : 'a eval_reduction_man = {
    get_man = (fun id -> find_domain_man ~target:id ~tree:Pool.id man);
  }


  (** Apply reduction rules on a pointwise evaluation *)
  let reduce_pointwise_eval exp man input (pointwise:('a, expr option list) cases) : 'a eval =
    pointwise >>$ fun el flow ->
    (* Keep only cases with non-empty results and remove duplicates *)
    let el' = List.filter (function Some _ -> true | _ -> false) el |>
              List.map (function Some e -> e | _ -> assert false) |>
              List.sort_uniq compare_expr
    in
    if el' = [] then Eval.empty flow
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
       with_effects
         (fun () ->
            apply_pointwise f exp man flow |>
            OptionExt.lift @@ fun pointwise ->
            add_missing_pointwise_results (man.eval ~route:successor) exp pointwise man flow |>
            merge_inter_conflicts man flow |>
            reduce_pointwise_eval exp man flow |>
            Eval.remove_duplicates man.lattice |>
            merge_intra_conflicts man flow
         )
    )

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
