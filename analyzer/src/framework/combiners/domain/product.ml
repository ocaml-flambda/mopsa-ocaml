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

(** Reduced product with n-ary reduction rules *)


open Core.All
open Sig.Reduction.Exec
open Sig.Reduction.Eval
open Sig.Combiner.Stacked
open Common


module type POOL =
sig
  include STACKED_COMBINER
  val alarms : alarm_class list list
  val exec : domain list -> stmt -> ('a,t) man -> 'a flow -> 'a post option list * 'a ctx
  val eval : string list -> expr -> ('a,t) man -> 'a flow -> 'a eval option list * 'a ctx
end


    
module EmptyPool : POOL =
struct
  type t = unit
  let id = C_empty
  let name = "()"
  let domains = []
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
  let exec _ _ _ flow = [], Flow.get_ctx flow
  let eval _ _ _ flow = [], Flow.get_ctx flow
  let ask _ _ _ _ = None
end


module MakePairPool(S:STACKED_COMBINER)(P:POOL) : POOL with type t = S.t * P.t =
struct
  type t = S.t * P.t
  let id = C_pair(Product,S.id,P.id)
  let domains = S.domains @ P.domains
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
         let l,ctx = f2 stmt (snd_pair_man man) flow in
         None :: l, ctx
      )
    else
      let f1 = S.exec targets in
      (fun stmt man flow ->
         let post = f1 stmt (fst_pair_man man) flow in
         let ctx = OptionExt.apply Cases.get_ctx (Flow.get_ctx flow) post in
         let flow = Flow.set_ctx ctx flow in
         let l,ctx = f2 stmt (snd_pair_man man) flow in
         post :: l, ctx
      )

  let eval targets =
    let f2 = P.eval targets in
    if not (sat_targets ~targets ~domains:S.domains) then
      (fun exp man flow ->
         let l,ctx = f2 exp (snd_pair_man man) flow in
         None :: l, ctx
      )
    else
      let f1 = S.eval targets in
      (fun exp man flow ->
         let eval = f1 exp (fst_pair_man man) flow in
         let ctx = OptionExt.apply Cases.get_ctx (Flow.get_ctx flow) eval in
         let flow = Flow.set_ctx ctx flow in
         let l,ctx = f2 exp (snd_pair_man man) flow in
         eval :: l, ctx
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
      

(** Product functor *)
module Make(Pool:POOL)
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



  (** Merge the conflicts between distinct domains in a pointwise result *)
  let merge_inter_conflicts man pre (pointwise:('a,'r) cases option list) : ('a,'r option option list) cases =
    let rec aux : type t. t id -> ('a,t) man -> ('a,'r) cases option list -> alarm_class list list -> ('a,'r option option list) cases =
      fun id man pointwise alarms ->
        match pointwise, id, alarms with
        | [None], C_pair(_, s, _), _ ->
          Cases.singleton [None] pre

        | [Some r], C_pair(_, s, _), _ ->
          r >>= fun rr flow ->
          Cases.singleton [Some rr] flow

        | None :: tl, C_pair(_,s,pid), _::tlalarms ->
          aux pid (snd_pair_man man) tl tlalarms >>$ fun after flow ->
          Cases.singleton (None :: after) flow

        | Some r :: tl, C_pair(_,s,pid), hdalarms::tlalarms ->
          aux pid (snd_pair_man man) tl tlalarms |>
          Cases.bind_full @@ fun after after_flow after_log after_cleaners ->
          r |> Cases.bind_full @@ fun rr flow log cleaners ->
          let after = after |> OptionExt.none_to_exn in
          if after |> List.exists (function Some _ -> true | None -> false) then
            let fst_pair_man = fst_pair_man man in
            let after_flow = Flow.set T_cur (
                let cur = Flow.get T_cur man.lattice flow in
                let after_cur = Flow.get T_cur man.lattice after_flow in
                fst_pair_man.set (fst_pair_man.get cur) after_cur
              ) man.lattice after_flow
            in
            let common_alarms = List.filter (fun a -> List.mem a hdalarms) (List.flatten tlalarms) in
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
            Cases.return (Some (Some rr :: after)) flow ~cleaners ~log
          else
            Cases.return (Some (Some rr :: after)) flow ~cleaners ~log


        | _ -> assert false
    in
    aux Pool.id man pointwise Pool.alarms



  (** Merge the conflicts emerging from the same domain *)
  let merge_intra_conflicts man pre (r:('a,'r) cases) : ('a,'r) cases =
    Cases.map_fold_conjunctions (fun (flow1,log1) (flow2,log2) ->
        merge_flows ~merge_alarms:AlarmSet.inter man pre (flow1,log1) (flow2,log2)
      ) r


  (** {2 Abstract transformer} *)
  (** ************************ *)

  (** Manager used by reductions *)
  let exec_reduction_man (man:('a, t) man) : 'a exec_reduction_man = {
    get_man = (fun id -> find_domain_man id Pool.id man);
  }


  (* Apply [exec] transfer function pointwise over all domains *)
  let exec_pointwise f stmt man flow : 'a post option list option =
    let posts, ctx = f stmt man flow in
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
    let rman = exec_reduction_man man in
    List.fold_left (fun pointwise rule ->
        let module R = (val rule : EXEC_REDUCTION) in
        post >>$ fun () -> R.reduce stmt man rman pre 
      ) post Rules.srules


  (** Entry point of abstract transformers *)
  let exec targets =
    let f = Pool.exec targets in
    (fun stmt man flow ->
       exec_pointwise f stmt man flow |>
       OptionExt.lift @@ fun pointwise ->
       merge_inter_conflicts man flow pointwise |>
       simplify_pointwise_post |>
       merge_intra_conflicts man flow |>
       reduce_post stmt man flow)


  (** {2 Abstract evaluations} *)
  (** ************************ *)


  (** Compute pointwise evaluations over the pool of domains *)
  let eval_pointwise f exp man flow : 'a eval option list option =
    let pointwise, ctx = f exp man flow in
    if List.exists (function Some _ -> true | None -> false) pointwise
    then Some pointwise
    else None


  (** Manager used by reductions *)
  let eval_reduction_man (man:('a, t) man) : 'a eval_reduction_man = {
    get_eval = (
      let f : type t. t id -> prod_eval -> expr option = fun id evals ->
        let rec aux : type t tt. t id -> tt id -> prod_eval -> expr option = fun target tree el ->
          match tree, el with
          | C_empty, [] -> None
          | C_pair(_,left,right), (hde::tle) ->
            if mem_domain ~target ~tree:left then
              match hde with None -> None | Some x -> x
            else
              aux target right tle
          | _ -> assert false
        in
        aux id Pool.id evals
      in
      f
    );

    del_eval = (
      let f : type t. t id -> prod_eval -> prod_eval = fun id evals ->
        let rec aux : type t tt. t id -> tt id -> prod_eval -> prod_eval = fun target tree el ->
          match tree, el with
          | C_empty, [] -> raise Not_found
          | C_pair(_,left,right), (hde::tle) ->
            if mem_domain ~target ~tree:left then None :: tle else hde :: aux target right tle
          | _ -> assert false
          in
          aux id Pool.id evals
      in
      f
    );

    get_man = (fun id -> find_domain_man id Pool.id man);
  }

  
  (** Apply reduction rules on a pointwise evaluation *)
  let reduce_pointwise_eval exp man (pointwise:('a, expr option option list) cases) : 'a eval =
    let rman = eval_reduction_man man in
    (* Let reduction rules roll out imprecise evaluations from [pointwise] *)
    let pointwise = List.fold_left (fun pointwise rule ->
        let module R = (val rule : EVAL_REDUCTION) in
        pointwise |> Cases.bind_some @@ fun el flow ->
        R.reduce exp man rman flow el
      ) pointwise Rules.erules
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
  let eval targets =
    let f = Pool.eval targets in
    (fun exp man flow ->
       eval_pointwise f exp man flow |>
       OptionExt.lift @@ fun pointwise ->
       merge_inter_conflicts man flow pointwise |>
       reduce_pointwise_eval exp man |>
       merge_intra_conflicts man flow)



end



(****************************************************************************)
(**                      {2 Functional factory}                             *)
(****************************************************************************)

(** The following functions are useful to create a reduced product
    from a list of first-class modules
*)



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
