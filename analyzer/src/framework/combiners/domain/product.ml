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


open Ast.All
open Core.All
open Sig.Abstraction.Stacked
open Sig.Reduction.Exec
open Sig.Reduction.Eval

module type POOL =
sig
  include STACKED
  val exec : zone -> stmt -> ('a,t,'s) man -> 'a flow -> 'a post option list * 'a ctx
  val eval : (zone * zone) -> expr -> ('a,t,'s) man -> 'a flow -> 'a eval option list * 'a ctx
end


type _ id += D_empty_product: unit id
type _ id += D_pair_product: (module STACKED with type t = 'a) * 'b id -> ('a*'b) id
    
module EmptyPool : POOL =
struct
  type t = unit
  let id = D_empty_product
  let () =
    let eq : type a. a id -> (a,unit) Eq.eq option = function
      | D_empty_product -> Some Eq
      | _ -> None
    in register_id { eq }
  let name = ""
  let interface = Interface.empty
  let alarms = []
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let print fmt () = ()
  let subset _ _ ((),s) ((),s') = true,s,s'
  let join _ _ ((),s) ((),s') = (),s,s'
  let meet _ _ ((),s) ((),s') = (),s,s'
  let widen _ _ ((),s) ((),s') = (),s,s',true
  let merge _ _ _ = ()
  let init _ _ flow = flow
  let exec _ _ _ flow = [], Flow.get_ctx flow
  let eval _ _ _ flow = [], Flow.get_ctx flow
  let ask _ _ _ = None
end


let hdman (man:('a, 'b * 'c, 's) man) : ('a, 'b, 's) man = {
  man with
  get = get_pair_fst man;
  set = set_pair_fst man;
  get_log = (fun glog -> man.get_log glog |> Log.get_left_log);
  set_log = (fun log glog -> man.set_log (
      Log.mk_log [] log (man.get_log glog |> Log.get_right_log)
    ) glog);
}

let tlman (man:('a, 'b * 'c, 's) man) : ('a, 'c, 's) man = {
  man with
  get = get_pair_snd man;
  set = set_pair_snd man;
  get_log = (fun glog -> man.get_log glog |> Log.get_right_log);
  set_log = (fun log glog -> man.set_log (
      Log.mk_log [] (man.get_log glog |> Log.get_left_log) log
    ) glog);
}



module MakePairPool(S:STACKED)(P:POOL) : POOL with type t = S.t * P.t =
struct
  type t = S.t * P.t
  let id = D_pair_product((module S), P.id)

  let () =
    let eq : type a. a id -> (a,t) Eq.eq option = function
      | D_pair_product(s,p) ->
        let module SS = (val s) in
        begin match equal_id S.id SS.id  with
          | Some Eq ->
            begin match equal_id P.id p with
              | Some Eq -> Some Eq
              | None -> None
            end
          | None -> None
        end
      | _ -> None
    in register_id { eq }


  let interface = Interface.concat S.interface P.interface
  let alarms = S.alarms @ P.alarms
  let name = ""

  let print fmt (s,p) =
    match P.id with
    | D_empty_product  -> Format.fprintf fmt "%a" S.print s
    | D_pair_product _ -> Format.fprintf fmt "%a@\n%a" S.print s P.print p
    | _ -> assert false

  let bottom = S.bottom, P.bottom
  let top = S.top, P.top

  let is_bottom (s,p) = S.is_bottom s || P.is_bottom p

  let subset man ctx ((a1,a2),s) ((a1',a2'),s') =
    let b1, s, s' = S.subset (hdman man) ctx (a1,s) (a1',s') in
    let b2, s, s' = P.subset (tlman man) ctx (a2,s) (a2',s') in
    b1 && b2, s, s'

  let join man ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s' = S.join (hdman man) ctx (a1,s) (a1',s') in
    let aa2, s, s' = P.join (tlman man) ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let meet man ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s' = S.meet (hdman man) ctx (a1,s) (a1',s') in
    let aa2, s, s' = P.meet (tlman man) ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let widen man ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s', stable1 = S.widen (hdman man) ctx (a1,s) (a1',s') in
    let aa2, s, s', stable2 = P.widen (tlman man) ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s', stable1 && stable2

  let merge (pre1,pre2) ((a1,a2), log) ((a1',a2'), log') =
    S.merge pre1 (a1, Log.get_left_log log) (a1', Log.get_left_log log'),
    P.merge pre2 (a2, Log.get_right_log log) (a2', Log.get_right_log log')

  let init prog man flow =
    S.init prog (hdman man) flow |>
    P.init prog (tlman man)

  let exec zone =
    let next = P.exec zone in
    if not (Interface.sat_exec zone S.interface) then
      (fun stmt man flow ->
         let l,ctx = next stmt (tlman man) flow in
         None :: l, ctx
      )
    else
      (fun stmt man flow ->
         let post = S.exec zone stmt (hdman man) flow in
         let ctx = OptionExt.apply Post.get_ctx (Flow.get_ctx flow) post in
         let flow = Flow.set_ctx ctx flow in
         let l,ctx = next stmt (tlman man) flow in
         post :: l, ctx
      )

  let eval zone =
    let next = P.eval zone in
    if not (Interface.sat_eval zone S.interface) then
      (fun stmt man flow ->
         let l,ctx = next stmt (tlman man) flow in
         None :: l, ctx
      )
    else
      (fun stmt man flow ->
         let eval = S.eval zone stmt (hdman man) flow in
         let ctx = OptionExt.apply Eval.get_ctx (Flow.get_ctx flow) eval in
         let flow = Flow.set_ctx ctx flow in
         let l,ctx = next stmt (tlman man) flow in
         eval :: l, ctx
      )

  let ask query man flow =
    OptionExt.neutral2
      (meet_query query)
      (S.ask query (hdman man) flow)
      (P.ask query (tlman man) flow)

end
      

(** Product functor *)
module Make(Pool:POOL)
    (Rules:sig
       val erules: (module EVAL_REDUCTION) list
       val srules: (module EXEC_REDUCTION) list
     end) : STACKED with type t = Pool.t =
struct

  include Pool

  let name = "framework.combiners.domain.product"

  (** {2 Merging functions} *)
  (** ********************* *)

  (** Merge the conflicts of two flows using logs *)
  let merge_flows ~merge_alarms (man:('a,'t, 's) man) pre (flow1,log1) (flow2,log2) =
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
    let rec aux : type t. t id -> ('a,t,'s) man -> ('a,'r) cases option list -> ('a,'r option option list * alarm_class list) cases =
      fun id man pointwise ->
        match pointwise, id with
        | [None], D_pair_product (s, _) ->
          Cases.singleton ([None], []) pre

        | [Some r], D_pair_product (s, _) ->
          r >>= fun rr flow ->
          let module S = (val s) in
          Cases.singleton ([Some rr], S.alarms) flow

        | None :: tl, D_pair_product(s,pid) ->
          aux pid (tlman man) tl >>$ fun (after,alarms) flow ->
          Cases.singleton (None :: after, alarms) flow

        | Some r :: tl, D_pair_product(s,pid) ->
          aux pid (tlman man) tl |>
          Cases.bind_full @@ fun after after_flow after_log after_cleaners ->
          r |> Cases.bind_full @@ fun rr flow log cleaners ->
          let module S = (val s) in
          let after, alarms = after |> OptionExt.none_to_exn in
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
            Cases.return (Some (Some rr :: after, S.alarms @ alarms)) flow ~cleaners ~log
          else
            Cases.return (Some (Some rr :: after, S.alarms @ alarms)) flow ~cleaners ~log


        | _ -> assert false
    in
    aux Pool.id man pointwise |>
    Cases.map (fun (r,_) -> r)



  (** Merge the conflicts emerging from the same domain *)
  let merge_intra_conflicts man pre (r:('a,'r) cases) : ('a,'r) cases =
    Cases.map_fold_conjunctions (fun (flow1,log1) (flow2,log2) ->
        merge_flows ~merge_alarms:AlarmSet.inter man pre (flow1,log1) (flow2,log2)
      ) r


  (** {2 Abstract transformer} *)
  (** ************************ *)

  (** Manager used by reductions *)
  let exec_reduction_man (man:('a, t, 's) man) : ('a, 's) exec_reduction_man = {
    get_man = (
      let f : type t. t id -> ('a,t,'s) man =
        fun id ->
          let rec aux : type t tt. t id -> tt id -> ('a,tt, 's) man -> ('a,t,'s) man =
            fun id pool man ->
              match pool with
              | D_empty_product -> raise Not_found
              | D_pair_product(s,pid) ->
                let module S = (val s) in
                begin match equal_id S.id id with
                  | Some Eq -> (hdman man)
                  | None -> aux id pid (tlman man) end
              | _ -> assert false
          in
          aux id Pool.id man
      in
      f
    );
  }


  (* Apply [exec] transfer function pointwise over all domains *)
  let exec_pointwise zone stmt man flow : 'a post option list option =
    let posts, ctx = Pool.exec zone stmt man flow in
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
        Post.bind (R.reduce stmt man rman pre) post
      ) post Rules.srules


  (** Entry point of abstract transformers *)
  let exec zone stmt man flow =
    exec_pointwise zone stmt man flow |>
    OptionExt.lift @@ fun pointwise ->
    merge_inter_conflicts man flow pointwise |>
    simplify_pointwise_post |>
    merge_intra_conflicts man flow |>
    reduce_post stmt man flow


  (** {2 Abstract evaluations} *)
  (** ************************ *)


  (** Compute pointwise evaluations over the pool of domains *)
  let eval_pointwise zone exp man flow : 'a eval option list option =
    let pointwise, ctx = Pool.eval zone exp man flow in
    if List.exists (function Some _ -> true | None -> false) pointwise
    then Some pointwise
    else None


  (** Manager used by reductions *)
  let eval_reduction_man (man:('a, t, 's) man) : ('a, 's) eval_reduction_man = {
    get_eval = (
      let f : type t. t id -> prod_eval -> expr option =
        fun id evals ->
          let rec aux : type t tt. t id -> tt id -> prod_eval -> expr option =
            fun id pool el ->
              match pool, el with
              | D_empty_product, [] -> None
              | D_pair_product(s,pid), (hde::tle) ->
                begin
                  let module S = (val s) in
                  match equal_id S.id id with
                  | Some Eq -> (match hde with None -> None | Some x -> x)
                  | None -> aux id pid tle
                end
              | _ -> assert false
          in
          aux id Pool.id evals
      in
      f
    );

    del_eval = (
      let f : type t. t id -> prod_eval -> prod_eval =
        fun id evals ->
          let rec aux : type t tt. t id -> tt id -> prod_eval -> prod_eval =
            fun id pool el ->
              match pool, el with
              | D_empty_product, [] -> raise Not_found
              | D_pair_product(s,pid), (hde::tle) ->
                begin
                  let module S = (val s) in
                  match equal_id S.id id with
                  | Some Eq -> None :: tle
                  | None -> hde :: aux id pid tle
                end
              | _ -> assert false
          in
          aux id Pool.id evals
      in
      f
    );

    get_man = (
      let f : type t. t id -> ('a,t,'s) man =
        fun id ->
          let rec aux : type t tt. t id -> tt id -> ('a,tt, 's) man -> ('a,t,'s) man =
            fun id pool man ->
              match pool with
              | D_empty_product -> raise Not_found
              | D_pair_product(s,pid) ->
                let module S = (val s) in
                begin match equal_id S.id id with
                  | Some Eq -> (hdman man)
                  | None -> aux id pid (tlman man) end
              | _ -> assert false
          in
          aux id Pool.id man
      in
      f
    );

  }

  
  (** Apply reduction rules on a pointwise evaluation *)
  let reduce_pointwise_eval exp man (pointwise:('a,expr option option list) cases) : 'a eval =
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
  let eval zone exp man flow =
    eval_pointwise zone exp man flow |>
    OptionExt.lift @@ fun pointwise ->
    merge_inter_conflicts man flow pointwise |>
    reduce_pointwise_eval exp man |>
    merge_intra_conflicts man flow



end



(****************************************************************************)
(**                      {2 Functional factory}                             *)
(****************************************************************************)

(** The following functions are useful to create a reduced product
    from a list of first-class modules
*)



let rec make_pool : (module STACKED) list -> (module POOL) = function
  | [] -> (module EmptyPool)
  | hd :: tl ->
    let module S = (val hd) in
    let p = make_pool tl in
    (module MakePairPool(S)(val p))
    

let make
    (domains: (module STACKED) list)
    (erules: (module EVAL_REDUCTION) list)
    (srules: (module EXEC_REDUCTION) list)
  : (module STACKED) =
  let p = make_pool domains in
  (module Make(val p)
       (struct
         let erules = erules
         let srules = srules
       end)
  )
