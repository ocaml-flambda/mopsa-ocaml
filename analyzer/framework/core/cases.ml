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


(** Cases - Encoding of results returned by transfer functions. *)


open Mopsa_utils
open Ast.Stmt
open Flow
open Token
open Effect
open Context
open Alarm
open Lattice

type cleaners = StmtSet.t

(** Single case of a computation *)
type 'r case =
  | Result of 'r * teffect * cleaners
  | Empty
  | NotHandled

(** Multiple cases of a computation, encoded as a DNF *)
type ('a,'r) cases = {cases: ('r case * 'a flow) Dnf.t;
                      ctx: 'a ctx}

let case (case:'r case) flow : ('a,'r) cases = {cases=Dnf.singleton (case,flow);
                                                ctx = Flow.get_ctx flow}

let return ?(effects=empty_teffect) ?(cleaners=[]) (res:'r) (flow:'a flow) =
  case (Result (res,effects,StmtSet.of_list cleaners)) flow

let singleton = return

let empty (flow:'a flow) : ('a,'r) cases =
  let flow = Flow.remove T_cur flow in
  case Empty flow

let not_handled (flow:'a flow) : ('a,'r) cases =
  case NotHandled flow


let opt_clean_cur_only = ref false


let get_ctx cases = cases.ctx

let set_ctx ctx cases =
  if ctx == get_ctx cases then
    cases
  else
    {cases=Dnf.map
      (fun (case,flow) -> (case, Flow.set_ctx ctx flow))
      cases.cases;
     ctx}

let copy_ctx src dst =
  set_ctx (get_ctx src) dst

let get_most_recent_ctx cases =
  Dnf.fold
    (fun acc (case,flow) -> most_recent_ctx acc (Flow.get_ctx flow))
    (get_ctx cases) cases.cases

let normalize_ctx cases =
  let ctx = get_most_recent_ctx cases in
  set_ctx ctx cases

let get_callstack r =
  get_ctx r |>
  find_ctx Context.callstack_ctx_key

let set_callstack cs r =
  set_ctx (
    get_ctx r |>
    add_ctx Context.callstack_ctx_key cs
  ) r

let get_case_cleaners (case:'r case) : StmtSet.t =
  match case with
  | Result(_,_,cleaners) -> cleaners
  | Empty | NotHandled   -> StmtSet.empty

let set_case_cleaners (cleaners:StmtSet.t) (case:'r case) : 'r case =
  match case with
  | Result(r,effects,_) -> Result(r,effects,cleaners)
  | _ -> case

let get_case_effects (case:'r case) : teffect =
  match case with
  | Result(_,effects,_)    -> effects
  | Empty | NotHandled -> empty_teffect

let set_case_effects (effects:teffect) (case:'r case) : 'r case =
  match case with
  | Result(r,old,cleaners) -> if old == effects then case else Result(r,effects,cleaners)
  | _ -> case


let map
    (f:'r case -> 'a flow -> 's case * 'a flow)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  {cases=Dnf.map (fun (case,flow) -> f case flow) cases.cases; ctx=cases.ctx} |>
  normalize_ctx


let map_result
    (f:'r->'s)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  map (fun case flow ->
      let case' =
        match case with
        | Result (r,effects,cleaners) -> Result (f r,effects,cleaners)
        | Empty                   -> Empty
        | NotHandled              -> NotHandled
      in
      (case',flow)
    ) cases

let map_conjunction
    (f:('r case * 'a flow) list -> ('s case * 'a flow) list)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  {cases=Dnf.map_conjunction f cases.cases; ctx=cases.ctx} |>
  normalize_ctx

let map_disjunction
    (f:('r case * 'a flow) list -> ('s case * 'a flow) list)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  {cases=Dnf.map_disjunction f cases.cases; ctx=cases.ctx} |>
  normalize_ctx


let reduce
    (f:'r case -> 'a flow -> 'b)
    ~(join:'b -> 'b -> 'b)
    ~(meet:'b -> 'b -> 'b)
    (cases:('a,'r) cases)
  : 'b =
  Dnf.reduce (fun (case,flow) -> f case flow) ~join ~meet cases.cases

let reduce_result
    (f:'r -> 'a flow -> 'b)
    ~(join:'b -> 'b -> 'b)
    ~(meet:'b -> 'b -> 'b)
    ~(bottom:'b)
    (cases:('a,'r) cases)
  : 'b =
  reduce
    (fun case flow ->
       match case with
       | Result (r,effects,cleaners) -> f r flow
       | Empty | NotHandled      -> bottom)
    ~join ~meet cases


let print pp fmt cases =
  Dnf.print (fun fmt (case,flow) -> pp fmt case flow) fmt cases.cases


let print_result pp fmt cases =
  print (fun fmt case flow ->
      match case with
      | Result (r,_,_) -> pp fmt r flow
      | Empty          -> Format.fprintf fmt "ε"
      | NotHandled     -> Format.fprintf fmt "✗"
    )
    fmt cases


let map_effects
    (f:teffect -> teffect)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  map
    (fun case flow ->
       match case with
       | Result(r,effects,cleaners) -> Result(r,f effects,cleaners), flow
       | _                      -> case, flow
    ) cases

let set_effects
    (effects:teffect)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  map
    (fun case flow ->
       match case with
       | Result(r,old,cleaners) -> if old == effects then case,flow else Result(r,effects,cleaners), flow
       | _                      -> case, flow
    ) cases

let set_cleaners
    (cleaners:stmt list)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  let cleaners = StmtSet.of_list cleaners in
  map
    (fun case flow ->
       match case with
       | Result(r,effects,_) -> Result(r,effects,cleaners), flow
       | _               -> case, flow
    ) cases


let concat_effects
    (old:teffect)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  map
    (fun case flow ->
       match case with
       | Result(r,recent,cleaners) ->
         (* Add effectss of non-empty environments only *)
         (* FIXME: Since are always called from the binders, we can't
              require having the lattice manager. So we can't test if
              T_cur is ⊥ or not! For the moment, we rely on empty flow
              maps, but this is not always sufficient.
         *)
         if Flow.mem T_cur flow then
           Result(r, concat_teffect ~old ~recent, cleaners), flow
         else
           case, flow
       | _ -> case, flow
    ) cases

let add_cleaners
    (cleaners:stmt list)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  let cleaners = StmtSet.of_list cleaners in
  map
    (fun case flow ->
       match case with
       | Result(r,effects,cleaners') -> Result(r,effects,StmtSet.union cleaners' cleaners), flow
       | _ -> case, flow
    ) cases

let fold
    (f:'b -> 'r case -> 'a flow -> 'b)
    (init:'b)
    (cases:('a,'r) cases)
  : 'b =
  Dnf.fold (fun acc (case,flow) -> f acc case flow) init cases.cases


let fold_result
    (f:'b -> 'r -> 'a flow -> 'b)
    (init:'b)
    (cases:('a,'r) cases)
  : 'b =
  fold
    (fun acc case flow ->
       match case with
       | Result (r,_,_)     -> f acc r flow
       | Empty | NotHandled -> acc
    ) init cases

let iter
    (f:'r case -> 'a flow -> unit)
    (cases:('a,'r) cases)
  : unit =
  Dnf.iter (fun (case,flow) -> f case flow) cases.cases


let iter_result
    (f:'r -> 'a flow -> unit)
    (cases:('a,'r) cases)
  : unit =
  iter
    (fun case flow ->
       match case with
       | Result (r,_,_)     -> f r flow
       | Empty | NotHandled -> ()
    ) cases

let partition
    (f:'r case -> 'a flow -> bool)
    (cases:('a,'r) cases)
  : ('a,'r) cases option * ('a,'r) cases option =
  let oc1, oc2 = Dnf.partition (fun (case,flow) -> f case flow) cases.cases in
  OptionExt.lift (fun c -> {cases=c; ctx=cases.ctx}) oc1,
  OptionExt.lift (fun c -> {cases=c; ctx=cases.ctx}) oc2

let flatten (cases:('a,'r) cases) : ('r case * 'a flow) list =
  Dnf.to_list cases.cases |>
  List.flatten

let for_all
    (f:'r case -> 'a flow -> bool)
    (cases:('a,'r) cases)
  : bool =
  flatten cases |>
  List.for_all (fun (case,flow) -> f case flow)

let for_all_result
    (f:'r -> 'a flow -> bool)
    (cases:('a,'r) cases)
  : bool =
  for_all
    (fun case flow ->
      match case with
      | Result (r,_,_)     -> f r flow
      | Empty | NotHandled -> true
    ) cases


let exists
    (f:'r case -> 'a flow -> bool)
    (cases:('a,'r) cases)
  : bool =
  flatten cases |>
  List.exists (fun (case,flow) -> f case flow)

let exists_result
    (f:'r -> 'a flow -> bool)
    (cases:('a,'r) cases)
  : bool =
  exists
    (fun case flow ->
      match case with
      | Result (r,_,_)     -> f r flow
      | Empty | NotHandled -> false
    ) cases


(** Join two results *)
let join (cases1:('a,'r) cases) (cases2:('a,'r) cases) : ('a,'r) cases =
  if cases1 == cases2 then cases1 else
  if for_all (fun _ flow -> Flow.is_empty flow) cases1 then cases2 else
  if for_all (fun _ flow -> Flow.is_empty flow) cases2 then cases1
  else
    {cases=Dnf.mk_or cases1.cases cases2.cases; ctx=cases1.ctx} |>
    normalize_ctx

(** Meet two results *)
let meet (cases1:('a,'r) cases) (cases2:('a,'r) cases) : ('a,'r) cases =
  {cases=Dnf.mk_and cases1.cases cases2.cases; ctx=cases1.ctx} |>
  normalize_ctx


(** Join a list of results *)
let join_list ~empty (l: ('a,'r) cases list) : ('a,'r) cases =
  match l with
  | [] -> empty ()
  | hd :: tl -> List.fold_left join hd tl


(** Meet a list of results *)
let meet_list ~empty (l: ('a,'r) cases list) : ('a,'r) cases =
  match l with
  | [] -> empty ()
  | hd :: tl -> List.fold_left meet hd tl


let remove_duplicates (compare_case: 'r case -> 'r case -> int) (lattice: 'a Lattice.lattice) (cases: ('a, 'r) cases): ('a, 'r) cases =
  (* Effects of empty environments should be ignored.
     This function returns an empty effects when T_cur environment is empty. *)
  let real_effects flow effects =
    if lattice.Lattice.is_bottom (Flow.get T_cur lattice flow)
    then empty_teffect
    else effects
  in
  (* Remove duplicates of a case in a conjunction *)
  let rec remove_case_duplicates_in_conj case flow conj =
    match conj with
    | [] -> case, flow, []
    | (case',flow') :: tl' ->
      let case'', flow'', tl'' = remove_case_duplicates_in_conj case flow tl' in
      match compare_case case case' with
        | 0 ->
          let flow = Flow.meet lattice flow' flow'' in
          let case = set_case_cleaners (StmtSet.union (get_case_cleaners case') (get_case_cleaners case'')) case'' |>
                     set_case_effects (meet_teffect (get_case_effects case') (get_case_effects case'') |> real_effects flow) in
          case,flow,tl''
        | _ -> case'', flow'', (case',flow')::tl''
  in
  (* Remove all duplicates in a conjunction *)
  let rec remove_duplicates_in_conj conj =
    match conj with
    | [] -> []
    | [(case,flow)] -> conj
    | (case,flow) :: tl ->
      (* Remove duplicates of case from tl *)
      let case', flow', tl' = remove_case_duplicates_in_conj case flow tl in
      (case',flow') :: remove_duplicates_in_conj tl'
  in
  (* Remove duplicates of a conjunction in a disjunction *)
  let rec remove_conj_duplicates_in_disj conj disj =
    match disj with
    | [] -> conj, []
    | conj'::tl ->
      let conj'', tl' = remove_conj_duplicates_in_disj conj tl in
      match Compare.list (fun (c,_) (c',_) -> compare_case c c') conj' conj'' with
      | 0 ->
        let conj =
          List.combine conj' conj'' |>
          List.map
            (fun ((case,flow), (case',flow')) ->
               let flow = Flow.join lattice flow flow' in
               let case = set_case_cleaners (StmtSet.union (get_case_cleaners case) (get_case_cleaners case')) case |>
                          set_case_effects (join_teffect (get_case_effects case) (get_case_effects case') |> real_effects flow) in
               case,flow
            )
        in
        conj,tl'
      | _ ->
        conj'',conj'::tl'
  in
  let rec remove_duplicates_in_disj = function
    | [] -> []
    | [[e]] as x -> x
    | [conj] -> [remove_duplicates_in_conj conj]
    | conj::tl ->
      let conj = remove_duplicates_in_conj conj in
      let conj',tl' = remove_conj_duplicates_in_disj conj tl in
      conj'::remove_duplicates_in_disj tl'
  in
  let cases' = Dnf.from_list (remove_duplicates_in_disj (Dnf.to_list cases.cases)) in
  {cases with cases=cases'}

let remove_duplicate_results compare_results lattice cases =
  remove_duplicates
    (fun case case' ->
       match case, case' with
       | Result(r,_,_), Result(r',_,_) -> compare_results r r'
       | _                             -> compare case case'
    ) lattice cases


let cardinal cases = Dnf.cardinal cases.cases


(****************************************************************************)
(**                        {2 Monadic binders}                              *)
(****************************************************************************)

let bind_opt
    (f: 'r case -> 'a flow -> ('a,'s) cases option )
    (cases: ('a,'r) cases)
  : ('a,'s) cases option =
  match Dnf.to_list cases.cases with
  | [[Result(_, effects, cleaners) as case, flow]]
    when StmtSet.is_empty cleaners && 
         (not (are_effects_enabled ()) || is_empty_teffect effects) ->
    f case flow

  | [[case, flow]] -> (
      match f case flow with
      | None -> None
      | Some cases' ->
        add_cleaners (get_case_cleaners case |> StmtSet.elements) cases' |>
        concat_effects (get_case_effects case) |>
        Option.some
    )
  
  | _ ->
    let ctx,ret =
      Dnf.fold_bind
        (fun ctx (case,flow) ->
           let flow = Flow.set_ctx ctx flow in
           let cases' =
             match f case flow with
             | None   -> not_handled flow
             | Some c -> c
           in
           let ctx' = get_ctx cases' in
           let cases'' = add_cleaners (get_case_cleaners case |> StmtSet.elements) cases' |>
                         concat_effects (get_case_effects case) in
           ctx',cases''.cases
        )
        (get_ctx cases) cases.cases in
    set_ctx ctx {cases with cases = ret} |>
    OptionExt.return


let (>>=?) cases f = bind_opt f cases

let bind f cases =
  match Dnf.to_list cases.cases with
  | [[Result(_, effects, cleaners) as case, flow]]
    when StmtSet.is_empty cleaners && 
         (not (are_effects_enabled ()) || is_empty_teffect effects) ->
    f case flow

  | [[case, flow]] ->
    let cases' = f case flow in
    add_cleaners (get_case_cleaners case |> StmtSet.elements) cases' |>
    concat_effects (get_case_effects case)
  
  | _ ->
    bind_opt (fun case flow -> Some (f case flow)) cases |>
    OptionExt.none_to_exn

let (>>=) cases f = bind f cases


let bind_result_opt
    (f:'r -> 'a flow -> ('a,'s) cases option)
    (cases:('a,'r) cases)
  : ('a,'s) cases option
  =
  match Dnf.to_list cases.cases with
  | [[Result(r, effects, cleaners), flow]]
    when StmtSet.is_empty cleaners && 
         (not (are_effects_enabled ()) || is_empty_teffect effects) ->
    f r flow

  | [[Result(r, effects, cleaners), flow]] -> (
      match f r flow with
      | None -> None
      | Some cases' ->
        add_cleaners (StmtSet.elements cleaners) cases' |>
        concat_effects effects |>
        Option.some
    )
  
  | _ ->
    bind_opt
      (fun case flow ->
         match case with
         | Result (r,_,_)   -> f r flow
         | Empty            -> Some (empty flow)
         | NotHandled       -> Some (not_handled flow)
      ) cases


let (>>$?) r f = bind_result_opt f r


let bind_result
    (f:'r -> 'a flow -> ('a,'s) cases)
    (cases:('a,'r) cases)
  : ('a,'s) cases
  =
  match Dnf.to_list cases.cases with
  | [[Result(r, effects, cleaners), flow]]
    when StmtSet.is_empty cleaners && 
         (not (are_effects_enabled ()) || is_empty_teffect effects) ->
    f r flow

  | [[Result(r, effects, cleaners), flow]] ->
    let cases' = f r flow in
    add_cleaners (StmtSet.elements cleaners) cases' |>
    concat_effects effects
  
  | _ ->
    bind_result_opt (fun r flow -> Some (f r flow)) cases |>
    OptionExt.none_to_exn


let (>>$) r f = bind_result f r
let ( let* ) r f = bind_result f r

let bind_conjunction
    (f:('r case * 'a flow) list -> ('a,'s) cases)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  let ctx,ret =
    Dnf.fold_bind_conjunction
      (fun ctx conj ->
         let conj' = List.map (fun (case,flow) -> (case,Flow.set_ctx ctx flow)) conj in
         let cases' = f conj' in
         let ctx' = get_ctx cases' in
         ctx',cases'.cases
      ) (get_ctx cases) cases.cases in
  set_ctx ctx {cases with cases=ret}

let bind_conjunction_result
    (f:'r list -> 'a flow -> ('a,'s) cases)
    (lattice:'a lattice)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  bind_conjunction
    (fun conj ->
       (* Separate cases actual results from empty and not-handled cases *)
       let handled,others = List.partition (fun (case,flow) -> match case with Result _ -> true | _ -> false) conj in
       (* This is a hack to change the type of others from 'r case to 's case *)
       let others = List.map (fun (case,flow) -> match case with NotHandled -> NotHandled,flow | Empty -> Empty,flow | _ -> assert false) others in
       if handled = [] then
         meet_list (List.map (fun (c,flow) -> case c flow) others) ~empty:(fun () -> assert false)
       else
         let cl,fl = List.split handled in
         let flow = List.fold_left (Flow.meet lattice) (List.hd fl) (List.tl fl) in
         let rl,effects,cleaners =
           List.fold_left
             (fun (acc1,acc2,acc3) case ->
                match case with
                | Result(r,effects,cleaners) -> r::acc1,meet_teffect acc2 effects,StmtSet.union acc3 cleaners
                | _ -> assert false
             ) ([],empty_teffect,StmtSet.empty) cl in
         let handled_res = f rl flow |>
                           add_cleaners (StmtSet.elements cleaners) |>
                           concat_effects effects in
         if others = [] then
           handled_res
         else
           meet_list (List.map (fun (c,flow) -> case c flow) others) ~empty:(fun () -> assert false) |>
           meet handled_res
    ) cases

let bind_disjunction
    (f:('r case * 'a flow) list -> ('a,'s) cases)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  let ctx,ret =
    Dnf.fold_bind_disjunction
      (fun ctx disj ->
         let disj' = List.map (fun (case,flow) -> (case,Flow.set_ctx ctx flow)) disj in
         let cases' = f disj' in
         let ctx' = get_ctx cases' in
         ctx',cases'.cases
      ) (get_ctx cases) cases.cases in
  set_ctx ctx {cases with cases=ret}

let bind_disjunction_result
    (f:'r list -> 'a flow -> ('a,'s) cases)
    (lattice:'a lattice)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  bind_disjunction
    (fun disj ->
       (* Separate cases actual results from empty and not-handled cases *)
       let handled,others = List.partition (fun (case,flow) -> match case with Result _ -> true | _ -> false) disj in
       (* This is a hack to change the type of others from 'r case to 's case *)
       let others = List.map (fun (case,flow) -> match case with NotHandled -> NotHandled,flow | Empty -> Empty,flow | _ -> assert false) others in
       if handled = [] then
         join_list (List.map (fun (c,flow) -> case c flow) others) ~empty:(fun () -> assert false)
       else
         let cl,fl = List.split handled in
         let flow = List.fold_left (Flow.join lattice) (List.hd fl) (List.tl fl) in
         let rl,effects,cleaners =
           List.fold_left
             (fun (acc1,acc2,acc3) case ->
                match case with
                | Result(r,effects,cleaners) -> r::acc1,join_teffect acc2 effects,StmtSet.union acc3 cleaners
                | _ -> assert false
             ) ([],empty_teffect,StmtSet.empty) cl in
         let handled_res = f rl flow |>
                           add_cleaners (StmtSet.elements cleaners) |>
                           concat_effects effects in
         if others = [] then
           handled_res
         else
           join_list (List.map (fun (c,flow) -> case c flow) others) ~empty:(fun () -> assert false) |>
           join handled_res
    ) cases


let bind_list_opt
    (l:'r list)
    (f:'r -> 'a flow -> ('a,'s) cases option)
    (flow:'a flow)
  : ('a, 's list) cases option
  =
  let rec aux l flow =
    match l with
    | e :: tl ->
      f e flow |>
      OptionExt.absorb @@ bind_result_opt @@ fun e' flow ->
      aux tl flow |>
      OptionExt.lift @@ bind_result @@ fun tl' flow ->
      return (e'::tl') flow


    | [] ->
      return [] flow |>
      OptionExt.return
  in
  aux l flow


let bind_list l f flow =
  bind_list_opt l (fun e flow -> Some (f e flow)) flow |>
  OptionExt.none_to_exn
