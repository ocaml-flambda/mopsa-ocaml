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


open Ast.Stmt
open Flow
open Token
open Log
open Context
open Alarm
open Lattice

type cleaners = StmtSet.t

(** Single case of a computation *)
type 'r case =
  | Result of 'r * log * cleaners
  | Empty
  | NotHandled

(** Multiple cases of a computation, encoded as a DNF *)
type ('a,'r) cases = ('r case * 'a flow) Dnf.t

let case (case:'r case) flow : ('a,'r) cases = Dnf.singleton (case,flow)

let return ?(log=empty_log) ?(cleaners=[]) (res:'r) (flow:'a flow) =
  case (Result (res,log,StmtSet.of_list cleaners)) flow

let singleton = return

let empty ?(bottom=true) (flow:'a flow) : ('a,'r) cases =
  let flow = if bottom then Flow.remove T_cur flow else flow in
  case Empty flow

let not_handled (flow:'a flow) : ('a,'r) cases =
  case NotHandled flow


let opt_clean_cur_only = ref false


let get_ctx cases =
  match Dnf.choose cases with
  | None             -> assert false
  | Some (case,flow) -> Flow.get_ctx flow

let set_ctx ctx cases =
  Dnf.map
    (fun (case,flow) -> (case, Flow.set_ctx ctx flow))
    cases

let copy_ctx src dst =
  set_ctx (get_ctx src) dst

let get_most_recent_ctx cases =
  Dnf.fold
    (fun acc (case,flow) -> Context.get_most_recent acc (Flow.get_ctx flow))
    (get_ctx cases) cases

let normalize_ctx cases =
  let ctx = get_most_recent_ctx cases in
  set_ctx ctx cases

let get_callstack r =
  get_ctx r |>
  Context.find_unit Context.callstack_ctx_key

let get_case_cleaners (case:'r case) : StmtSet.t =
  match case with
  | Result(_,_,cleaners) -> cleaners
  | Empty | NotHandled   -> StmtSet.empty

let set_case_cleaners (cleaners:StmtSet.t) (case:'r case) : 'r case =
  match case with
  | Result(r,log,_) -> Result(r,log,cleaners)
  | _ -> case

let get_case_log (case:'r case) : log =
  match case with
  | Result(_,log,_)    -> log
  | Empty | NotHandled -> empty_log

let set_case_log (log:log) (case:'r case) : 'r case =
  match case with
  | Result(r,_,cleaners) -> Result(r,log,cleaners)
  | _ -> case


let map
    (f:'r case -> 'a flow -> 's case * 'a flow)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  Dnf.map (fun (case,flow) -> f case flow) cases |>
  normalize_ctx


let map_result
    (f:'r->'s)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  map (fun case flow ->
      let case' =
        match case with
        | Result (r,log,cleaners) -> Result (f r,log,cleaners)
        | Empty                   -> Empty
        | NotHandled              -> NotHandled
      in
      (case',flow)
    ) cases

let map_conjunction
    (f:('r case * 'a flow) list -> ('s case * 'a flow) list)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  Dnf.map_conjunction f cases |>
  normalize_ctx

let map_disjunction
    (f:('r case * 'a flow) list -> ('s case * 'a flow) list)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  Dnf.map_disjunction f cases |>
  normalize_ctx


let reduce
    (f:'r case -> 'a flow -> 'b)
    ~(join:'b -> 'b -> 'b)
    ~(meet:'b -> 'b -> 'b)
    (cases:('a,'r) cases)
  : 'b =
  Dnf.reduce (fun (case,flow) -> f case flow) ~join ~meet cases

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
       | Result (r,log,cleaners) -> f r flow
       | Empty | NotHandled      -> bottom)
    ~join ~meet cases


let print pp fmt cases =
  Dnf.print (fun fmt (case,flow) -> pp fmt case flow) fmt cases


let print_result pp fmt cases =
  print (fun fmt case flow ->
      match case with
      | Result (r,_,_) -> pp fmt r flow
      | Empty          -> Format.fprintf fmt "ε"
      | NotHandled     -> Format.fprintf fmt "✗"
    )
    fmt cases


let map_log
    (f:log -> log)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  map
    (fun case flow ->
       match case with
       | Result(r,log,cleaners) -> Result(r,f log,cleaners), flow
       | _                      -> case, flow
    ) cases

let set_log
    (log:log)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  map
    (fun case flow ->
       match case with
       | Result(r,_,cleaners) -> Result(r,log,cleaners), flow
       | _                    -> case, flow
    ) cases

let set_cleaners
    (cleaners:stmt list)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  let cleaners = StmtSet.of_list cleaners in
  map
    (fun case flow ->
       match case with
       | Result(r,log,_) -> Result(r,log,cleaners), flow
       | _               -> case, flow
    ) cases


let concat_log
    (old:log)
    (cases:('a,'r) cases)
  : ('a,'r) cases =
  map
    (fun case flow ->
       match case with
       | Result(r,recent,cleaners) ->
         (* Add logs of non-empty environments only *)
         (* FIXME: Since are always called from the binders, we can't
              require having the lattice manager. So we can't test if
              T_cur is ⊥ or not! For the moment, we rely on empty flow
              maps, but this is not always sufficient.
         *)
         if Flow.mem T_cur flow then
           Result(r, Log.concat_log ~old ~recent, cleaners), flow
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
       | Result(r,log,cleaners') -> Result(r,log,StmtSet.union cleaners' cleaners), flow
       | _ -> case, flow
    ) cases

let fold
    (f:'b -> 'r case -> 'a flow -> 'b)
    (init:'b)
    (cases:('a,'r) cases)
  : 'b =
  Dnf.fold (fun acc (case,flow) -> f acc case flow) init cases


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

let partition
    (f:'r case -> 'a flow -> bool)
    (cases:('a,'r) cases)
  : ('a,'r) cases option * ('a,'r) cases option =
  let dnf1, dnf2 = Dnf.partition (fun (case,flow) -> f case flow) cases in
  let c1 = if Dnf.is_empty dnf1 then None else Some dnf1 in
  let c2 = if Dnf.is_empty dnf2 then None else Some dnf2 in
  c1, c2


let flatten (cases:('a,'r) cases) : ('r case * 'a flow) list =
  Dnf.to_list cases |>
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
  Dnf.mk_or cases1 cases2 |>
  normalize_ctx

(** Meet two results *)
let meet (cases1:('a,'r) cases) (cases2:('a,'r) cases) : ('a,'r) cases =
  Dnf.mk_or cases1 cases2 |>
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


let remove_duplicates compare_case lattice cases =
  (* Logs of empty environments should be ignored.
     This function returns an empty log when T_cur environment is empty. *)
  let real_log flow log =
    if lattice.Lattice.is_bottom (Flow.get T_cur lattice flow)
    then empty_log
    else log
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
                     set_case_log (meet_log (get_case_log case') (get_case_log case'') |> real_log flow) in
          case,flow,tl''
        | _ -> case'', flow'', (case',flow')::tl''
  in
  (* Remove all duplicates in a conjunction *)
  let rec remove_duplicates_in_conj conj =
    match conj with
    | [] | [(_,_)] -> conj
    | (case,flow) :: tl ->
      (* Remove duplicates of case from tl *)
      let case', flow', tl' = remove_case_duplicates_in_conj case flow tl in
      (case',flow') :: remove_duplicates_in_conj tl'
  in
  (* Remove all duplicates in all conjunctions *)
  let cases = map_conjunction remove_duplicates_in_conj cases in
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
                          set_case_log (join_log (get_case_log case) (get_case_log case') |> real_log flow) in
               case,flow
            )
        in
        conj,tl'
      | _ ->
        conj'',conj'::tl'
  in
  let rec remove_duplicates_in_disj = function
    | [] -> []
    | conj::tl ->
      let conj',tl' = remove_conj_duplicates_in_disj conj tl in
      conj'::remove_duplicates_in_disj tl'
  in
  Dnf.from_list (remove_duplicates_in_disj (Dnf.to_list cases))

let remove_duplicate_results compare_results lattice cases =
  remove_duplicates
    (fun case case' ->
       match case, case' with
       | Result(r,_,_), Result(r',_,_) -> compare_results r r'
       | _                             -> compare case case'
    ) lattice cases


let cardinal cases = Dnf.cardinal cases


(****************************************************************************)
(**                        {2 Monadic binders}                              *)
(****************************************************************************)

let bind_opt
    (f: 'r case -> 'a flow -> ('a,'s) cases option )
    (cases: ('a,'r) cases)
  : ('a,'s) cases option =
  Dnf.bind
    (fun (case,flow) ->
       let cases' =
         match f case flow with
           | None   -> not_handled flow
           | Some c -> c
         in
         add_cleaners (get_case_cleaners case |> StmtSet.elements) cases' |>
         concat_log (get_case_log case)
      )
      cases
  |> normalize_ctx
  |> OptionExt.return


let (>>=?) cases f = bind_opt f cases

let bind f cases =
  bind_opt (fun case flow -> Some (f case flow)) cases |>
  OptionExt.none_to_exn

let (>>=) cases f = bind f cases


let bind_result_opt
    (f:'r -> 'a flow -> ('a,'s) cases option)
    (cases:('a,'r) cases)
  : ('a,'s) cases option
  =
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
  bind_result_opt (fun r flow -> Some (f r flow)) cases |>
  OptionExt.none_to_exn


let (>>$) r f = bind_result f r


let bind_conjunction
    (f:('r case * 'a flow) list -> ('a,'s) cases)
    (cases:('a,'r) cases)
  : ('a,'s) cases =
  Dnf.bind_conjunction f cases |>
  normalize_ctx

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
         let rl,log,cleaners =
           List.fold_left
             (fun (acc1,acc2,acc3) case ->
                match case with
                | Result(r,log,cleaners) -> r::acc1,meet_log acc2 log,StmtSet.union acc3 cleaners
                | _ -> assert false
             ) ([],empty_log,StmtSet.empty) cl in
         let handled_res = f rl flow |>
                           add_cleaners (StmtSet.elements cleaners) |>
                           concat_log log in
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
  Dnf.bind_disjunction f cases |>
  normalize_ctx

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
         let rl,log,cleaners =
           List.fold_left
             (fun (acc1,acc2,acc3) case ->
                match case with
                | Result(r,log,cleaners) -> r::acc1,join_log acc2 log,StmtSet.union acc3 cleaners
                | _ -> assert false
             ) ([],empty_log,StmtSet.empty) cl in
         let handled_res = f rl flow |>
                           add_cleaners (StmtSet.elements cleaners) |>
                           concat_log log in
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
