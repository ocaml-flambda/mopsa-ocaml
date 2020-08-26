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


(****************************************************************************)
(**                           {2 Definition}                                *)
(****************************************************************************)

(** Single case *)
type ('a,'r) case = {
  case_result   : 'r option;     (** Case result *)
  case_flow     : 'a TokenMap.t; (** Token map of abstract states *)
  case_alarms   : AlarmSet.t;    (** Collected alarms *)
  case_log      : log;           (** Journal of statements log *)
  case_cleaners : block;         (** Cleaner statements *)
}

(** Multiple cases encoded as a DNF *)
type ('a,'r) cases = {
  cases_dnf : ('a,'r) case Dnf.t; (** DNF of cases *)
  cases_ctx : 'a ctx;             (** Flow-insensitive context *)
}


(****************************************************************************)
(**                      {2 Utility functions}                              *)
(****************************************************************************)

let mk_case
    ?(cleaners=[])
    ?(log=empty_log)
    (result:'r option)
    (flow:'a flow)
  : ('a,'r) case =
  {
    case_result = result;
    case_flow = Flow.get_token_map flow;
    case_alarms = Flow.get_alarms flow;
    case_log = log;
    case_cleaners = cleaners;
  }


let return
    ?(cleaners=[])
    ?(log=empty_log)
    (output:'r option)
    (flow:'a flow)
  : ('a,'r) cases
  =
  {
    cases_dnf = Dnf.singleton (mk_case output flow ~log ~cleaners);
    cases_ctx = Flow.get_ctx flow;
  }



let singleton
    ?(cleaners=[])
    (output:'r)
    (flow:'a flow)
  : ('a,'r) cases
  =
  return (Some output) flow ~cleaners



let empty_singleton (flow:'a flow) : ('a,'r) cases =
  Flow.remove T_cur flow |>
  return None


let opt_clean_cur_only = ref false


let get_ctx r = r.cases_ctx


let set_ctx ctx r =
  if ctx == r.cases_ctx then r else { r with cases_ctx = ctx }


let copy_ctx src dst =
  set_ctx (get_ctx src) dst


let get_callstack r =
  get_ctx r |>
  Context.find_unit Context.callstack_ctx_key


let map_cases
    (f:('a,'r) case -> ('a,'s) case)
    (r:('a,'r) cases)
  : ('a,'s) cases =
  {
    cases_dnf = r.cases_dnf |> Dnf.map f;
    cases_ctx = r.cases_ctx;
  }


let map (f:'r->'s) (r:('a,'r) cases) : ('a,'s) cases =
  map_cases (fun case ->
      let output = match case.case_result with
        | Some o -> Some (f o)
        | None -> None
      in
      {
        case_result = output;
        case_flow = case.case_flow;
        case_alarms = case.case_alarms;
        case_log = case.case_log;
        case_cleaners = case.case_cleaners;
      }
    ) r



(** Map each case with with function [f] if the return value is
    non-empty, otherwise remove the case *)
let map_opt (f:'r -> 's option option) (r:('a,'r) cases) : ('a,'s) cases =
  let cases =
    Dnf.apply
      (fun case ->
         match case.case_result with
         | None -> Some (Dnf.singleton { case with case_result = None })
         | Some o ->
           match f o with
           | None -> None
           | Some rr -> Some (Dnf.singleton { case with case_result = rr })
      )
      (OptionExt.neutral2 Dnf.mk_or) (OptionExt.neutral2 Dnf.mk_and)
      r.cases_dnf
  in
  match cases with
  | Some c -> { r with cases_dnf = c }
  | None -> Exceptions.panic ~loc:__LOC__ "map_opt: empty result"


let add_cleaners_case cleaners c =
  { c with case_cleaners = c.case_cleaners @ cleaners }


let add_cleaners cleaners r =
  r |> map_cases (add_cleaners_case cleaners)


let apply_full f join meet r =
  Dnf.apply (fun case ->
      let flow = Flow.create r.cases_ctx case.case_alarms case.case_flow in
      f case.case_result flow case.case_log case.case_cleaners
    ) join meet r.cases_dnf


let apply f join meet r =
  apply_full (fun out flow _ _ -> f out flow) join meet r

let apply_some f join meet bot r =
  apply (fun out flow ->
      match out with
      | None -> bot
      | Some o -> f o flow)
    join meet r


let print pp fmt r =
  Dnf.print (fun fmt case ->
      let flow = Flow.create r.cases_ctx case.case_alarms case.case_flow in
      pp fmt case.case_result flow
    )
    fmt r.cases_dnf


let print_some pp fmt r =
  print (fun fmt oo flow ->
      match oo with
      | None -> Format.fprintf fmt "ε"
      | Some o -> pp fmt o flow
    )
    fmt r


let map_log (f:log -> log) (r:('a,'r) cases) : ('a,'r) cases =
  r |> map_cases (fun case ->
      {
        case with case_log = f case.case_log;
      }
    )

let set_log log (r:('a,'r) cases) : ('a,'r) cases =
  r |> map_cases (fun case ->
      { case with
        case_log = log }
    )

let clear_log (r:('a,'r) cases) : ('a,'r) cases =
  r |> map_cases (fun case ->
      { case with
        case_log = empty_log }
    )

let concat_cases_log (old:log) (recent:('a,'r) cases) : ('a,'r) cases =
  recent |> map_cases (fun recent ->
      { recent with
        case_log =
          (* Add logs of non-empty environments only *)
          (* FIXME: Since are always called from the binders, we can't
             require having the lattice manager. So we can't test if
             T_cur is ⊥ or not! For the moment, we rely on empty flow
             maps, but this is not always sufficient.
          *)
          if TokenMap.is_empty recent.case_flow
          then old
          else concat_log ~old ~recent:recent.case_log }
    )


let map_fold_conjunctions
    (f: 'a flow * log -> 'a flow * log -> 'a flow)
    (r:('a,'r) cases)
    : ('a,'r) cases
  =
  let l = Dnf.to_list r.cases_dnf in
  let ctx = r.cases_ctx in
  let l' = List.map (fun conj ->
      match conj with
      | [] -> assert false
      | [x] -> [x]
      | hd :: tl ->
        let flow,log,cleaners =
          tl |> List.fold_left (fun (flow, log, cleaners) case ->
              let flow' = Flow.create ctx case.case_alarms case.case_flow in
              let flow = f (flow,log) (flow',case.case_log) in
              flow, concat_log log case.case_log, cleaners @ case.case_cleaners
            ) (Flow.create ctx hd.case_alarms hd.case_flow, hd.case_log, hd.case_cleaners)
        in
        hd :: tl |> List.map (fun case -> {
              case_result = case.case_result;
              case_flow = Flow.get_token_map flow;
              case_alarms = Flow.get_alarms flow;
              case_log = log;
              case_cleaners = cleaners
            }
          )
    ) l
  in
  { r with cases_dnf = Dnf.from_list l' }


let fold
    (f:'r option -> 'a flow -> 'b -> 'b)
    (c:('a,'r) cases)
    (init:'b)
  : 'b =
  let flat_cases = Dnf.to_list c.cases_dnf |>
                   List.flatten
  in
  List.fold_left (fun acc case ->
      let flow = Flow.create c.cases_ctx case.case_alarms case.case_flow in
      f case.case_result flow acc
    ) init flat_cases


let fold_some
    (f:'r -> 'a flow -> 'b -> 'b)
    (c:('a,'r) cases)
    (init:'b)
  : 'b =
  fold (fun r flow acc ->
      match r with
      | None -> acc
      | Some rr -> f rr flow acc
    ) c init

let for_all
    (f:'r option -> 'a flow -> bool)
    (c:('a,'r) cases)
  : bool =
  let flat_cases = Dnf.to_list c.cases_dnf |>
                   List.flatten
  in
  List.for_all (fun case ->
      let flow = Flow.create c.cases_ctx case.case_alarms case.case_flow in
      f case.case_result flow
    ) flat_cases

let for_all_some
    (f:'r -> 'a flow -> bool)
    (c:('a,'r) cases)
  : bool =
  for_all (fun r flow ->
      match r with
      | None -> true
      | Some rr -> f rr flow
    ) c

let exists
    (f:'r option -> 'a flow -> bool)
    (c:('a,'r) cases)
  : bool =
  let flat_cases = Dnf.to_list c.cases_dnf |>
                   List.flatten
  in
  List.exists (fun case ->
      let flow = Flow.create c.cases_ctx case.case_alarms case.case_flow in
      f case.case_result flow
    ) flat_cases

let exists_some
    (f:'r -> 'a flow -> bool)
    (c:('a,'r) cases)
  : bool =
  exists (fun r flow ->
      match r with
      | None -> false
      | Some rr -> f rr flow
    ) c

(****************************************************************************)
(**                       {2 Lattice operators}                             *)
(****************************************************************************)

let is_empty (r:('a,'r) cases) : bool =
  Dnf.to_list r.cases_dnf |>
  List.for_all (
    List.for_all (fun case -> AlarmSet.is_empty case.case_alarms &&
                              TokenMap.is_empty case.case_flow
                 )
  )

(** Join two results *)
let join (r1:('a,'r) cases) (r2:('a,'r) cases) : ('a,'r) cases =
  if is_empty r1 then r2
  else if is_empty r2 then r1
  else
  {
    cases_dnf = Dnf.mk_or r1.cases_dnf r2.cases_dnf;
    cases_ctx = Context.get_most_recent r1.cases_ctx r2.cases_ctx;
  }


(** Meet two results *)
let meet (r1:('a,'r) cases) (r2:('a,'r) cases) : ('a,'r) cases =
  {
    cases_dnf = Dnf.mk_and r1.cases_dnf r2.cases_dnf;
    cases_ctx = Context.get_most_recent r1.cases_ctx r2.cases_ctx;
  }


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



(****************************************************************************)
(**                        {2 Monadic binders}                              *)
(****************************************************************************)

let bind_full_opt
    (f: 'r option -> 'a flow -> log -> stmt list -> ('a,'s) cases option )
    (r: ('a,'r) cases)
  : ('a,'s) cases option =
  let ctx, ret = Dnf.fold_apply
      (fun ctx case ->
         let flow' = Flow.create ctx case.case_alarms case.case_flow in
         let r' = f case.case_result flow' case.case_log case.case_cleaners in
         let ctx = OptionExt.apply get_ctx ctx r' in
         (ctx,r')
      )
      (OptionExt.neutral2 join)
      (OptionExt.neutral2 meet)
      (get_ctx r) r.cases_dnf
  in
  OptionExt.lift (set_ctx ctx) ret


let (>>*?) r f = bind_full_opt f r



let bind_full f r =
  bind_full_opt (fun e flow log cleaners -> Some (f e flow log cleaners)) r |>
  OptionExt.none_to_exn

let (>>*) r f = bind_full f r



let bind_opt
    (f: 'r option -> 'a flow -> ('a,'s) cases option )
    (r: ('a,'r) cases)
  : ('a,'s) cases option =
  r |> bind_full_opt (fun r flow log cleaners ->
      f r flow |>
      OptionExt.lift (add_cleaners cleaners) |>
      OptionExt.lift (concat_cases_log log)
    )


let (>>=?) r f = bind_opt f r


let bind f r =
  bind_opt (fun e flow -> Some (f e flow)) r |>
  OptionExt.none_to_exn

let (>>=) r f = bind f r


let bind_some_opt
    (f:'r -> 'a flow -> ('a,'s) cases option)
    (r:('a,'r) cases)
  : ('a,'s) cases option
  =
  r |> bind_opt @@ fun r flow ->
  match r with
  | None -> Some (empty_singleton flow)
  | Some rr -> f rr flow


let (>>$?) r f = bind_some_opt f r


let bind_some
    (f:'r -> 'a flow -> ('a,'s) cases)
    (r:('a,'r) cases)
  : ('a,'s) cases
  =
  bind_some_opt (fun r flow -> Some (f r flow)) r |>
  OptionExt.none_to_exn


let (>>$) r f = bind_some f r


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
      OptionExt.absorb @@ bind_some_opt @@ fun e' flow ->
      aux tl flow |>
      OptionExt.lift @@ bind_some @@ fun tl' flow ->
      singleton (e'::tl') flow


    | [] ->
      singleton [] flow |>
      OptionExt.return
  in
  aux l flow


let bind_list l f flow =
  bind_list_opt l (fun e flow -> Some (f e flow)) flow |>
  OptionExt.none_to_exn


let remove_duplicates compare lattice r =
  let ctx = r.cases_ctx in
  let compare_case case case' = compare case.case_result case'.case_result in
  (* Logs of empty environments should be ignored.
     This function returns an empty log when T_cur environment is empty. *)
  let real_log flow log =
    if lattice.Lattice.is_bottom (TokenMap.get T_cur lattice flow)
    then empty_log
    else log
  in
  let rec simplify_conj conj =
    match conj with
    | [] -> conj
    | [case] -> [case]
    | case :: tl ->
      (* Remove duplicates of case from tl *)
      let case', tl' =
        let rec aux = function
          | [] -> case, []
          | case' :: tl' ->
            let case, tl'' = aux tl' in
            match compare_case case case' with
            | 0 ->
              let flow = TokenMap.meet lattice (Context.get_unit ctx) case.case_flow case'.case_flow in
              let case'' = {
                case_result = case.case_result;
                case_flow = flow;
                case_cleaners = case.case_cleaners @ case'.case_cleaners;
                case_alarms = AlarmSet.inter case.case_alarms case'.case_alarms;
                case_log = meet_log case.case_log  case'.case_log |>
                           real_log flow;
              }
              in
              case'', tl''
            | _ -> case, case' :: tl''
        in
        aux tl
      in
      case' :: simplify_conj tl'
  in
  let join_conj conj conj' =
    List.combine conj conj' |>
    List.map (fun (case, case') ->
        {
          case_result = case.case_result;
          case_flow = TokenMap.join lattice (Context.get_unit ctx) case.case_flow case'.case_flow;
          case_cleaners = case.case_cleaners @ case'.case_cleaners;
          case_alarms = AlarmSet.union case.case_alarms case'.case_alarms;
          case_log = join_log
              (real_log case.case_flow case.case_log)
              (real_log case'.case_flow case'.case_log);
        }
      )
  in
  let rec simplify_disj disj =
    match disj with
    | [] -> disj
    | conj :: tl ->
      let conj = simplify_conj conj in
      (* Remove duplicates of conj from tl *)
      let conj', tl' =
        let rec aux = function
          | [] -> conj, []
          | conj' :: tl' ->
            let conj, tl'' = aux tl' in
            match Compare.list compare_case conj conj' with
            | 0 -> join_conj conj conj', tl''
            | _ -> conj, conj' :: tl''
        in
        aux tl
      in
      conj' :: simplify_disj tl'
  in
  { r with cases_dnf = Dnf.from_list (simplify_disj (Dnf.to_list r.cases_dnf)) }

let remove_duplicates_some compare lattice r =
  remove_duplicates (OptionExt.compare compare) lattice r


let cardinal r = Dnf.cardinal r.cases_dnf
