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


(** Encoding of results returned by transfer functions. *)


open Ast.All
open Flow
open Token
open Log
open Context
open Alarm



(****************************************************************************)
(**                           {2 Definition}                                *)
(****************************************************************************)

(** Type of a single case of a result *)
type ('a,'r) case = {
  case_output   : 'r option;     (** Case output *)
  case_flow     : 'a TokenMap.t; (** Token map of abstract states *)
  case_alarms   : AlarmSet.t;    (** Collected alarms *)
  case_log      : log;           (** Journal of statements log *)
  case_cleaners : block;         (** Cleaner statements *)
}

(** Type of a result *)
type ('a,'r) result = {
  res_cases : ('a,'r) case Dnf.t; (** DNF of cases *)
  res_ctx   : 'a ctx;             (** Flow-insensitive context *)
}




(****************************************************************************)
(**                      {2 Utility functions}                              *)
(****************************************************************************)

(** Create a case for one output and flow *)
let mk_case
    ?(cleaners=[])
    ?(log=Log.empty)
    (output:'r option)
    (flow:'a flow)
  : ('a,'r) case =
  {
    case_output = output;
    case_flow = Flow.get_token_map flow;
    case_alarms = Flow.get_alarms flow;
    case_log = log;
    case_cleaners = cleaners;
  }


(** Create a singleton result *)
let return
    ?(cleaners=[])
    ?(log=Log.empty)
    (output:'r option)
    (flow:'a flow)
  : ('a,'r) result
  =
  {
    res_cases = Dnf.singleton (mk_case output flow ~log ~cleaners);
    res_ctx = Flow.get_ctx flow;
  }



(** Create a singleton result *)
let singleton
    ?(cleaners=[])
    (output:'r)
    (flow:'a flow)
  : ('a,'r) result
  =
  return (Some output) flow ~cleaners



(** Create an empty singleton result *)
let empty (flow:'a flow) : ('a,'r) result =
  return None flow

let empty_singleton = empty


(** Get the flow-insensitive context of a result *)
let get_ctx r = r.res_ctx


(** Set the flow-insensitive context of a result *)
let set_ctx ctx r = { r with res_ctx = ctx }


let copy_ctx src dst =
  set_ctx (get_ctx src) dst


(** Map each case of a result with function [f] *)
let map_cases
    (f:('a,'r) case -> ('a,'s) case)
    (r:('a,'r) result)
  : ('a,'s) result =
  {
    res_cases = r.res_cases |> Dnf.map f;
    res_ctx = r.res_ctx;
  }


(** Map outputs of a result *)
let map (f:'r->'r) (r:('a,'r) result) : ('a,'r) result =
  map_cases (fun case ->
      match case.case_output with
      | Some o -> { case with case_output = Some (f o) }
      | None -> case
    ) r
  


(** Map each case with with function [f] if the return value is
    non-empty, otherwise remove the case *)
let map_opt (f:'r -> 's option option) (r:('a,'r) result) : ('a,'s) result =
  let cases =
    Dnf.apply
      (fun case ->
         match case.case_output with
         | None -> Some (Dnf.singleton { case with case_output = None })
         | Some o ->
           match f o with
           | None -> None
           | Some rr -> Some (Dnf.singleton { case with case_output = rr })
      )
      (Option.neutral2 Dnf.mk_or) (Option.neutral2 Dnf.mk_and)
      r.res_cases
  in
  match cases with
  | Some c -> { r with res_cases = c }
  | None -> Exceptions.panic ~loc:__LOC__ "map_opt: empty result"


(** Add cleaners to a case *)
let add_cleaners_case cleaners c =
  { c with case_cleaners = concat_blocks c.case_cleaners cleaners }


(** Add cleaners to all cases of a result *)
let add_cleaners cleaners r =
  r |> map_cases (add_cleaners_case cleaners)


let apply_full f join meet r =
  Dnf.apply (fun case ->
      let flow = Flow.create r.res_ctx case.case_alarms case.case_flow in
      f case.case_output flow case.case_log case.case_cleaners
    ) join meet r.res_cases


let apply f join meet r =
  apply_full (fun out flow _ _ -> f out flow) join meet r


let print_full pp fmt r =
  Dnf.print (fun fmt case ->
      let flow = Flow.create r.res_ctx case.case_alarms case.case_flow in
      pp fmt case.case_output flow
    )
    fmt r.res_cases


(** Print a result *)
let print pp fmt r =
  print_full (fun fmt oo flow ->
      match oo with
      | None -> Format.fprintf fmt "ε"
      | Some o -> pp fmt o flow
    )
    fmt r


(** Map log of each case of a result *)
let map_log (f:log -> log) (r:('a,'r) result) : ('a,'r) result =
  r |> map_cases (fun case ->
      {
        case with case_log = f case.case_log;
      }
    )


(** Concatenate a log to the log a result *)
let concat_log (log:log) (r:('a,'r) result) : ('a,'r) result =
  r |> map_cases (fun case ->
      {
        case with case_log = Log.concat case.case_log log;
      }
    )


(** Merge flows of conjunctions of two results and keep outputs unmodified *)
let merge_conjunctions_flow
    (f: 'a flow * log -> 'a flow * log -> 'a flow)
    (r:('a,'r) result)
    : ('a,'r) result
  =
  let l = Dnf.to_list r.res_cases in
  let ctx = r.res_ctx in
  let l' = List.map (fun conj ->
      match conj with
      | [] -> assert false
      | [x] -> [x]
      | hd :: tl ->
        let flow,log,cleaners =
          tl |> List.fold_left (fun (flow, log, cleaners) case ->
              let flow' = Flow.create ctx case.case_alarms case.case_flow in
              let flow = f (flow,log) (flow',case.case_log) in
              flow, Log.concat log case.case_log, concat_blocks cleaners case.case_cleaners
            ) (Flow.create ctx hd.case_alarms hd.case_flow, hd.case_log, hd.case_cleaners)
        in
        hd :: tl |> List.map (fun case -> {
              case_output = case.case_output;
              case_flow = Flow.get_token_map flow;
              case_alarms = Flow.get_alarms flow;
              case_log = log;
              case_cleaners = cleaners
            }
          )
    ) l
  in
  { r with res_cases = Dnf.from_list l' }


(****************************************************************************)
(**                       {2 Lattice operators}                             *)
(****************************************************************************)

let is_empty (r:('a,'r) result) : bool =
  Dnf.to_list r.res_cases |>
  List.for_all (
    List.for_all (fun case -> AlarmSet.is_empty case.case_alarms &&
                              TokenMap.is_empty case.case_flow
                 )
  )

(** Join two results *)
let join (r1:('a,'r) result) (r2:('a,'r) result) : ('a,'r) result =
  if is_empty r1 then r2
  else if is_empty r2 then r1
  else
  {
    res_cases = Dnf.mk_or r1.res_cases r2.res_cases;
    res_ctx = Context.get_most_recent r1.res_ctx r2.res_ctx;
  }


(** Meet two results *)
let meet (r1:('a,'r) result) (r2:('a,'r) result) : ('a,'r) result =
  {
    res_cases = Dnf.mk_and r1.res_cases r2.res_cases;
    res_ctx = Context.get_most_recent r1.res_ctx r2.res_ctx;
  }


(** Join a list of results *)
let join_list ~empty (l: ('a,'r) result list) : ('a,'r) result =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left join hd tl


(** Meet a list of results *)
let meet_list ~empty (l: ('a,'r) result list) : ('a,'r) result =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left meet hd tl



(****************************************************************************)
(**                        {2 Monadic binders}                              *)
(****************************************************************************)

let bind_full_opt
    (f: 'r option -> 'a flow -> log -> stmt list -> ('a,'s) result option )
    (r: ('a,'r) result)
  : ('a,'s) result option =
  let ctx, ret = Dnf.fold_apply
      (fun ctx case ->
         let flow' = Flow.create ctx case.case_alarms case.case_flow in
         let r' = f case.case_output flow' case.case_log case.case_cleaners in
         let ctx = Option.apply get_ctx ctx r' in
         (ctx,r')
      )
      (Option.neutral2 join)
      (Option.neutral2 meet)
      (get_ctx r) r.res_cases
  in
  Option.lift (set_ctx ctx) ret


let (>>*?) r f = bind_full_opt f r




let bind_full f r =
  bind_full_opt (fun e flow log cleaners -> Some (f e flow log cleaners)) r |>
  Option.none_to_exn

let (>>*) r f = bind_full f r




let bind_opt
    (f: 'r option -> 'a flow -> ('a,'s) result option )
    (r: ('a,'r) result)
  : ('a,'s) result option =
  r |> bind_full_opt (fun r flow log cleaners ->
      f r flow |>
      Option.lift (add_cleaners cleaners) |>
      Option.lift (concat_log log)
    )


let (>>=?) r f = bind_opt f r




let bind f r =
  bind_opt (fun e flow -> Some (f e flow)) r |>
  Option.none_to_exn

let (>>=) r f = bind f r


let bind_some_opt
    (f:'r -> 'a flow -> ('a,'s) result option)
    (r:('a,'r) result)
  : ('a,'s) result option
  =
  r |> bind_opt @@ fun r flow ->
  match r with
  | None -> Some (empty flow)
  | Some rr -> f rr flow


let (>>$?) r f = bind_some_opt f r


let bind_some
    (f:'r -> 'a flow -> ('a,'s) result)
    (r:('a,'r) result)
  : ('a,'s) result
  =
  bind_some_opt (fun r flow -> Some (f r flow)) r |>
  Option.none_to_exn


let (>>$) r f = bind_some f r

let bind_list_opt
    (l:'r list)
    (f:'r -> 'a flow -> ('a,'s) result option)
    (flow:'a flow)
  : ('a, 's list) result option
  =
  let rec aux l flow =
    match l with
    | e :: tl ->
      f e flow |>
      Option.absorb @@ bind_some_opt @@ fun e' flow ->
      aux tl flow |>
      Option.lift @@ bind_some @@ fun tl' flow ->
      singleton (e'::tl') flow


    | [] ->
      singleton [] flow |>
      Option.return
  in
  aux l flow


let bind_list l f flow =
  bind_list_opt l (fun e flow -> Some (f e flow)) flow |>
  Option.none_to_exn


let remove_duplicates compare lattice r =
  let ctx = r.res_ctx in
  let compare_case case case' = Option.compare compare case.case_output case'.case_output in
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
              let case'' = {
                case_output = case.case_output;
                case_flow = TokenMap.meet lattice (Context.get_unit ctx) case.case_flow case'.case_flow;
                case_cleaners = case.case_cleaners @ case'.case_cleaners;
                case_alarms = AlarmSet.meet case.case_alarms case'.case_alarms;
                case_log = Log.concat case.case_log case.case_log;
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
          case_output = case.case_output;
          case_flow = TokenMap.join lattice (Context.get_unit ctx) case.case_flow case'.case_flow;
          case_cleaners = case.case_cleaners @ case'.case_cleaners;
          case_alarms = AlarmSet.join case.case_alarms case'.case_alarms;
          case_log = Log.concat case.case_log case.case_log;
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
  { r with res_cases = Dnf.from_list (simplify_disj (Dnf.to_list r.res_cases)) }
