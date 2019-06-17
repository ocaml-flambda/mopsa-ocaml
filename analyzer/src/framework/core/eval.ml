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


(** Evaluation of expressions *)

open Ast.Stmt
open Token
open Flow
open Zone
open Context


(*==========================================================================*)
(**                          {2 Evaluations}                                *)
(*==========================================================================*)


type ('e, 'a) case = {
  eval_result : 'e option;
  eval_tmap: 'a TokenMap.t;
  eval_cleaners: stmt list;
}

type ('e, 'a) eval = {
  cases: ('e, 'a) case Dnf.t;
  ctx : 'a ctx;
}


let case e ?(cleaners=[]) (flow: 'a flow) : ('e, 'a) eval =
  {
    cases = Dnf.singleton {
        eval_result = e;
        eval_tmap = Flow.get_token_map flow;
        eval_cleaners = cleaners
      };
    ctx = Flow.get_ctx flow;
  }


let singleton (e: 'e) ?(cleaners=[]) (flow: 'a flow) : ('e, 'a) eval =
  case (Some e) ~cleaners flow


let empty_singleton flow : ('e, 'a) eval  =
  case None flow

let iter (f: 'e -> 'a flow -> unit) (eval: ('e, 'a) eval) : unit =
  Dnf.iter (fun case ->
      match case.eval_result with
      | None -> ()
      | Some e -> f e (Flow.create eval.ctx case.eval_tmap)
    ) eval.cases


let map
    (f: 'e -> 'a flow -> 'e * 'a flow)
    (eval: ('e, 'a) eval)
  : ('e, 'a) eval =
  let cases, ctx =
    Dnf.map_fold (fun ctx case ->
        match case.eval_result with
        | None -> case, ctx
        | Some e ->
          let e', flow' = f e (Flow.create ctx case.eval_tmap) in
          let case' = {
            eval_result = Some e';
            eval_tmap = Flow.get_token_map flow';
            eval_cleaners = []
          }
          in
          case', Flow.get_ctx flow'
      ) eval.ctx eval.cases 
  in
  { cases; ctx }


let apply
    (f: 'e -> 'a flow -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (empty: 'b)
    (evl: ('e,'a) eval)
  : 'b =
  Dnf.apply
    (fun case ->
       match case.eval_result with
       | Some e -> f e (Flow.create evl.ctx case.eval_tmap)
       | None -> empty
    )
    join meet evl.cases


let fold_apply
    (f:'b -> 'e option -> 'a flow -> stmt list -> 'b * 'c)
    (join:'c -> 'c -> 'c)
    (meet:'c -> 'c -> 'c)
    (init:'b)
    (evl:('e,'a) eval)
  : 'b * 'c
  =
  Dnf.fold_apply
    (fun acc case -> f acc case.eval_result (Flow.create evl.ctx case.eval_tmap) case.eval_cleaners)
    join meet init
    evl.cases

let map_flow
    (f: 'a flow -> 'a flow)
    (eval: ('e, 'a) eval)
  : ('e, 'a) eval =
  let cases, ctx = Dnf.map_fold (fun ctx case ->
      let flow' = f (Flow.create ctx case.eval_tmap) in
      let cases' = { case with eval_tmap = Flow.get_token_map flow' } in
      cases', Flow.get_ctx flow'
    ) eval.ctx eval.cases
  in
  { cases; ctx }


let set_ctx ctx evl =
  { evl with ctx }

let get_ctx evl =
  evl.ctx

let copy_ctx src dst =
  set_ctx (get_ctx src) dst


let join (eval1: ('e, 'a) eval) (eval2: ('e, 'a) eval) : ('e, 'a) eval =
  {
    cases = Dnf.mk_or eval1.cases eval2.cases;
    ctx = Context.get_most_recent eval1.ctx eval2.ctx
  }


let join_list ~empty (l: ('e, 'a) eval list) : ('e, 'a) eval =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left join hd tl


let meet (eval1: ('e, 'a) eval) (eval2: ('e, 'a) eval) : ('e, 'a) eval =
  {
    cases = Dnf.mk_and eval1.cases eval2.cases;
    ctx = Context.get_most_recent eval1.ctx eval2.ctx
  }


let meet_list ~empty (l: ('e, 'a) eval list) : ('e, 'a) eval =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left meet hd tl


let print ~(pp: Format.formatter -> 'e -> unit) fmt (evl: ('e, 'a) eval) : unit =
  Dnf.print (fun fmt case ->
      Format.fprintf fmt "%a"
        (fun fmt e -> match e with
         | None -> Format.pp_print_string fmt "ϵ"
         | Some x -> pp fmt x
        )
        case.eval_result
    )
    fmt evl.cases


let add_cleaners (cleaners: stmt list) (eval: ('e, 'a) eval ) : ('e, 'a) eval  =
  {
    eval with
    cases = Dnf.map (fun case ->
        { case with eval_cleaners = case.eval_cleaners @ cleaners }
      ) eval.cases
  }


let bind_opt f eval =
  let ctx, ret = Dnf.fold_apply
      (fun ctx case ->
         let flow' = Flow.create ctx case.eval_tmap in
         let eval' =
           match case.eval_result with
           | None -> Some (empty_singleton flow' |> (add_cleaners case.eval_cleaners))
           | Some expr -> f expr flow' |>
                          Option.lift (add_cleaners case.eval_cleaners)
         in
         let ctx = Option.apply ctx get_ctx eval' in
         (ctx,eval')
      )
      (Option.neutral2 join)
      (Option.neutral2 meet)
      (get_ctx eval) eval.cases
  in
  Option.lift (set_ctx ctx) ret


let bind
    (f: 'e -> 'a flow -> ('f, 'a) eval)
    (evl: ('e, 'a) eval)
  : ('f, 'a) eval =
  bind_opt (fun e flow -> Some (f e flow)) evl |>
  Option.none_to_exn



let bind_lowlevel_opt
    (f: 'e option -> 'a flow -> stmt list -> ('f, 'a) eval option )
    (eval: ('e, 'a) eval) : ('f, 'a) eval option =
  let ctx, eval = Dnf.fold_apply
      (fun ctx case ->
         let flow' = Flow.create ctx case.eval_tmap in
         let eval' = f case.eval_result flow' case.eval_cleaners in
         let ctx = Option.apply ctx get_ctx eval' in
         (ctx,eval')
      )
      (Option.neutral2 join)
      (Option.neutral2 meet)
      (get_ctx eval) eval.cases
  in
  Option.lift (set_ctx ctx) eval



let bind_lowlevel f evl =
  bind_lowlevel_opt (fun e flow cleaners -> Some (f e flow cleaners)) evl |>
  Option.none_to_exn


let eval_list_opt feval l flow =
  let rec aux l flow =
    match l with
    | e :: tl ->
      feval e flow |>
      Option.absorb @@
      bind_opt (fun e' flow ->
          aux tl flow |>
          Option.lift @@
          bind (fun tl flow ->
              singleton (e'::tl) flow
            )
        )

    | [] ->
      singleton [] flow |>
      Option.return
  in
  aux l flow


let eval_list feval l flow =
  eval_list_opt (fun e flow -> Some (feval e flow)) l flow |>
  Option.none_to_exn


let bind_some f evl =
  Some (bind f evl)


let to_dnf (evl: ('e, 'a) eval) : ('e option * 'a flow * stmt list) Dnf.t =
  Dnf.map (fun case ->
      case.eval_result, Flow.create evl.ctx case.eval_tmap, case.eval_cleaners
    ) evl.cases


let to_dnf_with_cleaners (evl: ('e, 'a) eval) : ('e option * 'a flow * stmt list) Dnf.t =
  Dnf.map (fun case ->
      case.eval_result, Flow.create evl.ctx case.eval_tmap, case.eval_cleaners
    ) evl.cases



let merge f (evl1:('e,'a) eval) (evl2:('f,'a) eval) =
  let ctx = Context.get_most_recent evl1.ctx evl2.ctx in

  let add_cases_cleaners block cases = 
    Dnf.map (fun case ->
        { case with eval_cleaners = case.eval_cleaners @ block }
      ) cases
  in
  
  let cases1, cases2, ctx = Dnf.merge_fold (fun ctx case1 case2 ->
      match case1.eval_result, case2.eval_result with
      | None, _ | _, None ->
        Some (Dnf.singleton case1), Some (Dnf.singleton case2), ctx

      | Some e1, Some e2 ->
        match f e1 (Flow.create ctx case1.eval_tmap) e2 (Flow.create ctx case2.eval_tmap) with
        | Some r1, Some r2 ->
          Some (add_cases_cleaners case1.eval_cleaners r1.cases),
          Some (add_cases_cleaners case2.eval_cleaners r2.cases),
          Context.get_most_recent r1.ctx r2.ctx


        | Some r1, None ->
          Some (add_cases_cleaners case1.eval_cleaners r1.cases),
          None,
          r1.ctx

        | None, Some r2 ->
          None,
          Some (add_cases_cleaners case2.eval_cleaners r2.cases),
          r2.ctx


        | None, None ->
          None, None, ctx
    ) ctx evl1.cases evl2.cases
  in

  Option.lift (fun cases -> { cases; ctx }) cases1,
  Option.lift (fun cases -> { cases; ctx }) cases2


let rec simplify lattice compare evl =

  let compare_case c1 c2 =
    Compare.option compare c1.eval_result c2.eval_result
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
              let case'' = {
                eval_result = case.eval_result;
                eval_tmap = TokenMap.meet lattice (Context.get_unit evl.ctx) case.eval_tmap case'.eval_tmap;
                eval_cleaners = case.eval_cleaners @ case'.eval_cleaners
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
          eval_result = case.eval_result;
          eval_tmap = TokenMap.join lattice (Context.get_unit evl.ctx) case.eval_tmap case'.eval_tmap;
          eval_cleaners = case.eval_cleaners @ case'.eval_cleaners;
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
  { evl with cases = Dnf.from_list (simplify_disj (Dnf.to_list evl.cases)) }
