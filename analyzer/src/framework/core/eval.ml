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
open Flow
open Zone


(*==========================================================================*)
(**                          {2 Evaluations}                                *)
(*==========================================================================*)


type ('e, 'a) case = {
  eval_result : 'e option;
  eval_flow: 'a flow;
  eval_cleaners: stmt list;
}

type ('e, 'a) eval = ('e, 'a) case Dnf.t

let empty = Dnf.mk_false


let case e ?(cleaners=[]) (flow: 'a flow) : ('e, 'a) eval =
  Dnf.singleton {
    eval_result = e;
    eval_flow = flow;
    eval_cleaners = cleaners
  }

let singleton (e: 'e) ?(cleaners=[]) (flow: 'a flow) : ('e, 'a) eval =
  case (Some e) ~cleaners flow

let empty_singleton flow : ('e, 'a) eval  =
  case None flow

let iter (f: 'e -> 'a flow -> unit) (eval: ('e, 'a) eval) : unit =
  Dnf.iter (fun case ->
      match case.eval_result with
      | None -> ()
      | Some e -> f e case.eval_flow
    ) eval

let map
    (f: 'e -> 'a flow -> 'e * 'a flow)
    (eval: ('e, 'a) eval)
  : ('e, 'a) eval =
  Dnf.map (fun case ->
      match case.eval_result with
      | None -> case
      | Some e ->
        let e', flow' = f e case.eval_flow in
        { eval_result = Some e'; eval_flow = flow'; eval_cleaners = [] }
    ) eval


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
       | Some e -> f e case.eval_flow
       | None -> empty
    )
    join meet evl

let fold_apply
    (f:'b -> 'e option -> 'a flow -> stmt list -> 'b * 'c)
    (join:'c -> 'c -> 'c)
    (meet:'c -> 'c -> 'c)
    (init:'b)
    (evl:('e,'a) eval)
  : 'b * 'c
  =
  Dnf.fold_apply
    (fun acc case -> f acc case.eval_result case.eval_flow case.eval_cleaners)
    join meet init
    evl

let map_flow
    (f: 'a flow -> 'a flow)
    (eval: ('e, 'a) eval)
  : ('e, 'a) eval =
  Dnf.map (fun case ->
      let flow' = f case.eval_flow in
      { case with eval_flow = flow' }
    ) eval


let set_ctx ctx evl =
  map_flow (Flow.set_ctx ctx) evl

let choose_ctx eval =
  match Dnf.choose eval with
  | Some case -> get_ctx case.eval_flow
  | None -> Context.empty

let copy_ctx src dst =
  set_ctx (choose_ctx src) dst

let normalize_ctx evl =
  let ctx =
    Dnf.fold
      (fun ctx case ->
         Context.get_most_recent ctx (Flow.get_ctx case.eval_flow)
      )
      Context.get_most_recent
      Context.get_most_recent
      (choose_ctx evl)
      evl
  in
  set_ctx ctx evl


let join (eval1: ('e, 'a) eval) (eval2: ('e, 'a) eval) : ('e, 'a) eval =
  Dnf.mk_or eval1 eval2 |>
  normalize_ctx

let join_list ?(empty=empty) (l: ('e, 'a) eval list) : ('e, 'a) eval =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left join hd tl

let meet (eval1: ('e, 'a) eval) (eval2: ('e, 'a) eval) : ('e, 'a) eval =
  Dnf.mk_and eval1 eval2 |>
  normalize_ctx

let meet_list ?(empty=empty) (l: ('e, 'a) eval list) : ('e, 'a) eval =
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
    fmt evl


let add_cleaners (cleaners: stmt list) (eval: ('e, 'a) eval ) : ('e, 'a) eval  =
  Dnf.map (fun case ->
      {case with eval_cleaners = case.eval_cleaners @ cleaners}
    ) eval

let bind_opt f eval =
  let ctx, eval = Dnf.fold_apply
      (fun ctx case ->
         let flow' = Flow.set_ctx ctx case.eval_flow in
         let eval' =
           match case.eval_result with
           | None -> Some (empty_singleton flow' |> (add_cleaners case.eval_cleaners))
           | Some expr -> f expr flow' |>
                          Option.lift (add_cleaners case.eval_cleaners)
         in
         let ctx = Option.apply ctx choose_ctx eval' in
         (ctx,eval')
      )
      (Option.neutral2 join)
      (Option.neutral2 meet)
      (choose_ctx eval) eval
  in
  Option.lift (set_ctx ctx) eval

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
         let flow' = Flow.set_ctx ctx case.eval_flow in
         let eval' = f case.eval_result flow' case.eval_cleaners in
         let ctx = Option.apply ctx choose_ctx eval' in
         (ctx,eval')
      )
      (Option.neutral2 join)
      (Option.neutral2 meet)
      (choose_ctx eval) eval
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
      case.eval_result, case.eval_flow, case.eval_cleaners
    ) evl

let to_dnf_with_cleaners (evl: ('e, 'a) eval) : ('e option * 'a flow * stmt list) Dnf.t =
  Dnf.map (fun case -> case.eval_result, case.eval_flow, case.eval_cleaners) evl

let choose eval =
  match Dnf.choose eval with
  | Some case -> Some (case.eval_result, case.eval_flow)
  | None -> None

let merge f (evl1:('e,'a) eval) (evl2:('f,'b) eval) =
  Dnf.merge (fun case1 case2 ->
      match case1.eval_result, case2.eval_result with
      | Some e1, Some e2 ->
        begin match f e1 case1.eval_flow e2 case2.eval_flow with
          | Some r1, Some r2 ->
            Some (add_cleaners case1.eval_cleaners r1),
            Some (add_cleaners case2.eval_cleaners r2)

          | Some r1, None ->
            Some (add_cleaners case1.eval_cleaners r1),
            None

          | None, Some r2 ->
            None,
            Some (add_cleaners case2.eval_cleaners r2)

          | None, None ->
            None, None

        end
      | _ -> Some (Dnf.singleton case1), Some (Dnf.singleton case2)
    ) evl1 evl2


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
                eval_flow = Flow.meet lattice case.eval_flow case'.eval_flow;
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
          eval_flow = Flow.join lattice case.eval_flow case'.eval_flow;
          eval_cleaners = case.eval_cleaners @ case'.eval_cleaners;
        }
      )
  in

  match evl with
  | [] -> evl
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
    conj' :: simplify lattice compare tl'
