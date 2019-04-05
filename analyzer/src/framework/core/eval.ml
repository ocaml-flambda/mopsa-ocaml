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

let singleton (e: 'e) ?(cleaners=[]) (flow: 'a flow) : ('e, 'a) eval =
  Dnf.singleton {
    eval_result = Some e;
    eval_flow = flow;
    eval_cleaners = cleaners
  }

let empty_singleton flow : ('e, 'a) eval  =
  Dnf.singleton {
    eval_result = None;
    eval_flow = flow;
    eval_cleaners = []
  }

let iter_all (f: 'e option -> 'a flow -> unit) (eval: ('e, 'a) eval) : unit =
  Dnf.to_list eval |>
  List.flatten |>
  List.iter (fun case -> f case.eval_result case.eval_flow)

let iter (f: 'e -> 'a flow -> unit) (eval: ('e, 'a) eval) : unit =
  iter_all (fun res flow ->
      match res with
      | None -> ()
      | Some e -> f e flow
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

let choose_ctx eval =
  match Dnf.choose eval with
  | Some case -> get_ctx case.eval_flow
  | None -> Context.empty

let set_ctx ctx evl =
  map_flow (Flow.set_ctx ctx) evl

let copy_ctx src dst =
  set_ctx (choose_ctx src) dst

let join (eval1: ('e, 'a) eval) (eval2: ('e, 'a) eval) : ('e, 'a) eval =
  Dnf.mk_or eval1 eval2

let join_list ?(empty=empty) (l: ('e, 'a) eval list) : ('e, 'a) eval =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left join hd tl

let meet (eval1: ('e, 'a) eval) (eval2: ('e, 'a) eval) : ('e, 'a) eval =
  Dnf.mk_and eval1 eval2

let meet_list ?(empty=empty) (l: ('e, 'a) eval list) : ('e, 'a) eval =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left meet hd tl

let print ~(pp: Format.formatter -> 'e -> unit) fmt (evl: ('e, 'a) eval) : unit =
  Dnf.print (fun fmt case ->
      match case.eval_result with
      | None -> Format.pp_print_string fmt "ϵ"
      | Some x -> pp fmt x
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
           | None -> Some (empty_singleton flow')
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

let to_dnf (evl: ('e, 'a) eval) : ('e option * 'a flow) Dnf.t =
  Dnf.map (fun case -> case.eval_result, case.eval_flow) evl

let to_dnf_with_cleaners (evl: ('e, 'a) eval) : ('e option * 'a flow * stmt list) Dnf.t =
  Dnf.map (fun case -> case.eval_result, case.eval_flow, case.eval_cleaners) evl

let choose eval =
  match Dnf.choose eval with
  | Some case -> Some (case.eval_result, case.eval_flow)
  | None -> None
