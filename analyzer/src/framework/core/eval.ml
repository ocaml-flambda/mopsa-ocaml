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

open Ast
open Flow
open Manager
open Zone


(*==========================================================================*)
(**                          {2 Evaluations}                                *)
(*==========================================================================*)


type ('a, 'e) eval_case = {
  eval_result : 'e option;
  eval_flow: 'a flow;
  eval_cleaners: stmt list;
}

type ('a, 'e) eval = ('a, 'e) eval_case Dnf.t




let empty = Dnf.mk_false

let case  (c: ('a, 'e) eval_case) : ('a, 'e) eval =
  Dnf.singleton c

let singleton (e: 'e) ?(cleaners=[]) (flow: 'a flow) : ('a, 'e) eval =
  Dnf.singleton {
    eval_result = Some e;
    eval_flow = flow;
    eval_cleaners = cleaners
  }

let empty_singleton flow : ('a, 'e) eval  =
  Dnf.singleton {
    eval_result = None;
    eval_flow = flow;
    eval_cleaners = []
  }

let iter_cases (f: ('a, 'e) eval_case -> unit) (eval: ('a, 'e) eval) : unit =
  Dnf.to_list eval |>
  List.flatten |>
  List.iter f

let iter (f: 'e -> 'a flow -> unit) (eval: ('a, 'e) eval) : unit =
  iter_cases (fun case ->
      match case.eval_result with
      | None -> ()
      | Some e -> f e case.eval_flow
    ) eval

let map
    (f: 'e -> 'a flow -> 'f * 'a flow)
    (eval: ('a, 'e) eval)
  : ('a, 'f) eval =
  Dnf.map (fun case ->
      match case.eval_result with
      | None -> case
      | Some e ->
        let e', flow' = f e case.eval_flow in
        { eval_result = Some e'; eval_flow = flow'; eval_cleaners = [] }
    ) eval

let map_flow
    (f: 'a flow -> 'a flow)
    (eval: ('a, 'e) eval)
  : ('a, 'e) eval =
  Dnf.map (fun case ->
      let flow' = f case.eval_flow in
      { case with eval_flow = flow' }
    ) eval


let unify_annot annot evl =
  map_flow (Flow.set_all_annot annot) evl

let join (eval1: ('a, 'e) eval) (eval2: ('a, 'e) eval) : ('a, 'e) eval =
  Dnf.mk_or eval1 eval2

let join_list ?(empty=empty) (l: ('a, 'e) eval list) : ('a, 'e) eval =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left join hd tl

let meet (eval1: ('a, 'e) eval) (eval2: ('a, 'e) eval) : ('a, 'e) eval =
  Dnf.mk_and eval1 eval2

let meet_list ?(empty=empty) (l: ('a, 'e) eval list) : ('a, 'e) eval =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left meet hd tl


let add_cleaners (cleaners: Ast.stmt list) (eval: ('e, 'a) eval ) : ('e, 'a) eval  =
  Dnf.map (fun case ->
      {case with eval_cleaners = case.eval_cleaners @ cleaners}
    ) eval

let flip (eval: ('e, 'a) eval) : ('e, 'a) eval =
  Dnf.mk_neg (fun x -> case x) eval

let fold
    (f: 'b -> ('a, 'e) eval_case -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (init: 'b)
    (evl: ('a, 'e) eval)
  : 'b =
  Dnf.fold f join meet init evl

let fold2
    (f: 'c -> ('a, 'e) eval_case -> 'b * 'c)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (init: 'c)
    (evl: ('a, 'e) eval)
  : 'b * 'c =
  Dnf.fold2 f join meet init evl

let substitute
    (f: 'e -> 'a flow -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (empty: 'b)
    (evl: ('a, 'e) eval)
  : 'b =
  Dnf.substitute
    (fun case ->
       match case.eval_result with
       | Some e -> f e case.eval_flow
       | None -> empty
    )
    join meet evl


(* [choose_annot eval] returns any annotation from evaluation flows
   of [eval].
   Should be applied only if [eval] has been correctly constructed
   by propagating annotations in a flow-insensitive manner. *)
let choose_annot eval =
  match Dnf.choose eval with
  | Some case -> get_all_annot case.eval_flow
  | None -> Annotation.empty

let bind
    (f: 'e -> 'a flow -> ('a, 'f) eval)
    (evl: ('a, 'e) eval)
  : ('a, 'f) eval =
  let eval, _ = Dnf.fold2
    (fun annot case ->
      let flow' = set_all_annot annot case.eval_flow in
      let eval' =
        match case.eval_result with
        | None -> empty_singleton flow'
        | Some expr -> f expr flow' |>
                       add_cleaners case.eval_cleaners
      in
      let annot = choose_annot eval' in
      (eval', annot)
    )
    join meet
    (choose_annot evl) evl
  in
  eval

let bind_return f eval = bind f eval |> OptionExt.return

let bind_opt f eval =
  let eval, _ = Dnf.fold2
      (fun annot case ->
         let flow' = set_all_annot annot case.eval_flow in
         let eval' =
           match case.eval_result with
           | None -> Some (empty_singleton flow')
           | Some expr -> f expr flow' |>
                          OptionExt.option_lift1 (add_cleaners case.eval_cleaners)
         in
         let annot = OptionExt.option_dfl1 annot choose_annot eval' in
         (eval', annot)
      )
      (OptionExt.option_neutral2 join)
      (OptionExt.option_neutral2 meet)
      (choose_annot eval) eval
  in
  eval


let print ~(pp: Format.formatter -> 'e -> unit) fmt (evl: ('a, 'e) eval) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;∨@;")
    (fun fmt conj ->
       Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;∧@;")
         (fun fmt case ->
            match case.eval_result with
            | None -> Format.pp_print_string fmt "ϵ"
            | Some x -> pp fmt x
         )
         fmt
         conj
    )
    fmt
    (Dnf.to_list evl)

let to_dnf (evl: ('a, 'e) eval) : ('a, 'e) eval_case Dnf.t =
  evl

let choose eval =
  match Dnf.choose eval with
  | Some case -> Some (case.eval_result, case.eval_flow)
  | None -> None
