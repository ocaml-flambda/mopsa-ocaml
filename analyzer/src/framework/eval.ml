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

let empty = Dnf.mk_false

let case  (c: ('a, 'e) evl_case) : ('a, 'e) evl =
  Dnf.singleton c

let singleton (e: 'e) ?(cleaners=[]) (flow: 'a flow) : ('a, 'e) evl =
  Dnf.singleton {expr = Some e; flow; cleaners}

let empty_singleton flow : ('a, 'e) evl  =
  Dnf.singleton {expr = None; flow; cleaners = []}

let join (evl1: ('a, 'e) evl) (evl2: ('a, 'e) evl) : ('a, 'e) evl =
  Dnf.mk_or evl1 evl2

let join_list ?(empty=empty) (l: ('a, 'e) evl list) : ('a, 'e) evl =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left join hd tl

let meet (evl1: ('a, 'e) evl) (evl2: ('a, 'e) evl) : ('a, 'e) evl =
  Dnf.mk_and evl1 evl2

let meet_list ?(empty=empty) (l: ('a, 'e) evl list) : ('a, 'e) evl =
  match l with
  | [] -> empty
  | hd :: tl -> List.fold_left meet hd tl

let iter_cases (f: ('a, 'e) evl_case -> unit) (evl: ('a, 'e) evl) : unit =
  Dnf.to_list evl |>
  List.flatten |>
  List.iter f

let iter (f: 'e -> 'a flow -> unit) (evl: ('a, 'e) evl) : unit =
  iter_cases (fun case ->
      match case.expr with
      | None -> ()
      | Some e -> f e case.flow
    ) evl

let map
    (f: 'e -> 'a flow -> 'f * 'a flow)
    (evl: ('a, 'e) evl)
  : ('a, 'f) evl =
  Dnf.map (fun case ->
      match case.expr with
      | None -> case
      | Some e ->
        let e', flow' = f e case.flow in
        { expr = Some e'; flow = flow'; cleaners = [] }
    ) evl

let map_flow
    (f: 'a flow -> 'a flow)
    (evl: ('a, 'e) evl)
  : ('a, 'e) evl =
  Dnf.map (fun case ->
      let flow' = f case.flow in
      { case with flow = flow' }
    ) evl

let add_cleaners (cleaners: Ast.stmt list) (evl: ('e, 'a) evl ) : ('e, 'a) evl  =
  Dnf.map (fun case ->
      {case with cleaners = case.cleaners @ cleaners}
    ) evl

let flip (evl: ('e, 'a) evl) : ('e, 'a) evl =
  Dnf.mk_neg (fun x -> case x) evl

let fold
    (f: 'b -> ('a, 'e) evl_case -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (init: 'b)
    (evl: ('a, 'e) evl)
  : 'b =
  Dnf.fold f join meet init evl

let fold2
    (f: 'c -> ('a, 'e) evl_case -> 'b * 'c)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (init: 'c)
    (evl: ('a, 'e) evl)
  : 'b * 'c =
  Dnf.fold2 f join meet init evl

let substitute
    (f: 'e -> 'a flow -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (empty: 'b)
    (evl: ('a, 'e) evl)
  : 'b =
  Dnf.substitute
    (fun case ->
       match case.expr with
       | Some e -> f e case.flow
       | None -> empty
    )
    join meet evl


(* [choose_annot evl] returns any annotation from evaluation flows
   of [evl].
   Should be applied only if [evl] has been correctly constructed
   by propagating annotations in a flow-insensitive manner. *)
let choose_annot evl =
  match Dnf.choose evl with
  | Some case -> get_all_annot case.flow
  | None -> Annotation.empty

let bind
    (f: 'e -> 'a flow -> ('a, 'f) evl)
    (evl: ('a, 'e) evl)
  : ('a, 'f) evl =
  let evl, _ = Dnf.fold2
    (fun annot case ->
      let flow' = set_all_annot annot case.flow in
      let evl' =
        match case.expr with
        | None -> empty_singleton flow'
        | Some expr -> f expr flow' |>
                       add_cleaners case.cleaners
      in
      let annot = choose_annot evl' in
      (evl', annot)
    )
    join meet
    (choose_annot evl) evl
  in
  evl

let bind_return f evl = bind f evl |> OptionExt.return

let bind_opt f evl =
  let evl, _ = Dnf.fold2
      (fun annot case ->
         let flow' = set_all_annot annot case.flow in
         let evl' =
           match case.expr with
           | None -> Some (empty_singleton flow')
           | Some expr -> f expr flow' |>
                          OptionExt.option_lift1 (add_cleaners case.cleaners)
         in
         let annot = OptionExt.option_dfl1 annot choose_annot evl' in
         (evl', annot)
      )
      (OptionExt.option_neutral2 join)
      (OptionExt.option_neutral2 meet)
      (choose_annot evl) evl
  in
  evl

let assume
    cond ?(zone = any_zone)
    ~fthen ~felse
    ?(fboth = (fun flow1 flow2 -> (* FIXME: propagate annotations *) join (fthen flow1) (felse flow2)))
    ?(fnone = (fun flow -> empty_singleton flow))
    man flow
  : ('a, 'e) evl  =
  let then_flow = man.exec ~zone (mk_assume cond cond.erange) flow in
  let else_flow = man.exec ~zone (mk_assume (mk_not cond cond.erange) cond.erange) flow in
  match man.is_bottom (Flow.get T_cur man then_flow), man.is_bottom (Flow.get T_cur man else_flow) with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | false, false -> fboth then_flow else_flow
  | true, true -> fnone (Flow.join man then_flow else_flow)

let switch
    (cases : (((expr * bool) list) * ('a Flow.flow -> ('a, 'e) evl )) list)
    ?(zone = any_zone)
    man flow
  : ('a, 'e) evl  =
  match cases with
  | [] -> assert false

  | (cond, t) :: q ->
    let one (cond : (expr * bool) list) t =
      List.fold_left (fun acc (x, b) ->
          let s =
            if b then (mk_assume x x.erange)
            else (mk_assume (mk_not x x.erange) x.erange)
          in
          man.exec ~zone s acc
        ) flow cond
      |> t
    in
    List.fold_left (fun acc (cond, t) -> join (one cond t) acc) (one cond t) q

let eval_list
    (l: 'e list)
    (eval: 'e -> 'c flow -> ('c, 'b) evl)
    (flow: 'c flow)
  : ('c, 'b list) evl =
  let rec aux expl flow clean = function
    | [] ->
      singleton (List.rev expl) flow ~cleaners:clean
    | exp :: tl ->
      eval exp flow |>
      Dnf.substitute2
        (fun case ->
           let exp' = case.expr in
           let flow = case.flow in
           let clean' = case.cleaners in
           match exp' with
           | Some exp' -> (aux (exp' :: expl) flow (clean @ clean') tl)
           | None -> empty_singleton flow
        )
  in
  aux [] flow [] l


let eval_list_opt
    (l: 'e list)
    (eval: 'e -> 'c flow -> ('c, 'b) evl option)
    (flow: 'c flow)
  : ('c, 'b list) evl option =
  let rec aux expl flow clean = function
    | [] ->
      singleton (List.rev expl) flow ~cleaners:clean
    | exp :: tl ->
      eval exp flow |>
      OptionExt.none_to_exn |>
      Dnf.substitute2
        (fun case ->
           let exp' = case.expr in
           let flow = case.flow in
           let clean' = case.cleaners in
           match exp' with
           | Some exp' -> (aux (exp' :: expl) flow (clean @ clean') tl)
           | None -> empty_singleton flow
        )
  in
  try Some (aux [] flow [] l)
  with OptionExt.Found_None -> None

let print ~(pp: Format.formatter -> 'e -> unit) fmt (evl: ('a, 'e) evl) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;∨@;")
    (fun fmt conj ->
       Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;∧@;")
         (fun fmt case ->
            match case.expr with
            | None -> Format.pp_print_string fmt "ϵ"
            | Some x -> pp fmt x
         )
         fmt
         conj
    )
    fmt
    (Dnf.to_list evl)

let to_dnf (evl: ('a, 'e) evl) : ('a, 'e) evl_case Dnf.t =
  evl

let choose evl =
  match Dnf.choose evl with
  | Some case -> Some (case.expr, case.flow)
  | None -> None

let return evl = Some evl
