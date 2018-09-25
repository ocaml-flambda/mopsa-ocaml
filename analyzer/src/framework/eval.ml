(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Evaluations of expressions *)

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

let join_list (l: ('a, 'e) evl list) : ('a, 'e) evl =
  match l with
  | [] -> []
  | hd :: tl -> List.fold_left join hd tl

let meet (evl1: ('a, 'e) evl) (evl2: ('a, 'e) evl) : ('a, 'e) evl =
  Dnf.mk_and evl1 evl2

let meet_list (l: ('a, 'e) evl list) : ('a, 'e) evl =
  match l with
  | [] -> assert false
  | hd :: tl -> List.fold_left meet hd tl

let iter (f: ('a, 'e) evl_case -> unit) (evl: ('a, 'e) evl) : unit =
  Dnf.to_list evl |>
  List.flatten |>
  List.iter f

let map
    (f: ('a, 'e) evl_case -> ('a, 'f) evl_case)
    (evl: ('a, 'e) evl)
  : ('a, 'f) evl =
  Dnf.map f evl

let add_cleaners (cleaners: Ast.stmt list) (evl: ('e, 'a) evl ) : ('e, 'a) evl  =
  map (fun case ->
      {case with cleaners = case.cleaners @ cleaners}
    ) evl

let fold
    (f: 'b -> ('a, 'e) evl_case -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (init: 'b)
    (evl: ('a, 'e) evl)
  : 'b =
  Dnf.fold f join meet init evl


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

let bind_opt f evl =
  let evl, _ = Dnf.fold2
      (fun annot case ->
         let flow' = set_all_annot annot case.flow in
         let evl' =
           match case.expr with
           | None -> Some (empty_singleton flow')
           | Some expr -> f expr flow' |>
                          Option.option_lift1 (add_cleaners case.cleaners)
         in
         let annot = Option.option_dfl1 annot choose_annot evl' in
         (evl', annot)
      )
      (Option.option_neutral2 join)
      (Option.option_neutral2 meet)
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
