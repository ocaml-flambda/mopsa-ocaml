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

type ('e, 'a) case = ('e, 'a) Manager.eval_case
type ('e, 'a) t = ('e, 'a) eval option

let singleton (result: 'e option) ?(cleaners = []) (flow: 'a flow) : ('e, 'a) t = Some (singleton_eval result flow cleaners)

let empty flow = singleton None flow

let join (evl1: ('e, 'a) t)  (evl2: ('e, 'a) t) : ('e, 'a) t =
  Option.option_lift2 Manager.join_eval evl1 evl2

let add_cleaners (cleaners: Ast.stmt list) (evl: ('e, 'a) t) : ('e, 'a) t =
  Option.option_lift1 (Manager.add_cleaners cleaners) evl

let map
    (f: 'e -> 'a flow -> ('f, 'b) case)
    (evl: ('e, 'a) t)
  : ('f, 'b) t =
  Option.option_lift1 (
    Manager.map_eval (fun e flow cleaners ->
        let case = f e flow in
        {case with cleaners = cleaners @ case.cleaners}
      )
  ) evl

let fold
    (f: 'b -> ('e, 'a) case -> 'b)
    (init: 'b)
    (evl: ('e, 'a) t)
  : 'b option =
  Option.option_lift1 (fold_eval f init) evl


let bind
  (e: Ast.expr)
  (man: ('a, 't) Manager.manager) ?(zpath = Zone.path_top) ctx flow
  (f: Ast.expr -> 'a flow -> ('e, 'a) t)
  : ('e, 'a) t =
  let evl = man.eval ~zpath e ctx flow in
  fold_eval (fun acc case ->
      let evl' =
        match case.result with
        | None -> empty case.flow
        | Some result -> f result case.flow |>
                         add_cleaners case.cleaners
      in
      join evl' acc
    ) None evl

let bind_list
    (el: Ast.expr list)
    (man: ('a, 't) Manager.manager) ?(zpath = Zone.path_top) ctx flow
    (f: Ast.expr list -> 'a flow -> ('e, 'a) t)
  : ('e, 'a) t =
  let rec aux el clean flow = function
    | [] ->
      f (List.rev el) flow |>
      add_cleaners clean

    | e :: tl ->
      let evl = man.eval ~zpath e ctx flow in
      fold_eval
        (fun acc case ->
           match case.result with
           | None -> empty case.flow |> join acc
           | Some e' -> aux (e' :: el) (clean @ case.cleaners) case.flow tl |>
                        join acc
        ) None evl
  in
  aux [] [] flow el
