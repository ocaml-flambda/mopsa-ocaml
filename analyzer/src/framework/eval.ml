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

let return evl = Some evl

let case result ?(cleaners = []) flow = [{ result; flow; cleaners }]

let singleton result ?(cleaners = []) flow = return (case result flow ~cleaners)

let empty flow = singleton None flow ~cleaners:[]

let join (evl1: ('e, 'a) eval option)  (evl2: ('e, 'a) eval option) : ('e, 'a) eval option =
  Option.option_lift2 (@) evl1 evl2

let map_
    (f: 'e -> 'a flow -> ('f, 'b) case)
    (evl: ('e, 'a) eval)
  : ('f, 'b) eval =
  List.map (fun case ->
      match case.result with
      | None -> {result = None; flow = case.flow; cleaners = []}
      | Some result ->
        let case' = f result case.flow in
        {case' with cleaners = case.cleaners @ case'.cleaners}
    ) evl

let map
    (f: 'e -> 'a flow -> ('f, 'b) case)
    (evl: ('e, 'a) eval option)
  : ('f, 'b) eval option =
  Option.option_lift1 (map_ f) evl

let add_cleaners_ (cleaners: Ast.stmt list) (evl: ('e, 'a) eval) : ('e, 'a) eval =
  map_ (fun e flow ->
      {result = Some e; flow; cleaners}
    ) evl

let add_cleaners (cleaners: Ast.stmt list) (evl: ('e, 'a) eval option) : ('e, 'a) eval option =
  Option.option_lift1 (add_cleaners_ cleaners) evl

let fold_
    (f: 'b -> ('e, 'a) case -> 'b)
    (init: 'b)
    (evl: ('e, 'a) eval)
  : 'b =
  List.fold_left f init evl

let fold
    (f: 'b -> ('e, 'a) case -> 'b)
    (init: 'b)
    (evl: ('e, 'a) eval option)
  : 'b option =
  Option.option_lift1 (fold_ f init) evl


let iter_
    (f: 'e -> 'a Flow.flow -> unit)
    (evl: ('e, 'a) eval)
  : unit =
  List.iter (fun case ->
      match case.result with
      | None -> ()
      | Some e -> f e case.flow
    ) evl

let iter
    (f: 'e -> 'a Flow.flow -> unit)
    (evl: ('e, 'a) eval option)
  : unit =
  Option.option_apply (iter_ f) (fun () -> ()) evl

let fold
    (f: 'b -> ('e, 'a) case -> 'b)
    (init: 'b)
    (evl: ('e, 'a) eval option)
  : 'b option =
  Option.option_lift1 (fold_ f init) evl


let bind_
    (f: 'e -> 'a flow -> ('f, 'a) eval)
    (evl: ('e, 'a) eval)
  : ('f, 'a) eval =
  fold_ (fun acc x ->
      let evl' =
        match x.result with
        | None -> case None x.flow
        | Some result -> f result x.flow |>
                         add_cleaners_ x.cleaners
      in
      evl' @ acc
    ) [] evl


let bind
    (f: 'e -> 'a flow -> ('f, 'a) eval option)
    (evl: ('e, 'a) eval)
  : ('f, 'a) eval option =
  fold_ (fun acc case ->
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
    (man: ('a, 't) manager) ?(zpath = Zone.path_top) ctx flow
  : ('e, 'a) eval =
  let rec aux el flow = function
    | [] -> case (Some (List.rev el)) flow

    | e :: tl ->
      man.eval ~zpath e ctx flow |>
      bind_ @@ fun e flow ->
      aux (e :: el) flow tl
  in
  aux [] flow el

let assume
    cond ?(zone = Zone.top)
    ~fthen ~felse
    man ctx flow
    ?(fboth = (fun flow1 flow2 -> join (fthen flow1) (felse flow2)))
    ?(fnone = (fun () -> empty flow))
    ()
  : ('e, 'a) eval option =
  let then_flow = man.exec ~zone (mk_assume cond cond.erange) ctx flow in
  let else_flow = man.exec ~zone (mk_assume (mk_not cond cond.erange) cond.erange) ctx flow in
  match Manager.is_cur_bottom man then_flow, Manager.is_cur_bottom man else_flow with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | false, false -> fboth then_flow else_flow
  | true, true -> fnone ()

let switch
    (cases : (((expr * bool) list) * ('a Flow.flow -> ('e, 'a) eval option)) list)
    ?(zone = Zone.top)
    man ctx flow
  : ('e, 'a) eval option =
  match cases with
  | (cond, t) :: q ->
    let one (cond : (expr * bool) list) t =
      List.fold_left (fun acc (x, b) ->
          let s =
            if b then (mk_assume x x.erange)
            else (mk_assume (mk_not x x.erange) x.erange)
          in
          man.exec ~zone s ctx acc
        ) flow cond
      |> t
    in
    List.fold_left (fun acc (cond, t) -> join (one cond t) acc) (one cond t) q
  | [] -> None
  

let print ~(pp: Format.formatter -> 'e -> unit) fmt (evl: ('e, 'a) eval) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;⋁@;")
    (fun fmt ev ->
       match ev.result with
       | None -> Format.pp_print_string fmt "ϵ"
       | Some x -> pp fmt x
    )
    fmt
    evl
