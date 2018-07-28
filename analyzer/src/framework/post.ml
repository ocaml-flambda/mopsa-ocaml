(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Post-conditions of exec transfer functions *)

open Ast
open Manager

type channel = Channel : 'r Query.query -> channel

type 'a post = {
  flow : 'a flow;
  mergers : Ast.stmt list;
  publish: channel list;
  subscribe : channel list;
}

let add_mergers mergers post =
  {post with mergers = post.mergers @ mergers}

let bottom = {
  flow = Flow.bottom;
  mergers = [];
  publish = [];
  subscribe = [];
}

let singleton flow = {
  flow;
  mergers = [];
  publish = [];
  subscribe = [];
}

let join (man: ('a, _) man) (post1: 'a post) (post2: 'a post) : 'a post =
  {
    flow     = Flow.join man post1.flow post2.flow;
    mergers  = post1.mergers @ post2.mergers;
    publish  = post1.publish @ post2.publish;
    subscribe  = post1.subscribe @ post2.subscribe;
  }

let meet (man: ('a, _) man) (post1: 'a post) (post2: 'a post) : 'a post =
  {
    flow     = Flow.meet man post1.flow post2.flow;
    mergers  = post1.mergers @ post2.mergers; (* FIXME *)
    publish  = post1.publish @ post2.publish; (* FIXME *)
    subscribe  = post1.subscribe @ post2.subscribe; (* FIXME *)
  }

let bind
    ?(zone = Zone.top) (man: ('a, _) man)
    (f: 'e -> 'a flow -> 'a post) (evl: ('a, 'e) evl)
  : 'a post =
  Eval.fold (fun acc case ->
      let annot = get_annot acc.flow in
      let flow' = set_annot annot case.flow in
      match case.expr with
      | None -> join man acc {flow = flow'; mergers = []; publish = []; subscribe = []}

      | Some e ->
        let post = f e flow' in
        let flow'' = List.fold_left (fun acc stmt ->
            man.exec ~zone stmt acc
          ) post.flow case.cleaners
        in
        let post' = {post with flow = flow''} in
        let annot' = get_annot flow'' in
        let acc' = {acc with flow = set_annot annot' acc.flow} in
        join man acc' post'
    ) (join man) (meet man) bottom evl


let assume
    cond ?(zone = Zone.top) man
    ~fthen ~felse
    ?(fboth = (fun flow1 flow2 -> (* FIXME: propagate annotations *) join man (fthen flow1) (felse flow2)))
    ?(fnone = (fun flow -> singleton flow))
    flow
  : 'a post  =
  let then_flow = man.exec ~zone (mk_assume cond cond.erange) flow in
  let else_flow = man.exec ~zone (mk_assume (mk_not cond cond.erange) cond.erange) flow in
  match man.is_bottom (Flow.get T_cur man then_flow), man.is_bottom (Flow.get T_cur man else_flow) with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | false, false -> fboth then_flow else_flow
  | true, true -> fnone (Flow.join man then_flow else_flow)

let switch
    (cases : (((expr * bool) list) * ('a Flow.flow -> 'a post )) list)
    ?(zone = Zone.top)
    man flow
  : 'a post  =
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
    List.fold_left (fun acc (cond, t) -> join man (one cond t) acc) (one cond t) q
