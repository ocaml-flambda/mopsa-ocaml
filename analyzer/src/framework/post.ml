(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Post-conditions of a domain's [exec] transfer function *)

open Ast
open Manager
open Zone

type channel = ..

type 'a post = {
  flow : 'a flow;
  mergers : (stmt * zone) list;
  channels : channel list;
}

let add_mergers mergers ?(zone = any_zone) post =
  {post with mergers = post.mergers @ (List.map (fun s -> (s, zone)) mergers)}

let add_merger merger ?(zone = any_zone) post =
  {post with mergers = post.mergers @ [(merger, zone)]}

let add_channels channels post =
  {post with channels = post.channels @ channels}

let bottom annot = {
  flow = Flow.bottom annot;
  mergers = [];
  channels = [];
}

let of_flow flow = {
  flow;
  mergers = [];
  channels = [];
}

let return flow =
  Some (of_flow flow)

let map_flow f p =
  {p with flow = f p.flow}

let join (man: ('a, _) man) (post1: 'a post) (post2: 'a post) : 'a post =
  {
    flow     = Flow.join man post1.flow post2.flow;
    mergers  = post1.mergers @ post2.mergers;
    channels  = post1.channels @ post2.channels;
  }

let meet (man: ('a, _) man) (post1: 'a post) (post2: 'a post) : 'a post =
  {
    flow     = Flow.meet man post1.flow post2.flow;
    mergers  = post1.mergers @ post2.mergers; (* FIXME *)
    channels  = post1.channels @ post2.channels; (* FIXME *)
  }

let bind
    ?(zone = any_zone) (man: ('a, _) man)
    (f: 'e -> 'a flow -> 'a post)
    (evl: ('a, 'e) evl)
  : 'a post =
  let annot = Eval.choose evl |>
              OptionExt.option_dfl1 Annotation.empty (fun (_, flow) -> Flow.get_all_annot flow)
  in
  Eval.fold (fun acc case ->
      let annot = Flow.get_all_annot acc.flow in
      let flow' = Flow.set_all_annot annot case.flow in
      match case.expr with
      | None -> join man acc (of_flow flow')

      | Some e ->
        let post = f e flow' in
        let flow'' = List.fold_left (fun acc stmt ->
            man.exec ~zone stmt acc
          ) post.flow case.cleaners
        in
        let post' = {post with flow = flow''} in
        let annot' = Flow.get_all_annot flow'' in
        let acc' = {acc with flow = Flow.set_all_annot annot' acc.flow} in
        join man acc' post'
    ) (join man) (meet man) (bottom annot) evl


let bind_flow
    ?(zone = any_zone) (man: ('a, 't) man)
    (f: 'e -> 'a flow -> 'a flow)
    (evl: ('a, 'e) evl)
  : 'a flow =
  let annot = Eval.choose evl |>
              OptionExt.option_dfl1 Annotation.empty (fun (_, flow) -> Flow.get_all_annot flow)
  in
  Eval.fold (fun acc case ->
      let annot = Flow.get_all_annot acc in
      let flow' = Flow.set_all_annot annot case.flow in
      match case.expr with
      | None -> Flow.join man acc flow'

      | Some e ->
        let flow'' = f e flow' in
        let flow3 = List.fold_left (fun acc stmt ->
            man.exec ~zone stmt acc
          ) flow'' case.cleaners
        in
        let annot' = Flow.get_all_annot flow3 in
        let acc' = Flow.set_all_annot annot' acc in
        Flow.join man acc' flow3
    ) (Flow.join man) (Flow.meet man) (Flow.bottom annot) evl


let bind_opt
    ?(zone = any_zone) (man: ('a, _) man)
    (f: 'e -> 'a flow -> 'a post option)
    (evl: ('a, 'e) evl)
  : 'a post option =
  let annot = Eval.choose evl |>
              OptionExt.option_dfl1 Annotation.empty (fun (_, flow) -> Flow.get_all_annot flow)
  in
  let ojoin = OptionExt.option_neutral2 (join man) in
  Eval.fold (fun acc case ->
      let annot = match acc with None -> annot | Some acc -> Flow.get_all_annot acc.flow in
      let flow' = Flow.set_all_annot annot case.flow in
      match case.expr with
      | None -> ojoin
                  acc
                  (Some (of_flow flow'))

      | Some e ->
        match f e flow' with
        | None -> acc
        | Some post ->
          let flow'' = List.fold_left (fun acc stmt ->
              man.exec ~zone stmt acc
            ) post.flow case.cleaners
          in
          let post' = Some {post with flow = flow''} in
          let annot' = Flow.get_all_annot flow'' in
          let acc' =
            match acc with
            | None -> None
            | Some acc -> Some {acc with flow = Flow.set_all_annot annot' acc.flow}
          in
          ojoin acc' post'
    )
    (OptionExt.option_neutral2 (join man))
    (OptionExt.option_neutral2 (meet man))
    None
    evl



let assume
    cond ?(zone = any_zone) man
    ~fthen ~felse
    ?(fboth = (fun flow1 flow2 -> (* FIXME: propagate annotations *) join man (fthen flow1) (felse flow2)))
    ?(fnone = (fun flow -> of_flow flow))
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
    ?(zone = any_zone)
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

let print man fmt post = Flow.print man fmt post.flow
