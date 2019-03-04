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

let clean cleaners man flow =
  let exec_on_all_tokens stmt flow =
    Flow.fold (fun flow tk env ->
        (* Put env in T_cur token of flow and remove others *)
        let annot = Flow.get_all_annot flow in
        let flow' = Flow.singleton annot T_cur env in

        (* Execute the cleaner *)
        let flow'' = man.exec stmt flow' in

        (* Restore T_cur in tk *)
        Flow.copy T_cur tk man flow'' flow |>
        Flow.copy_annot flow''
      ) flow man flow
  in
  List.fold_left (fun flow stmt ->
      exec_on_all_tokens stmt flow
    ) flow cleaners

let bind
    ?(zone = any_zone) (man: ('a, _) man)
    (f: 'e -> 'a flow -> 'a post)
    (evl: ('a, 'e) evl)
  : 'a post =
  let annot = Eval.choose evl |>
              OptionExt.option_dfl1 Annotation.empty (fun (_, flow) -> Flow.get_all_annot flow)
  in
  let post, _ =
    Eval.fold2 (fun annot case ->
        let flow' = Flow.set_all_annot annot case.flow in
        match case.expr with
        | None -> of_flow flow', annot

        | Some e ->
          let post = f e flow' in
          let flow'' = clean case.cleaners man post.flow in
          let post' = {post with flow = flow''} in
          let annot'' = Flow.get_all_annot flow'' in
          post', annot''
      )
      (join man) (meet man)
      annot
      evl
  in
  post

let bind_with_cleaners
    ?(zone = any_zone) (man: ('a, _) man)
    (f: 'e -> Ast.stmt list -> 'a flow -> 'a post)
    (evl: ('a, 'e) evl)
  : 'a post =
  let annot = Eval.choose evl |>
              OptionExt.option_dfl1 Annotation.empty (fun (_, flow) -> Flow.get_all_annot flow)
  in
  let post, _ =
    Eval.fold2 (fun annot case ->
        let flow' = Flow.set_all_annot annot case.flow in
        match case.expr with
        | None -> of_flow flow', annot

        | Some e ->
           let post = f e case.cleaners flow' in
           post, Flow.get_all_annot post.flow
      )
      (join man) (meet man)
      annot
      evl
  in
  post

let bind_return ?(zone = any_zone) man f evl = bind ~zone man f evl |> OptionExt.return

let bind_flow
    ?(zone = any_zone) (man: ('a, 't) man)
    (f: 'e -> 'a flow -> 'a flow)
    (evl: ('a, 'e) evl)
  : 'a flow =
  let post =
    bind man ~zone
      (fun e flow ->
        let flow = f e flow in
        of_flow flow
      ) evl
  in
  post.flow

let bind_opt
    ?(zone = any_zone) (man: ('a, _) man)
    (f: 'e -> 'a flow -> 'a post option)
    (evl: ('a, 'e) evl)
  : 'a post option =
  let annot = Eval.choose evl |>
              OptionExt.option_dfl1 Annotation.empty (fun (_, flow) -> Flow.get_all_annot flow)
  in
  let post, _ =
    Eval.fold2 (fun annot case ->
        let flow' = Flow.set_all_annot annot case.flow in
        match case.expr with
        | None ->
          let post = of_flow flow' in
          Some post, annot

        | Some e ->
          match f e flow' with
          | None -> None, annot
          | Some post ->
            let flow'' = clean case.cleaners man post.flow in
            let post' = {post with flow = flow''} in
            let annot'' = Flow.get_all_annot flow'' in
            Some post', annot''
      )
      (OptionExt.option_neutral2 (join man))
      (OptionExt.option_neutral2 (meet man))
      annot
      evl
  in
  post


let assume
    cond ?(zone = any_zone) man
    ~fthen ~felse
    ?(fboth = (fun flow1 flow2 ->
        let fthen_r = fthen flow1 in
        let fthen_r_flow = fthen_r.flow in
        let flow2 = Flow.copy_annot fthen_r_flow flow2 in
        join man fthen_r (felse flow2)))
    ?(fnone = (fun flow -> of_flow flow))
    flow
  : 'a post  =
  let then_flow = man.exec ~zone (mk_assume cond cond.erange) flow in
  let flow = Flow.copy_annot then_flow flow in
  let else_flow = man.exec ~zone (mk_assume (mk_not cond cond.erange) cond.erange) flow in
  let then_flow = Flow.copy_annot else_flow then_flow in
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
