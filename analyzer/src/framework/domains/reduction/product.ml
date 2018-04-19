(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduced product composer. *)

open Flow
open Manager
open Domain

module Make(Head: DOMAIN)(Tail: DOMAIN) =
struct

  type t = Head.t * Tail.t

  let bottom = Head.bottom, Tail.bottom

  let is_bottom (hd, tl) = Head.is_bottom hd || Tail.is_bottom tl

  let top = Head.top, Tail.top

  let is_top (hd, tl) = Head.is_top hd && Tail.is_top tl

  let leq (hd1, tl1) (hd2, tl2) = Head.leq hd1 hd2 && Tail.leq tl1 tl2

  let join (hd1, tl1) (hd2, tl2) = (Head.join hd1 hd2), (Tail.join tl1 tl2)

  let meet (hd1, tl1) (hd2, tl2) = (Head.meet hd1 hd2), (Tail.meet tl1 tl2)

  let widening ctx (hd1, tl1) (hd2, tl2) =
    (Head.widening ctx hd1 hd2), (Tail.widening ctx tl1 tl2)

  let print fmt (hd, tl) =
    Format.fprintf fmt "%a%a" Head.print hd Tail.print tl

  let head_man man = {
    man with ax = {
      get = (fun env -> fst @@ man.ax.get env);
      set = (fun hd env -> man.ax.set (hd, snd @@ man.ax.get env) env);
    }
  }

  let tail_man man = {
    man with ax = {
      get = (fun env -> snd @@ man.ax.get env);
      set = (fun tl env -> man.ax.set (fst @@ man.ax.get env, tl) env);
    }
  }

  let init man ctx prog flow =
    let ctx, flow = Head.init (head_man man) ctx prog flow in
    Tail.init (tail_man man) ctx prog flow

  let copy_and_merge_flow man ctx ax mergers flow1 flow2 =
    let flow = man.flow.merge (fun tk env1 env2 ->
        match env1, env2 with
        | None, _ | _, None -> None
        | Some env1, Some env2 -> Some (ax.set (ax.get env1) env2)
      ) flow1 flow2
    in
    mergers |> List.fold_left (fun flow merger ->
        man.exec ctx merger flow
      ) flow

  let merge_reval_cases (man: ('a, t) manager) ctx ax (cases1: 'a reval_case list) (cases2: 'a reval_case list) : 'a reval_case list =
    List.fold_left (fun acc (exp1, flow1, cleaners) ->
        match exp1 with
        | None -> acc
        | Some (exp1, mergers1) ->
          begin
            List.map (fun (exp2, flow2, cleaners) ->
                match exp2 with
                | None -> None, flow2, cleaners
                | Some (exp2, mergers2) ->
                  Some (exp2, mergers2), (copy_and_merge_flow man ctx ax mergers1 flow1 flow2), cleaners
              ) acc
          end
      ) cases2 cases1

  let merge_rflow man hman tman ctx rflow1 rflow2 =
    let rec doit rflow1 rflow2 =
      match rflow1, rflow2 with
      | None, x | x, None -> x
      | Some rflow1, Some rflow2 ->
        let flow2' = copy_and_merge_flow man ctx hman.ax rflow1.mergers rflow1.out rflow2.out in
        let flow1' = copy_and_merge_flow man ctx tman.ax rflow2.mergers rflow2.out rflow1.out in
        let flow = man.flow.meet flow2' flow1' in
        let publish = rflow1.publish @ rflow2.publish in
        let subscribe = (fun channel flow ->
            doit (rflow1.subscribe channel flow) (rflow2.subscribe channel flow)
          )
        in
        let mergers = rflow1.mergers @ rflow2.mergers in
        Some {out = flow; publish; subscribe; mergers}
    in
    doit rflow1 rflow2

  let exec man ctx stmt flow =
    let hman = head_man man in
    let tman = tail_man man in
    let hout = Head.exec hman ctx stmt flow in
    let tout = Tail.exec tman ctx stmt flow in
    merge_rflow man hman tman ctx hout tout

  let merge_revals man hman tman ctx (hevl: 'a revals option) (tevl: 'a revals option) =
    Eval.oeval_meet ~fand:(fun hconj tconj ->
        let tconj = merge_reval_cases man ctx hman.ax hconj tconj
        and hconj = merge_reval_cases man ctx hman.ax hconj tconj
        in
        hconj @ tconj
      )
      hevl tevl

  let eval man ctx exp flow =
    let hman = head_man man in
    let tman = tail_man man in
    let hevl = Head.eval hman ctx exp flow
    and tevl = Tail.eval tman ctx exp flow in
    merge_revals man hman tman ctx hevl tevl

  let ask man ctx query flow =
    let head_reply = Head.ask (head_man man) ctx query flow in
    let tail_reply = Tail.ask (tail_man man) ctx query flow in
    Query.join query head_reply tail_reply

end
