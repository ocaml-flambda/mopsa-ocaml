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

  let copy_and_merge man ctx ax mergers out1 out2 =
    let out = man.flow.merge (fun tk env1 env2 ->
        match env1, env2 with
        | None, _ | _, None -> None
        | Some env1, Some env2 -> Some (ax.set (ax.get env1) env2)
      ) out1 out2
    in
    mergers |> List.fold_left (fun out merger ->
        man.exec ctx merger out
      ) out

  let copy_and_merge_list (man: ('a, t) manager) ctx ax (outl1: 'a eval_merge_case list) (outl2: 'a eval_merge_case list) : 'a eval_merge_case list =
    List.fold_left (fun acc (exp1, out1, cleaners) ->
        match exp1 with
        | None -> acc
        | Some (exp1, mergers1) ->
          begin
            List.map (fun (exp2, out2, cleaners) ->
                match exp2 with
                | None -> None, out2, cleaners
                | Some (exp2, mergers2) ->
                  Some (exp2, mergers2), (copy_and_merge man ctx ax mergers1 out1 out2), cleaners
              ) acc
          end
      ) outl2 outl1

  let merge_exec_out man hman tman ctx hout tout =
    let rec doit hout tout =
      match hout, tout with
      | None, x | x, None -> x
      | Some hout, Some tout ->
        let tout' = copy_and_merge man ctx hman.ax hout.mergers hout.out tout.out in
        let hout' = copy_and_merge man ctx tman.ax tout.mergers tout.out hout.out in
        let out = man.flow.meet hout' tout' in
        let publish = hout.publish @ tout.publish in
        let subscribe = (fun channel flow ->
            doit (hout.subscribe channel flow) (tout.subscribe channel flow)
          )
        in
        let mergers = hout.mergers @ tout.mergers in
        Some {out; publish; subscribe; mergers}
    in
    doit hout tout

  let exec man ctx stmt flow =
    let hman = head_man man in
    let tman = tail_man man in
    let hout = Head.exec hman ctx stmt flow in
    let tout = Tail.exec tman ctx stmt flow in
    merge_exec_out man hman tman ctx hout tout

  let merge_eval_out man hman tman ctx (hevl: 'a eval_out option) (tevl: 'a eval_out option) =
    Eval.oeval_meet ~fand:(fun hconj tconj ->
        let tconj = copy_and_merge_list man ctx hman.ax hconj tconj
        and hconj = copy_and_merge_list man ctx hman.ax hconj tconj
        in
        hconj @ tconj
      )
      hevl tevl

  let eval man ctx exp flow =
    let hman = head_man man in
    let tman = tail_man man in
    let hevl = Head.eval hman ctx exp flow
    and tevl = Tail.eval tman ctx exp flow in
    merge_eval_out man hman tman ctx hevl tevl

  let ask man ctx query flow =
    let head_reply = Head.ask (head_man man) ctx query flow in
    let tail_reply = Tail.ask (tail_man man) ctx query flow in
    Query.join query head_reply tail_reply

end
