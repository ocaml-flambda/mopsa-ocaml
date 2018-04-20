(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Root domain of a reduced product. *)

open Flow
open Manager
open Domain

let debug fmt = Debug.debug ~channel:"framework.domains.reduction.root" fmt

module Make(Domain: Domain.DOMAIN) =
struct

  type t = Domain.t

  let bottom = Domain.bottom
  let is_bottom = Domain.is_bottom
  let top = Domain.top
  let is_top = Domain.is_top
  let leq = Domain.leq
  let join = Domain.join
  let meet = Domain.meet
  let widening = Domain.widening
  let print = Domain.print

  let init = Domain.init

  let exec man ctx stmt flow =
    debug "exec %a" Pp.pp_stmt stmt;
    let rflow = Domain.exec man ctx stmt flow in
    match rflow with
    | None -> None
    | Some rflow ->
      let rec doit i post publish =
        debug "reduction iteration %d, |publish| = %d" i (List.length publish);
        if i = Options.(common_options.reduce_iter) then post
        else
          match publish with
          | [] -> post
          | _ ->
            let post', publish', nb_replies =
              List.fold_left (fun (post, publish, nb_replies) channel ->
                  match Domain.refine man ctx channel rflow.out with
                  | None ->
                    debug "no reply";
                    post, channel :: publish, nb_replies
                  | Some rflow' ->
                  debug "one reply";
                  rflow'.out, publish @ rflow'.publish, nb_replies + 1
                ) (post, [], 0) publish
            in
            if nb_replies = 0 then post
            else
              doit (i + 1) post' publish'
      in
      doit 0 rflow.out rflow.publish |>
      Stateful.return

  let eval man ctx exp flow =
    let revl = Domain.eval man ctx exp flow in
    Eval.oeval_map (function
        | None, flow, cleaners -> None, flow, cleaners
        | Some (exp, mergers), flow, cleaners -> Some exp, flow, cleaners
      ) revl

  let ask = Domain.ask

end
