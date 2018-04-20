(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Root domain of a reduced product.

    The Root domain implements the iterative reduction algorithm which behaves
    as follows: after executing a statement in the reduced product,
    the Root node folds all published channels on the resulting post condition.

    If a channel is used by some domain, we set its output as the current post
    condition and we remove the used channel from the channel pool of
    the next iteration. Also, we add the new channels generated by the the reduction
    to the pool.

    Otherwise, if a channel is not used, we keep it in the pool of the next
    iteration. When we are done with all channels, we check whether no reduction was
    applied or if the new pool is empty. If one of these conditions is verified,
    we return the current post-condition, otherwise we begin a new iteration with
    the new post condition and the new pool of channels.

    Note that the maximum number of iterations is bounded by the command line 
    option `-reduce-iter`.

*)

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
      (* Reduction iterator *)
      let rec doit i post publish =
        debug "reduction iteration %d, |publish| = %d" i (List.length publish);
        (* Stop when we reach the maximal number of iterations *)
        if i = Options.(common_options.reduce_iter) then post
        else
          match publish with
          (* Stop also when wo channel is published *)
          | [] -> post

          (* Otherwise, fold over them *)
          | _ ->
            let post', publish', nb_replies =
              List.fold_left (fun (post, publish, nb_replies) channel ->
                  match Domain.refine man ctx channel post with
                  | None ->
                    (* Channel not used by any domain, so keep it for the next iteration *)
                    post, channel :: publish, nb_replies
                  | Some rflow' ->
                    (* At least one domain replied. We use the new post condition for the
                       remaining iterations and we enrich the channel pool. *)
                  rflow'.out, publish @ rflow'.publish, nb_replies + 1
                ) (post, [], 0) publish
            in
            (* Stop when no domain replied *)
            if nb_replies = 0 then post
            else doit (i + 1) post' publish'
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
