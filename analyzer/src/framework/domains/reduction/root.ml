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
    let rflow = Domain.exec man ctx stmt flow in
    match rflow with
    | None -> None
    | Some rflow ->
      let rec doit i rflow =
        debug "reduction iteration %d" i;
        if i = Options.(common_options.reduce_iter) then rflow.out
        else
          let rec doit2 unused = function
            | [] ->
              debug "no publishers";
              None
              
            | channel :: tl ->
              debug "trying publisher";
              match Domain.refine man ctx channel rflow.out with
              | None ->
                debug "no reply";
                doit2 (channel :: unused) tl
              | Some rflow' ->
                debug "one reply";
                Some {rflow' with publish = rflow'.publish @ tl @ unused}
          in
          match doit2 [] rflow.publish with
          | None -> debug "really no reply"; rflow.out
          | Some rflow' -> doit (i + 1) rflow'
      in
      doit 0 rflow |>
      Stateful.return

  let eval man ctx exp flow =
    let revl = Domain.eval man ctx exp flow in
    Eval.oeval_map (function
        | None, flow, cleaners -> None, flow, cleaners
        | Some (exp, mergers), flow, cleaners -> Some exp, flow, cleaners
      ) revl

  let ask = Domain.ask

end
